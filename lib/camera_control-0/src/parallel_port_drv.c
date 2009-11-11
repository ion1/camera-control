/* camera-control – Control Sanyo PTZ cameras with a custom input board
 *
 * Copyright © 2009 Johan Kiviniemi
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <assert.h>
#include <errno.h>
#include <fcntl.h>        // open
#include <linux/ppdev.h>
#include <stdint.h>
#include <stdio.h>        // perror, NULL
#include <stdlib.h>       // exit
#include <string.h>       // strerror
#include <sys/ioctl.h>
#include <sys/stat.h>     // open
#include <sys/types.h>    // open
#include <unistd.h>       // close, STD*_FILENO

#include <joo/matcher.h>
#include <joo/reader.h>
#include <joo/writer.h>

#define APPLY_MASK(stored_byte, byte, mask) \
  (((stored_byte) & ~(mask)) | ((byte) & (mask)))

static int fd = -1;

static uint8_t written_data = 0;
static uint8_t written_control = 0;

static int
open_port (const char *port)
{
  int errsv;

  if (fd >= 0) {
    // Already open.
    errno = EIO;
    return -1;
  }

  fd = open (port, O_RDWR|O_NOCTTY|O_NONBLOCK);
  if (fd < 0)
    return -1;
  fcntl (fd, F_SETFL, 0);

  if (ioctl (fd, PPEXCL) < 0) {
    errsv = errno;
    goto ioctl_failed;
  }

  if (ioctl (fd, PPCLAIM) < 0) {
    errsv = errno;
    goto ioctl_failed;
  }

  return 0;

ioctl_failed:
  close (fd);
  fd = -1;
  errno = errsv;
  return -1;
}

static void
close_port (void)
{
  if (fd < 0)
    return;

  ioctl (fd, PPRELEASE);
  close (fd);
  fd = -1;
}

static int
write_data (const uint8_t byte,
            const uint8_t mask)
{
  uint8_t val;

  int datadir = 0;

  if (fd < 0) {
    errno = EIO;
    return -1;
  }

  val = APPLY_MASK (written_data, byte, mask);

  if (ioctl (fd, PPDATADIR, &datadir) < 0)
    return -1;
  if (ioctl (fd, PPWDATA, &val) < 0)
    return -1;

  written_data = val;

  return 0;
}

static int
read_data (uint8_t *byte)
{
  int datadir = 1;

  if (fd < 0) {
    errno = EIO;
    return -1;
  }

  if (ioctl (fd, PPDATADIR, &datadir) < 0)
    return -1;
  if (ioctl (fd, PPRDATA, byte) < 0)
    return -1;

  return 0;
}

static int
write_control (const uint8_t byte,
               const uint8_t mask)
{
  uint8_t val;

  if (fd < 0) {
    errno = EIO;
    return -1;
  }

  val = APPLY_MASK (written_control, byte, mask);

  if (ioctl (fd, PPWCONTROL, &val) < 0)
    return -1;

  written_control = val;

  return 0;
}

static int
read_control (uint8_t *byte)
{
  if (fd < 0) {
    errno = EIO;
    return -1;
  }

  if (ioctl (fd, PPRCONTROL, byte) < 0)
    return -1;

  return 0;
}

static int
read_status (uint8_t *byte)
{
  if (fd < 0) {
    errno = EIO;
    return -1;
  }

  if (ioctl (fd, PPRSTATUS, byte) < 0)
    return -1;

  *byte ^= 0x80; // Invert −BUSY.

  return 0;
}


typedef int (*WriteFunc) (const uint8_t, const uint8_t);
typedef int (*ReadFunc) (uint8_t *);

static int
write_erl (void          *user_data,
           JooPushedData *pushed_data)
{
  WriteFunc      func = (WriteFunc)user_data;

  int            pos = 0;
  JooPushedData *binary = (JooPushedData *)pushed_data->entry.next;

  assert (binary != pushed_data);

  while (pos < binary->size) {
    uint8_t byte = binary->data[pos];
    uint8_t mask = 0xff;

    if (pos+1 < binary->size)
      mask = binary->data[pos+1];

    if (func (byte, mask) < 0) {
      joo_write (STDOUT_FILENO, (JooTerm[]){
        joo_tuple_start (),
        joo_atom ("error"),
        joo_string (strerror (errno)),
        joo_tuple_end (),
        joo_end () });

      return -1;
    }

    pos += 2;
  }

  joo_write (STDOUT_FILENO, (JooTerm[]){ joo_atom ("ok"), joo_end () });

  return 0;
}

static int
read_erl (void          *user_data,
          JooPushedData *pushed_data)
{
  (void)pushed_data;

  ReadFunc func = (ReadFunc)user_data;
  uint8_t  byte;

  if (func (&byte) < 0) {
    joo_write (STDOUT_FILENO, (JooTerm[]){
      joo_tuple_start (),
      joo_atom ("error"),
      joo_string (strerror (errno)),
      joo_tuple_end (),
      joo_end () });

    return -1;
  }

  joo_write (STDOUT_FILENO, (JooTerm[]){
    joo_tuple_start (),
    joo_atom ("ok"),
    joo_binary ((char *)&byte, 1),
    joo_tuple_end (),
    joo_end () });

  return 0;
}

static int
no_match (void *user_data)
{
  (void)user_data;

  joo_write (STDOUT_FILENO, (JooTerm[]){
    joo_tuple_start (),
    joo_atom ("error"),
    joo_atom ("no_match"),
    joo_tuple_end (),
    joo_end () });

  return -1;
}

int
main (int argc, char **argv)
{
  JooMatcher *matcher;

  assert (argc == 2);

  if (open_port (argv[1]) < 0) {
    perror ("open_port");
    exit (1);
  }

  assert (matcher = joo_matcher_new (no_match, NULL));

  joo_matcher_add (matcher, write_erl, write_data, (JooTerm[]){
    joo_tuple_start (),
    joo_atom ("write_data"),
    joo_push_binary (),
    joo_tuple_end (),
    joo_end () });

  joo_matcher_add (matcher, read_erl, read_data, (JooTerm[]){
    joo_atom ("read_data"),
    joo_end () });

  joo_matcher_add (matcher, write_erl, write_control, (JooTerm[]){
    joo_tuple_start (),
    joo_atom ("write_control"),
    joo_push_binary (),
    joo_tuple_end (),
    joo_end () });

  joo_matcher_add (matcher, read_erl, read_control, (JooTerm[]){
    joo_atom ("read_control"),
    joo_end () });

  joo_matcher_add (matcher, read_erl, read_status, (JooTerm[]){
    joo_atom ("read_status"),
    joo_end () });

  joo_reader_init (STDIN_FILENO, matcher);
  joo_reader_main_loop ();

  joo_matcher_free (matcher);

  close_port ();

  return 0;
}

// vim:set et sw=2 sts=2:
