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
#include <fcntl.h>      // open
#include <stdio.h>      // perror, NULL
#include <stdlib.h>     // exit
#include <string.h>     // strerror
#include <sys/stat.h>   // open
#include <sys/types.h>  // open
#include <termios.h>    // tc*, cf*
#include <unistd.h>
#include <unistd.h>     // close, STD*_FILENO

#include <joo/matcher.h>
#include <joo/reader.h>
#include <joo/writer.h>

static int fd = -1;

static int
open_port (const char *port)
{
  int            errsv;

  struct termios options;

  if (fd >= 0) {
    // Already open.
    errno = EIO;
    return -1;
  }

  fd = open (port, O_RDWR|O_NOCTTY|O_NONBLOCK);
  if (fd < 0)
    return -1;
  fcntl (fd, F_SETFL, 0);

  if (tcgetattr (fd, &options) < 0)
    goto settings_error;

  cfsetispeed (&options, B2400);
  cfsetospeed (&options, B2400);

  options.c_cflag |= CLOCAL|CREAD;

  options.c_cflag &= ~CSIZE;   // 8
  options.c_cflag |= CS8;
  options.c_cflag &= ~PARENB;  // N
  options.c_cflag &= ~CSTOPB;  // 1

  options.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR |
                       ICRNL | IXON | IXOFF | IXANY);
  options.c_oflag &= ~OPOST;
  options.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG | IEXTEN);

  if (tcsetattr (fd, TCSANOW, &options) < 0)
    goto settings_error;

  return fd;

settings_error:
  errsv = errno;
  close (fd);
  errno = errsv;
  return -1;
}

static void
close_port (void)
{
  if (fd < 0)
    return;

  close (fd);
  fd = -1;
}

static int
write_buffer (const char *buf,
              ssize_t     size)
{
  ssize_t i;
  ssize_t written = 0;

  while (written < size) {
    if ((i = write (fd, buf+written, size-written)) < 0)
      return i;
    written += i;
  }

  return written;
}

static int
write_erl (void          *user_data,
           JooPushedData *pushed_data)
{
  (void)user_data;

  int            res;
  JooPushedData *binary = (JooPushedData *)pushed_data->entry.next;

  assert (binary != pushed_data);

  res = write_buffer (binary->data, binary->size);
  if (res < 0) {
    joo_write (STDOUT_FILENO, (JooTerm[]){
      joo_tuple_start (),
      joo_atom ("error"),
      joo_string (strerror (errno)),
      joo_tuple_end (),
      joo_end () });

    return -1;

  } else if (res != binary->size) {
    joo_write (STDOUT_FILENO, (JooTerm[]){
      joo_tuple_start (),
      joo_atom ("error"),
      joo_atom ("short_write"),
      joo_tuple_end (),
      joo_end () });

    return -1;
  }

  joo_write (STDOUT_FILENO, (JooTerm[]){ joo_atom ("ok"), joo_end () });

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

  joo_matcher_add (matcher, write_erl, NULL, (JooTerm[]){
    joo_tuple_start (),
    joo_atom ("write"),
    joo_push_binary (),
    joo_tuple_end (),
    joo_end () });

  joo_reader_init (STDIN_FILENO, matcher);
  joo_reader_main_loop ();

  joo_matcher_free (matcher);

  close_port ();

  return 0;
}

// vim:set et sw=2 sts=2:
