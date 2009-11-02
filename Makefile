CFLAGS  := -g -Os -W -Wall -Werror -std=gnu99 -Ijoo
LDFLAGS := -Wl,--as-needed -Ljoo/joo
LDLIBS  := -ljoo

ERLC_FLAGS := -W +warn_unused_vars +warn_unused_import

cc_path := lib/camera_control-0

dirnoslash = $(patsubst %/,%,$(dir $1))
parent = $(call dirnoslash,$(call dirnoslash, $1))

e_pathmap = $(call parent,$1)/ebin/$(notdir $(1:.erl=.beam))

c_src := $(wildcard lib/*/src/*.c)
e_src := $(wildcard lib/*/src/*.erl)

c_obj := $(c_src:.c=.o)
e_obj := $(foreach s,$(e_src),$(call e_pathmap,$s))

all_deps := $(c_src:.c=.dep)

cc_drivers := $(cc_path)/priv/parallel_port_drv $(cc_path)/priv/serial_port_drv

release_path := releases/0
cc_rel       := $(release_path)/camera_control

all : $(cc_drivers) $(e_obj)

.PHONY : run
run : $(cc_rel).boot
	$(release_path)/run

.PHONY : bootscript
bootscript : $(cc_rel).boot

clean :=

$(cc_path)/priv/parallel_port_drv : $(cc_path)/src/parallel_port_drv.o
	mkdir -p "$$(dirname $@)"
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)
clean += $(cc_path)/priv/parallel_port_drv

$(cc_path)/priv/serial_port_drv : $(cc_path)/src/serial_port_drv.o
	mkdir -p "$$(dirname $@)"
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)
clean += $(cc_path)/priv/serial_port_drv

$(cc_path)/ebin/%.beam : $(cc_path)/src/%.erl
	erlc $(ERLC_FLAGS) -o $(dir $@) $<

$(cc_rel).boot $(cc_rel).script : $(e_obj) $(cc_drivers)
	$(release_path)/make_bootscript
clean += $(cc_rel).boot $(cc_rel).script

.PHONY : clean
clean ::
	$(RM) $(clean) $(c_obj) $(e_obj) $(all_deps)

-include $(all_deps)

%.dep : %.c
	$(CC) -MM -MQ $(<:.c=.o) $(CFLAGS) -o $@ $<

