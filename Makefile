CC=gcc
CFLAGS= -W -Wextra -g -O2
PLOTICUS=ploticus

COMMON_SRCS=big_int_util.ml dissect.ml hole.ml hole_of_interest.ml \
	live_stub.ml results.ml shadow_stub.ml states.ml stub_base.ml \
	util.ml pages.ml monparnes_config.ml dissect_config.ml stats.ml

all: stub stub-pie monparnes.native monparnes_analyze.native dissect.native

monparnes.native: $(COMMON_SRCS) monparnes.ml
	corebuild -pkg core_extended -pkg str -pkg sexplib_num $@

monparnes_analyze.native: $(COMMON_SRCS) monparnes_analyze.ml
	corebuild -pkg core_extended -pkg str -pkg sexplib_num -pkg fileutils $@

dissect.native: $(COMMON_SRCS) dissect.ml
	corebuild -pkg core_extended -pkg str -pkg sexplib_num -pkg fileutils $@

stub: stub.c
	$(CC) $(CFLAGS) -o $@ $< -lreadline

stub-pie: stub.c
	$(CC) $(CFLAGS) -fpic -pie -o $@ $< -lreadline

clean:
	rm -f stub stub-pie
	rm -rf _build
