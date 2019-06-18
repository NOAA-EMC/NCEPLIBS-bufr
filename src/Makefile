SHELL=/bin/sh

# sources, headers, objects, library and etc.
SRCS_Ff = $(patsubst %.F, %.f, $(wildcard *.F))
SRCS_f  = $(wildcard modv_*.f) $(wildcard moda_*.f) \
          $(filter-out mod[av]_*.f, $(wildcard *.f))
SRCS_C  = $(wildcard *.c)
DEPS    = $(wildcard *.h)
OBJS    = $(patsubst %.c, %.o, $(SRCS_C)) \
          $(patsubst %.f, %.o, $(SRCS_f))
OBJS_Ff = $(patsubst %.f, %.o, $(SRCS_Ff))
MODS    = $(wildcard *.mod)
BUFRLIB = bufrlib

all: build

build: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

debug: dbg_build

dbg_build: override CFLAGS += $(DEBUG)
dbg_build: override FFLAGS += $(DEBUG)
dbg_build: $(LIB)

preproc: $(SRCS_Ff)

$(BUFRLIB).prm: $(BUFRLIB).PRM
	$(CPP) $(CPPFLAGS) $(CPPDEFS) $< $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(CFLAGSDEFS) $<

%.o: %.f
	$(FC) -c $(FFLAGS) $(FFLAGSDEFS) $<

#%.f: %.F
#	$(CPP) $(CPPFLAGS) $(CPPDEFS) $< $@

%.o: %.F
	$(FC) -c $(CPPDEFS) $(FFLAGS) $(FFLAGSDEFS) $<

message: $(MSGSRC)
	$(CC) -c $(MSGSRC)
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.c, %.o, $(MSGSRC))
	$(RM) $(MSGSRC) $(patsubst %.c, %.o, $(MSGSRC))

install:
	[[ -z "$(LIB)" || -z "$(LIB_DIR)" ]] || mv $(LIB) $(LIB_DIR)
	[[ -z "$(SRC_DIR)" ]] || cp ./* $(SRC_DIR)

test:
	@echo "*** unit test unavailable currently ***"

cleancpp:
	$(RM) $(SRCS_Ff) $(OBJS_Ff) $(BUFRLIB).prm

clean:
	$(RM) $(OBJS) $(MODS) $(LIB)

.SUFFIXES: .c .h .F .f .mod .o

# Specify targets that do not generate filesystem objects
.PHONY: all build preproc install debug dbg_build message test clean cleancpp

