SHELL=/bin/sh

# sources, headers, objects, library and etc.
SRCS_F  = $(wildcard modv_*.F) $(wildcard moda_*.F)
SRCS_f  = $(wildcard *.f)
SRCS_C  = $(wildcard *.c)
DEPS    = $(wildcard *.h)
OBJS    = $(patsubst %.c, %.o, $(SRCS_C)) \
          $(patsubst %.F, %.o, $(SRCS_F)) \
          $(patsubst %.f, %.o, $(SRCS_f))
MODS    = $(wildcard *.mod)

all: build

build: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

debug: dbg_build

dbg_build: override CFLAGS += $(DEBUG)
dbg_build: override FFLAGS += $(DEBUG)
dbg_build: $(LIB)

%.o: %.c
	$(CC) -c $(CFLAGS) $(CFLAGSDEFS) $<

%.o: %.F
	$(FC) -c $(FPPCPP) $(FFLAGS) $(FFLAGSDEFS) $<

%.o: %.f
	$(FC) -c $(FFLAGS) $(FFLAGSDEFS) $<

message: $(MSGSRC)
	$(CC) -c $(MSGSRC)
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.c, %.o, $(MSGSRC))
	$(RM) $(MSGSRC) $(patsubst %.c, %.o, $(MSGSRC))

install:
	[[ -z "$(LIB)" || -z "$(LIB_DIR)" ]] || mv $(LIB) $(LIB_DIR)
	[[ -z "$(SRC_DIR)" ]] || cp ./* $(SRC_DIR)

test:
	@echo "*** unit test unavailable currently ***"

clean:
	$(RM) $(OBJS) $(MODS) $(LIB)

.SUFFIXES: .c .h .F .f .mod .o

# Specify targets that do not generate filesystem objects
.PHONY: all build install debug dbg_build message test clean

