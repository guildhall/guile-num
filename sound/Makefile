SWIG = swig
SWIGFLAGS = -guile -c
CC = gcc
LD = gcc
CFLAGS = -g -O2
LDFLAGS = -shared -lswigguile -lsndfile

all: libguile-sndfile.so

dist: all clean

%.o:%.c
	$(CC) $(CFLAGS) -c -fpic -o $@ $<

libguile-sndfile.so: sndfile_wrap.o
	$(LD) $(LDFLAGS) -o $@ $^

sndfile_wrap.c: sndfile.i
	$(SWIG) $(SWIGFLAGS) -scmstub sndfile.scm $<

# maintainer-clean: clean
clean:
	$(RM) *.so *.o *~
	$(RM) *_wrap.c
	$(RM) sndfile.scm

.PHONY: all clean
