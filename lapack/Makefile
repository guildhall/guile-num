SWIG = swig
SWIGFLAGS = -guile -c
CC = gcc
LD = g77
CFLAGS = -g -O2 -DHAVE_INLINE
LDFLAGS = -shared -lswigguile -L/usr/lib/atlas -llapack -lblas

all: libguile-lapack.so

dist: all clean

%.o:%.c
	$(CC) $(CFLAGS) -c -fpic -o $@ $<

libguile-lapack.so: gsl_lapack_wrap.o
	$(LD) $(LDFLAGS) -o $@ $^

gsl_lapack_wrap.c: gsl_lapack.i
	$(SWIG) $(SWIGFLAGS) -scmstub lapack.scm $<

# maintainer-clean: clean
clean:
	$(RM) *.so *.o *~
	$(RM) gsl_*_wrap.c
	$(RM) lapack.scm

.PHONY: all clean
