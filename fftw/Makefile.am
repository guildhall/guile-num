SWIG = swig
SWIGFLAGS = -guile -c
LDADD = -lswigguile -lfftw3 -lm

subpkgdatadir = $(datadir)/guile/fftw
subpkgdata_DATA = *.scm

SWIG_INTERFACES = \
	fftw.i

BUILT_SOURCES = \
	fftw.c

lib_LTLIBRARIES = \
	libguile-fftw.la

libguile_fftw_la_SOURCES = fftw.c
libguile_fftw_la_LDFLAGS = $(LDADD)

SUFFIXES = .i
.i.c:
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(basename $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

MOSTLYCLEANFILES = $(BUILT_SOURCES) *.scm
