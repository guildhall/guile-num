SWIG = swig
SWIGFLAGS = -guile -scmstub -Linkage module -outdir ..
LDADD = -lfftw3 -lm

#subpkgdatadir = $(datadir)/guile/fftw
#subpkgdata_DATA = *.scm

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
	$(SWIG) $(SWIGFLAGS) -o $@ $<

# -scmstub $$(basename $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
#

MOSTLYCLEANFILES = $(BUILT_SOURCES)
