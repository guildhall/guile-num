SWIG = swig
SWIGFLAGS = -guile -scmstub -Linkage module -outdir ..
LD = g77
LDADD = @FLIBS@ -llapack -lblas

subpkgdatadir = $(datadir)/guile/lapack
subpkgdata_DATA = lapack.scm

SWIG_INTERFACES = \
	lapack.i

BUILT_SOURCES = \
	lapack.c

lib_LTLIBRARIES = \
	libguile-lapack.la

libguile_lapack_la_SOURCES = lapack.c
libguile_lapack_la_LDFLAGS = $(LDADD)

SUFFIXES = .i
.i.c:
	$(SWIG) $(SWIGFLAGS) -o $@ $<

MOSTLYCLEANFILES = $(BUILT_SOURCES) *.scm
