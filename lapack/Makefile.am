SWIG = swig
SWIGFLAGS = -guile -c
LD = g77
LDADD = -lswigguile @FLIBS@ -llapack -lblas2

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
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(echo $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

MOSTLYCLEANFILES = $(BUILT_SOURCES) *.scm
