SWIG = swig
SWIGFLAGS = -guile -c
LDADD = -lswigguile -lsndfile

subpkgdatadir = $(datadir)/guile/sound
subpkgdata_DATA = *.scm

SWIG_INTERFACES = \
	sndfile.i

BUILT_SOURCES = \
	sndfile.c

lib_LTLIBRARIES = \
	libguile-sndfile.la

libguile_sndfile_la_SOURCES =  sndfile.c
libguile_sndfile_la_LDFLAGS = $(LDADD)

SUFFIXES = .i
.i.c:
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(echo $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

MOSTLYCLEANFILES = $(BUILT_SOURCES) sndfile.scm
