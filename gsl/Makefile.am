SWIG = swig
SWIGFLAGS = -guile -c
PP = ../destill.scm

LDADD = -lswigguile -lgsl -lgslcblas -lguile

subpkgdatadir = $(datadir)/guile/gsl
subpkgdata_DATA = *.scm

GSL_HEADERFILES =				\
	gsl_blas.h				\
	gsl_block.h				\
	gsl_chebyshev.h				\
	gsl_combination.h			\
	gsl_dht.h				\
	gsl_diff.h				\
	gsl_eigen.h				\
	gsl_errno.h				\
	gsl_fft.h				\
	gsl_fit.h				\
	gsl_histogram.h				\
	gsl_histogram2d.h			\
	gsl_integration.h			\
	gsl_interp.h				\
	gsl_linalg.h				\
	gsl_math.h				\
	gsl_matrix.h				\
	gsl_min.h				\
	gsl_monte.h				\
	gsl_multifit.h				\
	gsl_multimin.h				\
	gsl_multiroots.h			\
	gsl_odeiv.h				\
	gsl_permute.h				\
	gsl_poly.h				\
	gsl_qrng.h				\
	gsl_randist.h				\
	gsl_rng.h				\
	gsl_roots.h				\
	gsl_sf.h				\
	gsl_siman.h				\
	gsl_spline.h				\
	gsl_statistics.h			\
	gsl_sum.h				\
	gsl_vector.h

SWIG_INTERFACES =				\
	$(GSL_HEADERFILES:.h=.i)		\
	gsl_function.i

BUILT_SOURCES =					\
	$(GSL_HEADERFILES:.h=.inc)		\
	$(SWIG_INTERFACES:.i=.c)

lib_LTLIBRARIES =				\
	libguile-gsl-blas.la			\
	libguile-gsl-block.la			\
	libguile-gsl-chebyshev.la		\
	libguile-gsl-combination.la		\
	libguile-gsl-dht.la			\
	libguile-gsl-diff.la			\
	libguile-gsl-eigen.la			\
	libguile-gsl-errno.la			\
	libguile-gsl-fft.la			\
	libguile-gsl-fit.la			\
	libguile-gsl-function.la		\
	libguile-gsl-histogram.la		\
	libguile-gsl-histogram2d.la		\
	libguile-gsl-integration.la		\
	libguile-gsl-interp.la			\
	libguile-gsl-linalg.la			\
	libguile-gsl-math.la			\
	libguile-gsl-matrix.la			\
	libguile-gsl-min.la			\
	libguile-gsl-monte.la			\
	libguile-gsl-multifit.la		\
	libguile-gsl-multimin.la		\
	libguile-gsl-multiroots.la		\
	libguile-gsl-permute.la			\
	libguile-gsl-poly.la			\
	libguile-gsl-qrng.la			\
	libguile-gsl-randist.la			\
	libguile-gsl-rng.la			\
	libguile-gsl-roots.la			\
	libguile-gsl-siman.la			\
	libguile-gsl-sf.la			\
	libguile-gsl-spline.la			\
	libguile-gsl-sum.la			\
	libguile-gsl-statistics.la		\
	libguile-gsl-vector.la

#	libguile-gsl-odeiv.la \
#

libguile_gsl_blas_la_SOURCES =  gsl_blas.c
libguile_gsl_blas_la_LDFLAGS = $(LDADD)
libguile_gsl_block_la_SOURCES = gsl_block.c
libguile_gsl_block_la_LDFLAGS = $(LDADD)
libguile_gsl_chebyshev_la_SOURCES = gsl_chebyshev.c
libguile_gsl_chebyshev_la_LDFLAGS = $(LDADD)
libguile_gsl_combination_la_SOURCES = gsl_combination.c
libguile_gsl_combination_la_LDFLAGS = $(LDADD)
libguile_gsl_dht_la_SOURCES = gsl_dht.c
libguile_gsl_dht_la_LDFLAGS = $(LDADD)
libguile_gsl_diff_la_SOURCES = gsl_diff.c
libguile_gsl_diff_la_LDFLAGS = $(LDADD)
libguile_gsl_eigen_la_SOURCES = gsl_eigen.c
libguile_gsl_eigen_la_LDFLAGS = $(LDADD)
libguile_gsl_errno_la_SOURCES = gsl_errno.c
libguile_gsl_errno_la_LDFLAGS = $(LDADD)
libguile_gsl_fft_la_SOURCES = gsl_fft.c
libguile_gsl_fft_la_LDFLAGS = $(LDADD)
libguile_gsl_fit_la_SOURCES = gsl_fit.c
libguile_gsl_fit_la_LDFLAGS = $(LDADD)
libguile_gsl_function_la_SOURCES = gsl_function.c
libguile_gsl_function_la_LDFLAGS = $(LDADD)
libguile_gsl_histogram_la_SOURCES = gsl_histogram.c
libguile_gsl_histogram_la_LDFLAGS = $(LDADD)
libguile_gsl_histogram2d_la_SOURCES = gsl_histogram2d.c
libguile_gsl_histogram2d_la_LDFLAGS = $(LDADD)
libguile_gsl_integration_la_SOURCES = gsl_integration.c
libguile_gsl_integration_la_LDFLAGS = $(LDADD)
libguile_gsl_interp_la_SOURCES = gsl_interp.c
libguile_gsl_interp_la_LDFLAGS = $(LDADD)
libguile_gsl_linalg_la_SOURCES = gsl_linalg.c
libguile_gsl_linalg_la_LDFLAGS = $(LDADD)
libguile_gsl_math_la_SOURCES = gsl_math.c
libguile_gsl_math_la_LDFLAGS = $(LDADD)
libguile_gsl_matrix_la_SOURCES = gsl_matrix.c
libguile_gsl_matrix_la_LDFLAGS = $(LDADD)
libguile_gsl_min_la_SOURCES = gsl_min.c
libguile_gsl_min_la_LDFLAGS = $(LDADD)
libguile_gsl_monte_la_SOURCES = gsl_monte.c
libguile_gsl_monte_la_LDFLAGS = $(LDADD)
libguile_gsl_multifit_la_SOURCES = gsl_multifit.c
libguile_gsl_multifit_la_LDFLAGS = $(LDADD)
libguile_gsl_multimin_la_SOURCES = gsl_multimin.c
libguile_gsl_multimin_la_LDFLAGS = $(LDADD)
libguile_gsl_multiroots_la_SOURCES = gsl_multiroots.c
libguile_gsl_multiroots_la_LDFLAGS = $(LDADD)
libguile_gsl_permute_la_SOURCES = gsl_permute.c
libguile_gsl_permute_la_LDFLAGS = $(LDADD)
libguile_gsl_poly_la_SOURCES = gsl_poly.c
libguile_gsl_poly_la_LDFLAGS = $(LDADD)
libguile_gsl_qrng_la_SOURCES = gsl_qrng.c
libguile_gsl_qrng_la_LDFLAGS = $(LDADD)
libguile_gsl_randist_la_SOURCES = gsl_randist.c
libguile_gsl_randist_la_LDFLAGS = $(LDADD)
libguile_gsl_rng_la_SOURCES = gsl_rng.c
libguile_gsl_rng_la_LDFLAGS = $(LDADD)
libguile_gsl_roots_la_SOURCES = gsl_roots.c
libguile_gsl_roots_la_LDFLAGS = $(LDADD)
libguile_gsl_siman_la_SOURCES = gsl_siman.c
libguile_gsl_siman_la_LDFLAGS = $(LDADD)
libguile_gsl_sf_la_SOURCES = gsl_sf.c
libguile_gsl_sf_la_LDFLAGS = $(LDADD)
libguile_gsl_spline_la_SOURCES = gsl_spline.c
libguile_gsl_spline_la_LDFLAGS = $(LDADD)
libguile_gsl_sum_la_SOURCES = gsl_sum.c
libguile_gsl_sum_la_LDFLAGS = $(LDADD)
libguile_gsl_statistics_la_SOURCES = gsl_statistics.c
libguile_gsl_statistics_la_LDFLAGS = $(LDADD)
libguile_gsl_vector_la_SOURCES = gsl_vector.c
libguile_gsl_vector_la_LDFLAGS = $(LDADD)


SUFFIXES = .i .inc
%.c: %.i %.inc
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(echo $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

%.c: %.i
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(echo $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

gsl_blas.inc: /usr/include/gsl/gsl_blas.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_(c|)blas.*' > $@

gsl_block.inc: /usr/include/gsl/gsl_block.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_block.*' > $@

gsl_chebyshev.inc: /usr/include/gsl/gsl_chebyshev.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_chebyshev.*' > $@

gsl_combination.inc: /usr/include/gsl/gsl_combination.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_combination.*' > $@

gsl_dht.inc: /usr/include/gsl/gsl_dht.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_dht.*' > $@

gsl_diff.inc: /usr/include/gsl/gsl_diff.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_diff.*' > $@

gsl_eigen.inc: /usr/include/gsl/gsl_eigen.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_eigen.*' > $@

gsl_errno.inc: /usr/include/gsl/gsl_errno.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_errno.*' > $@

gsl_fft.inc: /usr/include/gsl/gsl_fft_*.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_fft.*' > $@

gsl_fit.inc: /usr/include/gsl/gsl_fit.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_fit.*' > $@

gsl_histogram.inc: /usr/include/gsl/gsl_histogram.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_histogram.*' > $@

gsl_histogram2d.inc: /usr/include/gsl/gsl_histogram2d.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_histogram2d.*' > $@

gsl_integration.inc: /usr/include/gsl/gsl_integration.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_integration.*' > $@

gsl_interp.inc: /usr/include/gsl/gsl_interp.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_interp.*' > $@

gsl_linalg.inc: /usr/include/gsl/gsl_linalg.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_linalg.*' > $@

gsl_math.inc: /usr/include/gsl/gsl_math.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_math.*' > $@

gsl_matrix.inc: /usr/include/gsl/gsl_matrix.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_matrix.*' > $@

gsl_min.inc: /usr/include/gsl/gsl_min.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_min.*' > $@

gsl_monte.inc: /usr/include/gsl/gsl_monte*.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_monte.*' > $@

gsl_multifit.inc: /usr/include/gsl/gsl_multifit.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_multifit.*' > $@

gsl_multimin.inc: /usr/include/gsl/gsl_multimin.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_multimin.*' > $@

gsl_multiroots.inc: /usr/include/gsl/gsl_multiroots.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_multiroots.*' > $@

gsl_odeiv.inc: /usr/include/gsl/gsl_odeiv.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_odeiv.*' > $@

gsl_permute.inc: /usr/include/gsl/gsl_permute.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_permute.*' > $@

gsl_poly.inc: /usr/include/gsl/gsl_poly.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_poly.*' > $@

gsl_qrng.inc: /usr/include/gsl/gsl_qrng.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_qrng.*' > $@

gsl_randist.inc: /usr/include/gsl/gsl_randist.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_randist.*' > $@

gsl_rng.inc: /usr/include/gsl/gsl_rng.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_rng.*' > $@

gsl_roots.inc: /usr/include/gsl/gsl_roots.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_roots.*' > $@

gsl_sf.inc: /usr/include/gsl/gsl_sf.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_sf.*' > $@

gsl_siman.inc: /usr/include/gsl/gsl_siman.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_siman.*' > $@

gsl_spline.inc: /usr/include/gsl/gsl_spline.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_spline.*' > $@

gsl_sum.inc: /usr/include/gsl/gsl_sum.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_sum.*' > $@

gsl_statistics.inc: /usr/include/gsl/gsl_statistics.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_statistics.*' > $@

gsl_vector.inc: /usr/include/gsl/gsl_vector.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_vector.*' > $@


MOSTLYCLEANFILES = $(BUILT_SOURCES) *.scm
