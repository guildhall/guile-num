SWIG = swig
SWIGFLAGS = -guile -c

LDADD = -lswigguile -lgsl -lgslcblas -lguile

subpkgdatadir = $(datadir)/guile/gsl
subpkgdata_DATA = *.scm

SWIG_INTERFACES =				\
	gsl_blas.i				\
	gsl_block.i				\
	gsl_chebyshev.i				\
	gsl_combination.i			\
	gsl_dht.i				\
	gsl_diff.i				\
	gsl_eigen.i				\
	gsl_errno.i				\
	gsl_fft.i				\
	gsl_fit.i				\
	gsl_function.i				\
	gsl_histogram.i				\
	gsl_integration.i			\
	gsl_interp.i				\
	gsl_levin.i				\
	gsl_linalg.i				\
	gsl_math.i				\
	gsl_matrix.i				\
	gsl_minimize.i				\
	gsl_monte.i				\
	gsl_multimin.i				\
	gsl_multiroots.i			\
	gsl_odeiv.i				\
	gsl_permute.i				\
	gsl_poly.i				\
	gsl_qrng.i				\
	gsl_randist.i
	gsl_rng.i \
	gsl_roots.i \
	gsl_siman.i \
	gsl_special.i \
	gsl_statistics.i \
	gsl_vector.i

BUILT_SOURCES =					\
	gsl_blas.c				\
	gsl_block.c				\
	gsl_chebyshev.c				\
	gsl_combination.c			\
	gsl_dht.c				\
	gsl_diff.c				\
	gsl_eigen.c				\
	gsl_errno.c				\
	gsl_fft.c				\
	gsl_fit.c				\
	gsl_function.c				\
	gsl_histogram.c				\
	gsl_integration.c			\
	gsl_interp.c				\
	gsl_levin.c				\
	gsl_linalg.c				\
	gsl_math.c				\
	gsl_matrix.c				\
	gsl_minimize.c				\
	gsl_monte.c				\
	gsl_multimin.c				\
	gsl_multiroots.c			\
	gsl_permute.c				\
	gsl_poly.c				\
	gsl_qrng.c				\
	gsl_randist.c				\
	gsl_rng.c				\
	gsl_roots.c				\
	gsl_siman.c				\
	gsl_special.c				\
	gsl_statistics.c			\
	gsl_vector.c

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
	libguile-gsl-integration.la		\
	libguile-gsl-interp.la			\
	libguile-gsl-levin.la			\
	libguile-gsl-linalg.la			\
	libguile-gsl-math.la			\
	libguile-gsl-matrix.la			\
	libguile-gsl-minimize.la		\
	libguile-gsl-monte.la			\
	libguile-gsl-multimin.la		\
	libguile-gsl-multiroots.la		\
	libguile-gsl-permute.la			\
	libguile-gsl-poly.la			\
	libguile-gsl-qrng.la			\
	libguile-gsl-randist.la			\
	libguile-gsl-rng.la			\
	libguile-gsl-roots.la			\
	libguile-gsl-siman.la			\
	libguile-gsl-special.la			\
	libguile-gsl-statistics.la		\
	libguile-gsl-vector.la

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
libguile_gsl_integration_la_SOURCES = gsl_integration.c
libguile_gsl_integration_la_LDFLAGS = $(LDADD)
libguile_gsl_interp_la_SOURCES = gsl_interp.c
libguile_gsl_interp_la_LDFLAGS = $(LDADD)
libguile_gsl_levin_la_SOURCES = gsl_levin.c
libguile_gsl_levin_la_LDFLAGS = $(LDADD)
libguile_gsl_linalg_la_SOURCES = gsl_linalg.c
libguile_gsl_linalg_la_LDFLAGS = $(LDADD)
libguile_gsl_math_la_SOURCES = gsl_math.c
libguile_gsl_math_la_LDFLAGS = $(LDADD)
libguile_gsl_matrix_la_SOURCES = gsl_matrix.c
libguile_gsl_matrix_la_LDFLAGS = $(LDADD)
libguile_gsl_minimize_la_SOURCES = gsl_minimize.c
libguile_gsl_minimize_la_LDFLAGS = $(LDADD)
libguile_gsl_monte_la_SOURCES = gsl_monte.c
libguile_gsl_monte_la_LDFLAGS = $(LDADD)
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
libguile_gsl_special_la_SOURCES = gsl_special.c
libguile_gsl_special_la_LDFLAGS = $(LDADD)
libguile_gsl_statistics_la_SOURCES = gsl_statistics.c
libguile_gsl_statistics_la_LDFLAGS = $(LDADD)
libguile_gsl_vector_la_SOURCES = gsl_vector.c
libguile_gsl_vector_la_LDFLAGS = $(LDADD)


SUFFIXES = .i
.i.c:
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(echo $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

MOSTLYCLEANFILES = $(BUILT_SOURCES) *.scm
