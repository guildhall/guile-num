SWIG = swig
SWIGFLAGS = -guile -scmstub -Linkage module -outdir ..
PP = $(top_srcdir)/destill.scm

AM_CFLAGS = $(GUILE_CFLAGS)
LDADD = -lgsl -lgslcblas $(GUILE_LIBS)

subpkgdatadir = $(datadir)/guile/gsl
#subpkgdata_DATA = *.scm

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
	gsl_sort.h				\
	gsl_sort_vector.h			\
	gsl_spline.h				\
	gsl_statistics.h			\
	gsl_sum.h				\
	gsl_vector.h

SWIG_INTERFACES =				\
	blas.i					\
	block.i					\
	chebyshev.i				\
	combination.i				\
	dht.i					\
	diff.i					\
	eigen.i					\
	errno.i					\
	fft.i					\
	fit.i					\
	histogram.i				\
	histogram2d.i				\
	integration.i				\
	interp.i				\
	linalg.i				\
	math.i					\
	matrix.i				\
	min.i					\
	monte.i					\
	multifit.i				\
	multimin.i				\
	multiroots.i				\
	odeiv.i					\
	permute.i				\
	poly.i					\
	qrng.i					\
	randist.i				\
	rng.i					\
	roots.i					\
	sf.i					\
	siman.i					\
	sort.i					\
	sortvector.i				\
	spline.i				\
	statistics.i				\
	sum.i					\
	vector.i				\
	function.i

BUILT_SOURCES =					\
	$(GSL_HEADERFILES:.h=_inc.i)		\
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
	libguile-gsl-sort.la			\
	libguile-gsl-sort-vector.la		\
	libguile-gsl-spline.la			\
	libguile-gsl-sum.la			\
	libguile-gsl-statistics.la		\
	libguile-gsl-vector.la

libguile_gsl_blas_la_SOURCES =  blas.c
libguile_gsl_blas_la_LDFLAGS = $(LDADD)
libguile_gsl_block_la_SOURCES = block.c
libguile_gsl_block_la_LDFLAGS = $(LDADD)
libguile_gsl_chebyshev_la_SOURCES = chebyshev.c
libguile_gsl_chebyshev_la_LDFLAGS = $(LDADD)
libguile_gsl_combination_la_SOURCES = combination.c
libguile_gsl_combination_la_LDFLAGS = $(LDADD)
libguile_gsl_dht_la_SOURCES = dht.c
libguile_gsl_dht_la_LDFLAGS = $(LDADD)
libguile_gsl_diff_la_SOURCES = diff.c
libguile_gsl_diff_la_LDFLAGS = $(LDADD)
libguile_gsl_eigen_la_SOURCES = eigen.c
libguile_gsl_eigen_la_LDFLAGS = $(LDADD)
libguile_gsl_errno_la_SOURCES = errno.c
libguile_gsl_errno_la_LDFLAGS = $(LDADD)
libguile_gsl_fft_la_SOURCES = fft.c
libguile_gsl_fft_la_LDFLAGS = $(LDADD)
libguile_gsl_fit_la_SOURCES = fit.c
libguile_gsl_fit_la_LDFLAGS = $(LDADD)
libguile_gsl_function_la_SOURCES = function.c
libguile_gsl_function_la_LDFLAGS = $(LDADD)
libguile_gsl_histogram_la_SOURCES = histogram.c
libguile_gsl_histogram_la_LDFLAGS = $(LDADD)
libguile_gsl_histogram2d_la_SOURCES = histogram2d.c
libguile_gsl_histogram2d_la_LDFLAGS = $(LDADD)
libguile_gsl_integration_la_SOURCES = integration.c
libguile_gsl_integration_la_LDFLAGS = $(LDADD)
libguile_gsl_interp_la_SOURCES = interp.c
libguile_gsl_interp_la_LDFLAGS = $(LDADD)
libguile_gsl_linalg_la_SOURCES = linalg.c
libguile_gsl_linalg_la_LDFLAGS = $(LDADD)
libguile_gsl_math_la_SOURCES = math.c
libguile_gsl_math_la_LDFLAGS = $(LDADD)
libguile_gsl_matrix_la_SOURCES = matrix.c
libguile_gsl_matrix_la_LDFLAGS = $(LDADD)
libguile_gsl_min_la_SOURCES = min.c
libguile_gsl_min_la_LDFLAGS = $(LDADD)
libguile_gsl_monte_la_SOURCES = monte.c
libguile_gsl_monte_la_LDFLAGS = $(LDADD)
libguile_gsl_multifit_la_SOURCES = multifit.c
libguile_gsl_multifit_la_LDFLAGS = $(LDADD)
libguile_gsl_multimin_la_SOURCES = multimin.c
libguile_gsl_multimin_la_LDFLAGS = $(LDADD)
libguile_gsl_multiroots_la_SOURCES = multiroots.c
libguile_gsl_multiroots_la_LDFLAGS = $(LDADD)
libguile_gsl_permute_la_SOURCES = permute.c
libguile_gsl_permute_la_LDFLAGS = $(LDADD)
libguile_gsl_poly_la_SOURCES = poly.c
libguile_gsl_poly_la_LDFLAGS = $(LDADD)
libguile_gsl_qrng_la_SOURCES = qrng.c
libguile_gsl_qrng_la_LDFLAGS = $(LDADD)
libguile_gsl_randist_la_SOURCES = randist.c
libguile_gsl_randist_la_LDFLAGS = $(LDADD)
libguile_gsl_rng_la_SOURCES = rng.c
libguile_gsl_rng_la_LDFLAGS = $(LDADD)
libguile_gsl_roots_la_SOURCES = roots.c
libguile_gsl_roots_la_LDFLAGS = $(LDADD)
libguile_gsl_siman_la_SOURCES = siman.c
libguile_gsl_siman_la_LDFLAGS = $(LDADD)
libguile_gsl_sf_la_SOURCES = sf.c
libguile_gsl_sf_la_LDFLAGS = $(LDADD)
libguile_gsl_sort_la_SOURCES = sort.c
libguile_gsl_sort_la_LDFLAGS = $(LDADD)
libguile_gsl_sort_vector_la_SOURCES = sortvector.c
libguile_gsl_sort_vector_la_LDFLAGS = $(LDADD)
libguile_gsl_spline_la_SOURCES = spline.c
libguile_gsl_spline_la_LDFLAGS = $(LDADD)
libguile_gsl_sum_la_SOURCES = sum.c
libguile_gsl_sum_la_LDFLAGS = $(LDADD)
libguile_gsl_statistics_la_SOURCES = statistics.c
libguile_gsl_statistics_la_LDFLAGS = $(LDADD)
libguile_gsl_vector_la_SOURCES = vector.c
libguile_gsl_vector_la_LDFLAGS = $(LDADD)


SUFFIXES = .i
%.c: %.i
	$(SWIG) $(SWIGFLAGS) -o $@ $<

## -scmstub $$(basename $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') 

gsl_blas_inc.i: /usr/include/gsl/gsl_blas.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_(c|)blas.*' > $@

gsl_block_inc.i: /usr/include/gsl/gsl_block.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_block.*' > $@

gsl_chebyshev_inc.i: /usr/include/gsl/gsl_chebyshev.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_chebyshev.*' > $@

gsl_combination_inc.i: /usr/include/gsl/gsl_combination.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_combination.*' > $@

gsl_dht_inc.i: /usr/include/gsl/gsl_dht.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_dht.*' > $@

gsl_diff_inc.i: /usr/include/gsl/gsl_diff.h
	$(CPP) $^ | \
	$(PP) '/usr/include/gsl/gsl_diff.*' | \
	sed 's/result/OUTPUT/g;' | \
	sed 's/abserr/OUTPUT/g;' > $@

gsl_eigen_inc.i: /usr/include/gsl/gsl_eigen.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_eigen.*' > $@

gsl_errno_inc.i: /usr/include/gsl/gsl_errno.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_errno.*' > $@

gsl_fft_inc.i: /usr/include/gsl/gsl_fft_*.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_fft.*' > $@

gsl_fit_inc.i: /usr/include/gsl/gsl_fit.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_fit.*' > $@

gsl_histogram_inc.i: /usr/include/gsl/gsl_histogram.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_histogram.*' > $@

gsl_histogram2d_inc.i: /usr/include/gsl/gsl_histogram2d.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_histogram2d.*' > $@

gsl_integration_inc.i: /usr/include/gsl/gsl_integration.h
	$(CPP) $^ | \
	$(PP) '/usr/include/gsl/gsl_integration.*' | \
	sed 's/result/OUTPUT/g;' | \
	sed 's/abserr/OUTPUT/g;' > $@

gsl_interp_inc.i: /usr/include/gsl/gsl_interp.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_interp.*' > $@

gsl_linalg_inc.i: /usr/include/gsl/gsl_linalg.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_linalg.*' > $@

gsl_math_inc.i: /usr/include/gsl/gsl_math.h /usr/include/gsl/gsl_complex_math.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_(complex_|)math.*' > $@

gsl_matrix_inc.i: /usr/include/gsl/gsl_matrix.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_matrix.*' > $@

gsl_min_inc.i: /usr/include/gsl/gsl_min.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_min.*' > $@

gsl_monte_inc.i: /usr/include/gsl/gsl_monte*.h
	$(CPP) $^ | \
	$(PP) '/usr/include/gsl/gsl_monte.*' | \
	sed 's/double *\* *result/double *OUTPUT/g;' | \
	sed 's/double *\* *abserr/double *OUTPUT/g;' > $@

gsl_multifit_inc.i: /usr/include/gsl/gsl_multifit.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_multifit.*' > $@

gsl_multimin_inc.i: /usr/include/gsl/gsl_multimin.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_multimin.*' > $@

gsl_multiroots_inc.i: /usr/include/gsl/gsl_multiroots.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_multiroots.*' > $@

gsl_odeiv_inc.i: /usr/include/gsl/gsl_odeiv.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_odeiv.*' > $@

gsl_permute_inc.i: /usr/include/gsl/gsl_permute.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_permute.*' > $@

gsl_poly_inc.i: /usr/include/gsl/gsl_poly.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_poly.*' > $@

gsl_qrng_inc.i: /usr/include/gsl/gsl_qrng.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_qrng.*' > $@

gsl_randist_inc.i: /usr/include/gsl/gsl_randist.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_randist.*' > $@

gsl_rng_inc.i: /usr/include/gsl/gsl_rng.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_rng.*' > $@

gsl_roots_inc.i: /usr/include/gsl/gsl_roots.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_roots.*' > $@

gsl_sf_inc.i: /usr/include/gsl/gsl_sf.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_sf.*' > $@

gsl_siman_inc.i: /usr/include/gsl/gsl_siman.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_siman.*' > $@

gsl_sort_inc.i: /usr/include/gsl/gsl_sort.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_sort.*' > $@

gsl_sort_vector_inc.i: /usr/include/gsl/gsl_sort_vector.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_sort_vector.*' > $@

gsl_spline_inc.i: /usr/include/gsl/gsl_spline.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_spline.*' > $@

gsl_sum_inc.i: /usr/include/gsl/gsl_sum.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_sum.*' > $@

gsl_statistics_inc.i: /usr/include/gsl/gsl_statistics.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_statistics.*' > $@

gsl_vector_inc.i: /usr/include/gsl/gsl_vector.h
	$(CPP) $^ | $(PP) '/usr/include/gsl/gsl_vector.*' > $@


MOSTLYCLEANFILES = $(BUILT_SOURCES) *.scm
