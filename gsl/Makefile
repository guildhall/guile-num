SWIG = swig
SWIGFLAGS = -guile -c
CC = gcc
CFLAGS = -g -O2 -DHAVE_INLINE -c -fpic -I/usr/include
LDFLAGS = -shared -lswigguile -lgsl -lgslcblas

all: libguile-gsl-math.so libguile-gsl-block.so libguile-gsl-vector.so libguile-gsl-matrix.so libguile-gsl-blas.so libguile-gsl-special.so libguile-gsl-errno.so libguile-gsl-poly.so libguile-gsl-permute.so libguile-gsl-combination.so libguile-gsl-linalg.so libguile-gsl-eigen.so libguile-gsl-fft.so libguile-gsl-integration.so libguile-gsl-rng.so libguile-gsl-qrng.so libguile-gsl-randist.so libguile-gsl-statistics.so libguile-gsl-histogram.so libguile-gsl-monte.so libguile-gsl-odeiv.so libguile-gsl-interp.so libguile-gsl-function.so libguile-gsl-diff.so libguile-gsl-chebyshev.so libguile-gsl-levin.so libguile-gsl-dht.so libguile-gsl-roots.so libguile-gsl-minimize.so libguile-gsl-multiroot-function.so libguile-gsl-multiroots.so libguile-gsl-fit.so libguile-gsl-multimin.so

dist: clean

%.o:%.c
	$(CC) $(CFLAGS) -o $@ $<

%_wrap.c: %.i
	$(SWIG) $(SWIGFLAGS) \
  -scmstub $$(echo $< | sed 's/_/-/g' | sed 's/\.i/\.scm/') \
  -o $@ $<

libguile-gsl-math.so: gsl_math_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-block.so: gsl_block_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-vector.so: gsl_vector_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-matrix.so: gsl_matrix_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-blas.so: gsl_blas_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-special.so: gsl_special_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-errno.so: gsl_errno_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-poly.so: gsl_poly_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-permute.so: gsl_permute_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-combination.so: gsl_combination_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-linalg.so: gsl_linalg_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-eigen.so: gsl_eigen_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-fft.so: gsl_fft_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-integration.so: gsl_integration_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-rng.so: gsl_rng_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-qrng.so: gsl_qrng_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-randist.so: gsl_randist_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-statistics.so: gsl_statistics_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-histogram.so: gsl_histogram_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-monte.so: gsl_monte_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libguile-gsl-odeiv.so: gsl_odeiv_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libguile-gsl-interp.so: gsl_interp_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-function.so: gsl_function_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libguile-gsl-diff.so: gsl_diff_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-chebyshev.so: gsl_chebyshev_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-levin.so: gsl_levin_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-dht.so: gsl_dht_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-roots.so: gsl_roots_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-minimize.so: gsl_minimize_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-multiroot-function.so: gsl_multiroot_function_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libguile-gsl-multiroots.so: gsl_multiroots_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-fit.so: gsl_fit_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libguile-gsl-multimin.so: gsl_multimin_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

.PRECIOUS: gsl_math_wrap.c gsl_block_wrap.c gsl_vector_wrap.c gsl_matrix_wrap.c gsl_blas_wrap.c gsl_special_wrap.c gsl_errno_wrap.c gsl_poly_wrap.c gsl_permute_wrap.c gsl_combination_wrap.c gsl_linalg_wrap.c gsl_eigen_wrap.c gsl_fft_wrap.c gsl_integration_wrap.c gsl_rng_wrap.c gsl_qrng_wrap.c gsl_randist_wrap.c gsl_statistics_wrap.c gsl_histogram_wrap.c gsl_monte_wrap.c gsl_odeiv_wrap.c gsl_interp_wrap.c gsl_function_wrap.c gsl_diff_wrap.c gsl_chebyshev_wrap.c gsl_levin_wrap.c gsl_dht_wrap.c gsl_roots_wrap.c gsl_minimize_wrap.c gsl_multiroot_function_wrap.c gsl_multiroots_wrap.c gsl_fit_wrap.c gsl_multimin_wrap.c


# maintainer-clean: clean
clean:
	$(RM) *.so *.o *~
	$(RM) gsl_*_wrap.c

.PHONY: all clean
