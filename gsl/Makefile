SWIG = swig
SWIGFLAGS = -guile -c -Linkage ltdlmod
CC = gcc
CFLAGS = -g -O2 -DHAVE_INLINE -c -fpic
LDFLAGS = -shared -lswigguile -lgsl -lgslcblas

all: libgsl-math.so libgsl-block.so libgsl-vector.so libgsl-matrix.so libgsl-blas.so libgsl-special.so libgsl-errno.so libgsl-poly.so libgsl-permute.so libgsl-combination.so libgsl-linalg.so libgsl-eigen.so libgsl-fft.so libgsl-integration.so libgsl-rng.so libgsl-qrng.so libgsl-randist.so libgsl-statistics.so libgsl-histogram.so libgsl-monte.so libgsl-odeiv.so libgsl-interp.so libgsl-function.so libgsl-diff.so libgsl-chebyshev.so libgsl-levin.so libgsl-dht.so libgsl-roots.so libgsl-minimize.so libgsl-multiroot-function.so libgsl-multiroots.so libgsl-fit.so

dist: clean

%.o:%.c
	$(CC) $(CFLAGS) -o $@ $<

%_wrap.c: %.i
	$(SWIG) $(SWIGFLAGS) -o $@ $<

libgsl-math.so: gsl_math_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-block.so: gsl_block_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-vector.so: gsl_vector_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-matrix.so: gsl_matrix_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-blas.so: gsl_blas_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-special.so: gsl_special_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-errno.so: gsl_errno_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-poly.so: gsl_poly_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-permute.so: gsl_permute_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-combination.so: gsl_combination_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-linalg.so: gsl_linalg_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-eigen.so: gsl_eigen_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-fft.so: gsl_fft_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-integration.so: gsl_integration_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-rng.so: gsl_rng_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-qrng.so: gsl_qrng_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-randist.so: gsl_randist_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-statistics.so: gsl_statistics_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-histogram.so: gsl_histogram_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-monte.so: gsl_monte_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libgsl-odeiv.so: gsl_odeiv_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libgsl-interp.so: gsl_interp_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-function.so: gsl_function_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libgsl-diff.so: gsl_diff_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-chebyshev.so: gsl_chebyshev_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-levin.so: gsl_levin_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-dht.so: gsl_dht_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-roots.so: gsl_roots_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-minimize.so: gsl_minimize_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-multiroot-function.so: gsl_multiroot_function_wrap.o
	$(CC) $(LDFLAGS) -lguile -o $@ $^

libgsl-multiroots.so: gsl_multiroots_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

libgsl-fit.so: gsl_fit_wrap.o
	$(CC) $(LDFLAGS) -o $@ $^

.PRECIOUS: gsl_math_wrap.c gsl_block_wrap.c gsl_vector_wrap.c gsl_matrix_wrap.c gsl_blas_wrap.c gsl_special_wrap.c gsl_errno_wrap.c gsl_poly_wrap.c gsl_permute_wrap.c gsl_combination_wrap.c gsl_linalg_wrap.c gsl_eigen_wrap.c gsl_fft_wrap.c gsl_integration_wrap.c gsl_rng_wrap.c gsl_qrng_wrap.c gsl_randist_wrap.c gsl_statistics_wrap.c gsl_histogram_wrap.c gsl_monte_wrap.c gsl_odeiv_wrap.c gsl_interp_wrap.c gsl_function_wrap.c gsl_diff_wrap.c gsl_chebyshev_wrap.c gsl_levin_wrap.c gsl_dht_wrap.c gsl_roots_wrap.c gsl_minimize_wrap.c gsl_multiroot_function_wrap.c gsl_multiroots_wrap.c gsl_fit_wrap.c


# maintainer-clean: clean
clean:
	$(RM) *.so *.o *~
	$(RM) gsl_*_wrap.c

.PHONY: all clean
