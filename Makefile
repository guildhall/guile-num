PACKAGE=guile-numerics
VERSION=0.1.0
GUILE = guile

all:
	$(MAKE) -C gsl all
	$(MAKE) -C lapack all

dist: clean
	$(MAKE) -C gsl dist
	(cd ..; tar czf $(PACKAGE)-$(VERSION).tar.gz --exclude CVS $(PACKAGE) )

clean:
	$(MAKE) -C gsl clean
	$(MAKE) -C lapack clean
	find -name "*~" | xargs $(RM)

maintainer-clean:
	$(MAKE) -C gsl maintainer-clean
	$(MAKE) -C lapack maintainer-clean

check: all
	$(GUILE) -s test_gsl_blas.scm
	$(GUILE) -s test_gsl_chebyshev.scm
	$(GUILE) -s test_gsl_diff.scm
	$(GUILE) -s test_gsl_fit.scm
	$(GUILE) -s test_gsl_integration.scm
	$(GUILE) -s test_gsl_interp.scm
	$(GUILE) -s test_gsl_math.scm
	$(GUILE) -s test_gsl_minimize.scm
	$(GUILE) -s test_gsl_monte.scm
	$(GUILE) -s test_gsl_multimin.scm
	$(GUILE) -s test_gsl_multiroots.scm
	$(GUILE) -s test_gsl_odeiv.scm
	$(GUILE) -s test_gsl_qrng.scm
	$(GUILE) -s test_gsl_rng.scm
	$(GUILE) -s test_gsl_roots.scm
	$(GUILE) -s test_gsl_statistics.scm
	$(GUILE) -s test_lapack.scm
	$(GUILE) -s test_plot_gnuplot.scm
	$(GUILE) -s test_signal_filter.scm
