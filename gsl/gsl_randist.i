%module "gsl/gsl-randist"

%{
#include <gsl/gsl_randist.h>
%}

%multiple_values;

extern double gsl_ran_gaussian (const gsl_rng * R, double SIGMA);
extern double gsl_ran_gaussian_pdf (double X, double SIGMA);
extern double gsl_ran_gaussian_ratio_method (const gsl_rng * R, const double SIGMA);
extern double gsl_ran_ugaussian (const gsl_rng * R);
extern double gsl_ran_ugaussian_pdf (double X);
extern double gsl_ran_ugaussian_ratio_method (const gsl_rng * R);

extern double gsl_ran_gaussian_tail (const gsl_rng * R, double A, double SIGMA);
extern double gsl_ran_gaussian_tail_pdf (double X, double A, double SIGMA);
extern double gsl_ran_ugaussian_tail (const gsl_rng * R, double A);
extern double gsl_ran_ugaussian_tail_pdf (double X, double A);

extern void gsl_ran_bivariate_gaussian (const gsl_rng * R, double SIGMA_X, double SIGMA_Y, double RHO, double * OUTPUT, double * OUTPUT);
extern double gsl_ran_bivariate_gaussian_pdf (double X, double Y, double SIGMA_X, double SIGMA_Y, double RHO);

extern double gsl_ran_exponential (const gsl_rng * R, double MU);
extern double gsl_ran_exponential_pdf (double X, double MU);

extern double gsl_ran_laplace (const gsl_rng * R, double A);
extern double gsl_ran_laplace_pdf (double X, double A);

extern double gsl_ran_exppow (const gsl_rng * R, double A, double B);
extern double gsl_ran_exppow_pdf (double X, double A, double B);

extern double gsl_ran_cauchy (const gsl_rng * R, double A);
extern double gsl_ran_cauchy_pdf (double X, double A);

extern double gsl_ran_rayleigh (const gsl_rng * R, double SIGMA);
extern double gsl_ran_rayleigh_pdf (double X, double SIGMA);

extern double gsl_ran_rayleigh_tail (const gsl_rng * R, double A, double SIGMA);
extern double gsl_ran_rayleigh_tail_pdf (double X, double A, double SIGMA);

extern double gsl_ran_landau (const gsl_rng * R);
extern double gsl_ran_landau_pdf (double X);

extern double gsl_ran_levy (const gsl_rng * R, double C, double ALPHA);

extern double gsl_ran_levy_skew (const gsl_rng * R, double C, double ALPHA, double BETA);

extern double gsl_ran_gamma (const gsl_rng * R, double A, double B);
extern double gsl_ran_gamma_pdf (double X, double A, double B);

extern double gsl_ran_flat (const gsl_rng * R, double A, double B);
extern double gsl_ran_flat_pdf (double X, double A, double B);

extern double gsl_ran_lognormal (const gsl_rng * R, double ZETA, double SIGMA);
extern double gsl_ran_lognormal_pdf (double X, double ZETA, double SIGMA);

extern double gsl_ran_chisq (const gsl_rng * R, double NU);
extern double gsl_ran_chisq_pdf (double X, double NU);

extern double gsl_ran_fdist (const gsl_rng * R, double NU1, double NU2);
extern double gsl_ran_fdist_pdf (double X, double NU1, double NU2);

extern double gsl_ran_tdist (const gsl_rng * R, double NU);
extern double gsl_ran_tdist_pdf (double X, double NU);

extern double gsl_ran_beta (const gsl_rng * R, double A, double B);
extern double gsl_ran_beta_pdf (double X, double A, double B);

extern double gsl_ran_logistic (const gsl_rng * R, double A);
extern double gsl_ran_logistic_pdf (double X, double A);

extern double gsl_ran_pareto (const gsl_rng * R, double A, double B);
extern double gsl_ran_pareto_pdf (double X, double A, double B);

extern void gsl_ran_dir_2d (const gsl_rng * R, double *OUTPUT, double *OUTPUT);
extern void gsl_ran_dir_2d_trig_method (const gsl_rng * R, double *OUTPUT, double *OUTPUT);
extern void gsl_ran_dir_3d (const gsl_rng * R, double *OUTPUT, double *OUTPUT, double * OUTPUT);
extern void gsl_ran_dir_nd (const gsl_rng * R, size_t N, double *OUTPUT);

extern double gsl_ran_weibull (const gsl_rng * R, double A, double B);
extern double gsl_ran_weibull_pdf (double X, double A, double B);

extern double gsl_ran_gumbel1 (const gsl_rng * R, double A, double B);
extern double gsl_ran_gumbel1_pdf (double X, double A, double B);

extern double gsl_ran_gumbel2 (const gsl_rng * R, double A, double B);
extern double gsl_ran_gumbel2_pdf (double X, double A, double B);

extern gsl_ran_discrete_t * gsl_ran_discrete_preproc (size_t K, const double * P);
extern size_t gsl_ran_discrete (const gsl_rng * R, const gsl_ran_discrete_t * G);
extern double gsl_ran_discrete_pdf (size_t K, const gsl_ran_discrete_t * G);
extern void gsl_ran_discrete_free (gsl_ran_discrete_t * G);

extern unsigned int gsl_ran_poisson (const gsl_rng * R, double MU);
extern double gsl_ran_poisson_pdf (unsigned int K, double MU);

extern unsigned int gsl_ran_bernoulli (const gsl_rng * R, double P);
extern double gsl_ran_bernoulli_pdf (unsigned int K, double P);

extern unsigned int gsl_ran_binomial (const gsl_rng * R, double P, unsigned int N);
extern double gsl_ran_binomial_pdf (unsigned int K, double P, unsigned int N);

extern unsigned int gsl_ran_negative_binomial (const gsl_rng * R, double P, double N);
extern double gsl_ran_negative_binomial_pdf (unsigned int K, double P, double N);

extern unsigned int gsl_ran_pascal (const gsl_rng * R, double P, unsigned int K);
extern double gsl_ran_pascal_pdf (unsigned int K, double P, unsigned int N);
extern  unsigned int gsl_ran_geometric (const gsl_rng * R, double P);
extern double gsl_ran_geometric_pdf (unsigned int K, double P);

extern unsigned int gsl_ran_hypergeometric (const gsl_rng * R, unsigned int N1, unsigned int N2, unsigned int T);
extern double gsl_ran_hypergeometric_pdf (unsigned int K, unsigned int N1, unsigned int N2, unsigned int T);

extern unsigned int gsl_ran_logarithmic (const gsl_rng * R, double P);
extern double gsl_ran_logarithmic_pdf (unsigned int K, double P);

// these functions have to get a different interface:
extern void gsl_ran_shuffle (const gsl_rng * R, void * BASE, size_t N, size_t SIZE);
extern int gsl_ran_choose (const gsl_rng * R, void * DEST, size_t K, void * SRC, size_t N, size_t SIZE);
extern void gsl_ran_sample (const gsl_rng * R, void * DEST, size_t K, void * SRC, size_t N, size_t SIZE);
