%module "gsl/gsl-special"

%{
#include <gsl/gsl_sf.h>
%}

typedef enum {GSL_PREC_DOUBLE, GSL_PREC_SINGLE, GSL_PREC_APPROX} gsl_mode_t;

extern double gsl_sf_airy_Ai (double X, gsl_mode_t MODE);
extern double gsl_sf_airy_Bi (double X, gsl_mode_t MODE);
extern double gsl_sf_airy_Ai_scaled (double X, gsl_mode_t MODE);
extern double gsl_sf_airy_Bi_scaled (double X, gsl_mode_t MODE);

extern double gsl_sf_airy_Ai_deriv (double X, gsl_mode_t MODE);
extern double gsl_sf_airy_Bi_deriv (double X, gsl_mode_t MODE);
extern double gsl_sf_airy_Ai_deriv_scaled (double X, gsl_mode_t MODE);
extern double gsl_sf_airy_Bi_deriv_scaled (double X, gsl_mode_t MODE);

extern double gsl_sf_airy_zero_Ai (unsigned int S);
extern double gsl_sf_airy_zero_Bi (unsigned int S);
extern double gsl_sf_airy_zero_Ai_deriv (unsigned int S);
extern double gsl_sf_airy_zero_Bi_deriv (unsigned int S);

extern double gsl_sf_bessel_J0 (double X);
extern double gsl_sf_bessel_J1 (double X);
extern double gsl_sf_bessel_Jn (int N, double X);
//extern int gsl_sf_bessel_Jn_array (int NMIN, int NMAX, double X, double RESULT_ARRAY[]);
extern double gsl_sf_bessel_Y0 (double X);
extern double gsl_sf_bessel_Y1 (double X);
extern double gsl_sf_bessel_Yn (int N,double X);
//extern int gsl_sf_bessel_Yn_array (int NMIN, int NMAX, double X, double RESULT_ARRAY[]);

extern double gsl_sf_bessel_I0 (double X);
extern double gsl_sf_bessel_I1 (double X);
extern double gsl_sf_bessel_In (int N, double X);
//extern int gsl_sf_bessel_In_array (int NMIN, int NMAX, double X, double RESULT_ARRAY[]);
extern double gsl_sf_bessel_I0_scaled (double X);
extern double gsl_sf_bessel_I1_scaled (double X);
extern double gsl_sf_bessel_In_scaled (int N, double X);
//extern int gsl_sf_bessel_In_scaled_array (int NMIN, int NMAX, double X, double RESULT_ARRAY[]);

extern double gsl_sf_bessel_K0 (double X);
extern double gsl_sf_bessel_K1 (double X);
extern double gsl_sf_bessel_Kn (int N, double X);
//extern int gsl_sf_bessel_Kn_array (int NMIN, int NMAX, double X, double RESULT_ARRAY[]);
extern double gsl_sf_bessel_K0_scaled (double X);
extern double gsl_sf_bessel_K1_scaled (double X);
extern double gsl_sf_bessel_Kn_scaled (int N, double X);
//extern int gsl_sf_bessel_Kn_scaled_array (int NMIN, int NMAX, double X, double RESULT_ARRAY[]);

extern double gsl_sf_bessel_j0 (double X);
extern double gsl_sf_bessel_j1 (double X);
extern double gsl_sf_bessel_j2 (double X);
extern double gsl_sf_bessel_jl (int L, double X);
//extern int gsl_sf_bessel_jl_array (int LMAX, double X, double RESULT_ARRAY[]);
//extern int gsl_sf_bessel_jl_steed_array (int LMAX, double X, double * JL_X_ARRAY);

extern double gsl_sf_bessel_y0 (double X);
extern double gsl_sf_bessel_y1 (double X);
extern double gsl_sf_bessel_y2 (double X);
extern double gsl_sf_bessel_yl (int L, double X);
//extern int gsl_sf_bessel_yl_array (int LMAX, double X, double RESULT_ARRAY[]);

extern double gsl_sf_bessel_i0_scaled (double X);
extern double gsl_sf_bessel_i1_scaled (double X);
extern double gsl_sf_bessel_i2_scaled (double X);
extern double gsl_sf_bessel_il_scaled (int L, double X);
//extern int gsl_sf_bessel_il_scaled_array (int LMAX, double X, double RESULT_ARRAY[]);

extern double gsl_sf_bessel_k0_scaled (double X);
extern double gsl_sf_bessel_k1_scaled (double X);
extern double gsl_sf_bessel_k2_scaled (double X);
extern double gsl_sf_bessel_kl_scaled (int L, double X);
//extern int gsl_sf_bessel_kl_scaled_array (int LMAX, double X, double RESULT_ARRAY[]);

extern double gsl_sf_bessel_Jnu (double NU, double X);
extern double gsl_sf_bessel_Ynu (double NU, double X);
extern double gsl_sf_bessel_Inu (double NU, double X);
extern double gsl_sf_bessel_Inu_scaled (double NU, double X);
extern double gsl_sf_bessel_Knu (double NU, double X);
extern double gsl_sf_bessel_lnKnu (double NU, double X);
extern double gsl_sf_bessel_Knu_scaled (double NU, double X);

extern double gsl_sf_bessel_zero_J0 (unsigned int S);
extern double gsl_sf_bessel_zero_J1 (unsigned int S);
extern double gsl_sf_bessel_zero_Jnu (double NU, unsigned int S);

extern double gsl_sf_clausen (double X);

/* Coulomb functions not included */

extern double gsl_sf_coupling_3j (int TWO_JA, int TWO_JB, int TWO_JC, int TWO_MA, int TWO_MB, int TWO_MC);
extern double gsl_sf_coupling_6j (int TWO_JA, int TWO_JB, int TWO_JC, int TWO_JD, int TWO_JE, int TWO_JF);
extern double gsl_sf_coupling_9j (int TWO_JA, int TWO_JB, int TWO_JC, int TWO_JD, int TWO_JE, int TWO_JF, int TWO_JG, int TWO_JH, int TWO_JI);

extern double gsl_sf_dawson (double X);

extern double gsl_sf_debye_1 (double X);
extern double gsl_sf_debye_2 (double X);
extern double gsl_sf_debye_3 (double X);
extern double gsl_sf_debye_4 (double X);

extern double gsl_sf_dilog (double X);
// int gsl_sf_complex_dilog_e (double R, double THETA, gsl_sf_result * RESULT_RE, gsl_sf_result * RESULT_IM);

/* Elementary operations not included */

extern double gsl_sf_ellint_Kcomp (double K, gsl_mode_t MODE);
extern double gsl_sf_ellint_Ecomp (double K, gsl_mode_t MODE);
extern double gsl_sf_ellint_F (double PHI, double K, gsl_mode_t MODE);
extern double gsl_sf_ellint_E (double PHI, double K, gsl_mode_t MODE);
extern double gsl_sf_ellint_P (double PHI, double K, double N, gsl_mode_t MODE);
extern double gsl_sf_ellint_D (double PHI, double K, double N, gsl_mode_t MODE);

extern double gsl_sf_ellint_RC (double X, double Y, gsl_mode_t MODE);
extern double gsl_sf_ellint_RD (double X, double Y, double Z, gsl_mode_t MODE);
extern double gsl_sf_ellint_RF (double X, double Y, double Z, gsl_mode_t MODE);
extern double gsl_sf_ellint_RJ (double X, double Y, double Z, double P, gsl_mode_t MODE);

int gsl_sf_elljac_e (double U, double M, double * SN, double * CN, double * DN);

extern double gsl_sf_erf (double X);
extern double gsl_sf_erfc (double X);
extern double gsl_sf_log_erfc (double X);
extern double gsl_sf_erf_Z (double X);
extern double gsl_sf_erf_Q (double X);

extern double gsl_sf_exp (double X);
//extern int gsl_sf_exp_e10_e (double X, gsl_sf_result_e10 * RESULT);
extern double gsl_sf_exp_mult (double X, double Y);
//extern int gsl_sf_exp_mult_e10_e (const double X, const double Y, gsl_sf_result_e10 * RESULT);
extern double gsl_sf_expm1 (double X);
extern double gsl_sf_exprel (double X);
extern double gsl_sf_exprel_2 (double X);
extern double gsl_sf_exprel_n (int N, double X);

/* Exponentiation With Error Estimate not included */

extern double gsl_sf_expint_E1 (double X);
extern double gsl_sf_expint_E2 (double X);
extern double gsl_sf_expint_Ei (double X);
extern double gsl_sf_Shi (double X);
extern double gsl_sf_Chi (double X);
extern double gsl_sf_expint_3 (double X);
extern double gsl_sf_Si (const double X);
extern double gsl_sf_Ci (const double X);
extern double gsl_sf_atanint (double X);

extern double gsl_sf_fermi_dirac_m1 (double X);
extern double gsl_sf_fermi_dirac_0 (double X);
extern double gsl_sf_fermi_dirac_1 (double X);
extern double gsl_sf_fermi_dirac_2 (double X);
extern double gsl_sf_fermi_dirac_int (int J, double X);
extern double gsl_sf_fermi_dirac_mhalf (double X);
extern double gsl_sf_fermi_dirac_half (double X);
extern double gsl_sf_fermi_dirac_3half (double X);
extern double gsl_sf_fermi_dirac_inc_0 (double X, double B);

extern double gsl_sf_gamma (double X);
extern double gsl_sf_lngamma (double X);
extern double gsl_sf_gammastar (double X);
extern double gsl_sf_gammainv (double X);
extern double gsl_sf_taylorcoeff (int N, double X);
extern double gsl_sf_fact (unsigned int N);
extern double gsl_sf_doublefact (unsigned int N);
extern double gsl_sf_lnfact (unsigned int N);
extern double gsl_sf_lndoublefact (unsigned int N);
extern double gsl_sf_choose (unsigned int N, unsigned int M);
extern double gsl_sf_lnchoose (unsigned int N, unsigned int M);
extern double gsl_sf_poch (double A, double X);
extern double gsl_sf_lnpoch (double A, double X);
extern double gsl_sf_pochrel (double A, double X);
extern double gsl_sf_gamma_inc_Q (double A, double X);
extern double gsl_sf_gamma_inc_P (double A, double X);
extern double gsl_sf_beta (double A, double B);
extern double gsl_sf_lnbeta (double A, double B);
extern double gsl_sf_beta_inc (double A, double B, double X);

extern double gsl_sf_gegenpoly_1 (double LAMBDA, double X);
extern double gsl_sf_gegenpoly_2 (double LAMBDA, double X);
extern double gsl_sf_gegenpoly_3 (double LAMBDA, double X);

extern double gsl_sf_gegenpoly_n (int N, double LAMBDA, double X);
//extern int gsl_sf_gegenpoly_array (int NMAX, double LAMBDA, double X, double RESULT_ARRAY[]);

extern double gsl_sf_hyperg_0F1 (double C, double X);
extern double gsl_sf_hyperg_1F1_int (int M, int N, double X);
extern double gsl_sf_hyperg_1F1 (double A, double B, double X);
extern double gsl_sf_hyperg_U_int (int M, int N, double X);
extern double gsl_sf_hyperg_U (double A, double B, double X);
extern double gsl_sf_hyperg_2F1 (double A, double B, double C, double X);
extern double gsl_sf_hyperg_2F1_conj (double AR, double AI, double C, double X);
extern double gsl_sf_hyperg_2F1_renorm (double A, double B, double C, double X);
extern double gsl_sf_hyperg_2F1_conj_renorm (double AR, double AI, double C, double X);
extern double gsl_sf_hyperg_2F0 (double A, double B, double X);

extern double gsl_sf_laguerre_1 (double A, double X);
extern double gsl_sf_laguerre_2 (double A, double X);
extern double gsl_sf_laguerre_3 (double A, double X);
extern double gsl_sf_laguerre_n (const int N, const double A, const double X);

extern double gsl_sf_lambert_W0 (double X);
extern double gsl_sf_lambert_Wm1 (double X);

extern double gsl_sf_legendre_P1 (double X);
extern double gsl_sf_legendre_P2 (double X);
extern double gsl_sf_legendre_P3 (double X);

extern double gsl_sf_legendre_Pl (int L, double X);
//extern int gsl_sf_legendre_Pl_array (int LMAX, double X, double RESULT_ARRAY[]);
extern double gsl_sf_legendre_Q0 (double X);
extern double gsl_sf_legendre_Q1 (double X);
extern double gsl_sf_legendre_Ql (int L, double X);
extern double gsl_sf_legendre_Plm (int L, int M, double X);
//extern int gsl_sf_legendre_Plm_array (int LMAX, int M, double X, double RESULT_ARRAY[]);
extern double gsl_sf_legendre_sphPlm (int L, int M, double X);
//extern int gsl_sf_legendre_sphPlm_array (int LMAX, int M, double X, double RESULT_ARRAY[]);
//extern int gsl_sf_legendre_array_size (const int LMAX, const int M);

extern double gsl_sf_conicalP_half (double LAMBDA, double X);
extern double gsl_sf_conicalP_mhalf (double LAMBDA, double X);
extern double gsl_sf_conicalP_0 (double LAMBDA, double X);
extern double gsl_sf_conicalP_1 (double LAMBDA, double X);
extern double gsl_sf_conicalP_sph_reg (int L, double LAMBDA, double X);
extern double gsl_sf_conicalP_cyl_reg (int M, double LAMBDA, double X);

extern double gsl_sf_legendre_H3d_0 (double LAMBDA, double ETA);
extern double gsl_sf_legendre_H3d_1 (double LAMBDA, double ETA);
extern double gsl_sf_legendre_H3d (int L, double LAMBDA, double ETA);
//extern int gsl_sf_legendre_H3d_array (int LMAX, double LAMBDA, double ETA, double RESULT_ARRAY[]);

extern double gsl_sf_log (double X);
extern double gsl_sf_log_abs (double X);
extern double gsl_sf_log_1plusx (double X);
extern double gsl_sf_log_1plusx_mx (double X);

extern double gsl_sf_pow_int (double X, int N);

extern double gsl_sf_psi_int (int N);
extern double gsl_sf_psi (double X);
extern double gsl_sf_psi_1piy (double Y);
extern double gsl_sf_psi_1_int (int N);
extern double gsl_sf_psi_n (int M, double X);

extern double gsl_sf_synchrotron_1 (double X);
extern double gsl_sf_synchrotron_2 (double X);

extern double gsl_sf_transport_2 (double X);
extern double gsl_sf_transport_3 (double X);
extern double gsl_sf_transport_4 (double X);
extern double gsl_sf_transport_5 (double X);

extern double gsl_sf_sin (double X);
extern double gsl_sf_cos (double X);
extern double gsl_sf_hypot (double X, double Y);
extern double gsl_sf_sinc (double X);

/* Trigonometric Functions for Complex Arguments not included */

extern double gsl_sf_lnsinh (double X);
extern double gsl_sf_lncosh (double X);

/* Conversion functions not included */

extern double gsl_sf_angle_restrict_symm (double THETA);
extern double gsl_sf_angle_restrict_pos (double THETA);

extern double gsl_sf_zeta_int (int N);
extern double gsl_sf_zeta (double S);

extern double gsl_sf_hzeta (double S, double Q);

extern double gsl_sf_eta_int (int N);
extern double gsl_sf_eta (double S);



%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-special.la"))
(dynamic-call "SWIG_init" my-so)
%}
