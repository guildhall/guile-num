%module "gsl/gsl-math"

%{
#include <gsl/gsl_math.h>
#include <gsl/gsl_complex_math.h>

double gsl_pi(void)
{
	return M_PI;
}

double gsl_e(void)
{
	return M_E;
}

double gsl_euler(void)
{
	return M_EULER;
}

double gsl_posinf(void)
{
	return GSL_POSINF;
}

double gsl_neginf(void)
{
	return GSL_NEGINF;
}

double gsl_nan(void)
{
	return GSL_NAN;
}

gsl_complex * gsl_complex_alloc (void)
{
	gsl_complex * Z;

	Z = (gsl_complex *) malloc(sizeof(gsl_complex));
	return Z;
}

void gsl_complex_free (gsl_complex * Z)
{
	free (Z);
}

void gsl_complex_copy (gsl_complex *target, gsl_complex *source)
{
	memcpy(target, source, sizeof(gsl_complex));
}

double gsl_real(gsl_complex * z)
{
	return GSL_REAL(*z);
}

double gsl_imag(gsl_complex * z)
{
	return GSL_IMAG(*z);
}

void gsl_set_complex(gsl_complex * zp, double x, double y)
{
	GSL_SET_COMPLEX(zp, x, y);
}

void gsl_set_real(gsl_complex * zp, double x)
{
	GSL_SET_REAL(zp, x);
}

void gsl_set_imag(gsl_complex * zp, double y)
{
	GSL_SET_IMAG(zp, y);
}

%}

double gsl_pi(void);
double gsl_e(void);
double gsl_euler(void);
double gsl_posinf(void);
double gsl_neginf(void);
double gsl_nan(void);

double gsl_log1p (const double X);
double gsl_expm1 (const double X);
double gsl_hypot (const double X, const double Y);
double gsl_acosh (const double X);
double gsl_asinh (const double X);
double gsl_atanh (const double X);
double gsl_pow_int (double X, int N);

gsl_complex * gsl_complex_alloc (void);
void gsl_complex_free (gsl_complex * Z);
void gsl_complex_copy (gsl_complex * TARGET, gsl_complex * SOURCE);

extern double gsl_real (gsl_complex * Z);
extern double gsl_imag (gsl_complex * Z);
extern void gsl_set_complex (gsl_complex * ZP, double X, double Y);
extern void gsl_set_real (gsl_complex * ZP, double X);
extern void gsl_set_imag (gsl_complex * ZP, double Y);

extern gsl_complex gsl_complex_rect (double X, double Y);
extern gsl_complex gsl_complex_polar (double R, double THETA);

extern double gsl_complex_arg (gsl_complex Z);
extern double gsl_complex_abs (gsl_complex Z);
extern double gsl_complex_abs2 (gsl_complex Z);
extern double gsl_complex_logabs (gsl_complex Z);

extern gsl_complex gsl_complex_add (gsl_complex A, gsl_complex B);
extern gsl_complex gsl_complex_sub (gsl_complex A, gsl_complex B);
extern gsl_complex gsl_complex_mul (gsl_complex A, gsl_complex B);
extern gsl_complex gsl_complex_div (gsl_complex A, gsl_complex B);
extern gsl_complex gsl_complex_add_real (gsl_complex A, double X);
extern gsl_complex gsl_complex_sub_real (gsl_complex A, double X);
extern gsl_complex gsl_complex_mul_real (gsl_complex A, double X);
extern gsl_complex gsl_complex_div_real (gsl_complex A, double X);
extern gsl_complex gsl_complex_add_imag (gsl_complex A, double Y);
extern gsl_complex gsl_complex_sub_imag (gsl_complex A, double Y);
extern gsl_complex gsl_complex_mul_imag (gsl_complex A, double Y);
extern gsl_complex gsl_complex_div_imag (gsl_complex A, double Y);
extern gsl_complex gsl_complex_conjugate (gsl_complex Z);
extern gsl_complex gsl_complex_inverse (gsl_complex Z);
extern gsl_complex gsl_complex_negative (gsl_complex Z);

extern gsl_complex gsl_complex_sqrt (gsl_complex Z);
extern gsl_complex gsl_complex_sqrt_real (double x);
extern gsl_complex gsl_complex_pow (gsl_complex Z, gsl_complex A);
extern gsl_complex gsl_complex_pow_real (gsl_complex Z, double X);
extern gsl_complex gsl_complex_exp (gsl_complex Z);
extern gsl_complex gsl_complex_log (gsl_complex Z);
extern gsl_complex gsl_complex_log10 (gsl_complex Z);
extern gsl_complex gsl_complex_log_b (gsl_complex Z, gsl_complex B) ;

extern gsl_complex gsl_complex_sin (gsl_complex Z);
extern gsl_complex gsl_complex_cos (gsl_complex Z);
extern gsl_complex gsl_complex_tan (gsl_complex Z);
extern gsl_complex gsl_complex_sec (gsl_complex Z);
extern gsl_complex gsl_complex_csc (gsl_complex Z);
extern gsl_complex gsl_complex_cot (gsl_complex Z);

extern gsl_complex gsl_complex_arcsin (gsl_complex Z);
extern gsl_complex gsl_complex_arcsin_real (double Z);
extern gsl_complex gsl_complex_arccos (gsl_complex Z);
extern gsl_complex gsl_complex_arccos_real (double Z);
extern gsl_complex gsl_complex_arctan (gsl_complex Z);
extern gsl_complex gsl_complex_arcsec (gsl_complex Z);
extern gsl_complex gsl_complex_arcsec_real (double Z);
extern gsl_complex gsl_complex_arccsc (gsl_complex Z);
extern gsl_complex gsl_complex_arccsc_real (double Z);
extern gsl_complex gsl_complex_arccot (gsl_complex Z);

extern gsl_complex gsl_complex_sinh (gsl_complex Z);
extern gsl_complex gsl_complex_cosh (gsl_complex Z);
extern gsl_complex gsl_complex_tanh (gsl_complex Z);
extern gsl_complex gsl_complex_sech (gsl_complex Z);
extern gsl_complex gsl_complex_csch (gsl_complex Z);
extern gsl_complex gsl_complex_coth (gsl_complex Z);

extern gsl_complex gsl_complex_arcsinh (gsl_complex Z);
extern gsl_complex gsl_complex_arccosh (gsl_complex Z);
extern gsl_complex gsl_complex_arccosh_real (double Z);
extern gsl_complex gsl_complex_arctanh (gsl_complex Z);
extern gsl_complex gsl_complex_arctanh_real (double Z);
extern gsl_complex gsl_complex_arcsech (gsl_complex Z);
extern gsl_complex gsl_complex_arccsch (gsl_complex Z);
extern gsl_complex gsl_complex_arccoth (gsl_complex Z);
