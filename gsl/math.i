%module "gsl/math"

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

gsl_complex * gsl_complex_alloc (void);
void gsl_complex_free (gsl_complex * Z);
void gsl_complex_copy (gsl_complex * TARGET, gsl_complex * SOURCE);

double gsl_real (gsl_complex * Z);
double gsl_imag (gsl_complex * Z);
void gsl_set_complex (gsl_complex * ZP, double X, double Y);
void gsl_set_real (gsl_complex * ZP, double X);
void gsl_set_imag (gsl_complex * ZP, double Y);

%include "gsl_math_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-math.la"))
(dynamic-call "scm_init_gsl_math_module" my-so)

(export complex->gsl-complex gsl-complex->complex)

;;; Complex number conversion functions
(define (complex->gsl-complex x)
  (let ((z (gsl-complex-alloc)))
    (gsl-set-complex z (real-part x) (imag-part x))))

(define (gsl-complex->complex z)
  (+ (gsl-real z) (* +i (gsl-imag z))))
%}
