%module "gsl/gsl-poly"

%{
#include <gsl/gsl_poly.h>
%}

typedef double * gsl_complex_packed_ptr;

extern double gsl_poly_eval (const double c[], const int LEN, const double X);
extern int gsl_poly_dd_init (double dd[], const double xa[], const double ya[], size_t SIZE);
extern double gsl_poly_dd_eval (const double dd[], const double xa[], const size_t SIZE, const double X);
extern int gsl_poly_dd_taylor (double c[], double XP, const double dd[], const double xa[], size_t SIZE, double w[]);
extern int gsl_poly_solve_quadratic (double A, double B, double C, double *X0, double *X1);
extern int gsl_poly_complex_solve_quadratic (double A, double B, double C, gsl_complex *Z0, gsl_complex *Z1);
extern int gsl_poly_solve_cubic (double A, double B, double C, double *X0, double *X1, double *X2);
extern int gsl_poly_complex_solve_cubic (double A, double B, double C, gsl_complex *Z0, gsl_complex *Z1, gsl_complex *Z2);
extern gsl_poly_complex_workspace * gsl_poly_complex_workspace_alloc (size_t N);
extern void gsl_poly_complex_workspace_free (gsl_poly_complex_workspace * W);
extern int gsl_poly_complex_solve (const double * A, size_t N, gsl_poly_complex_workspace * W, gsl_complex_packed_ptr Z);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-poly.so"))
(dynamic-call "SWIG_init" my-so)

(export real-poly-eval
	real-poly-solve)

(define (real-poly-eval v x)
  (if (not (real-vector? v))
      (error "V must be a real vector"))
  (let* ((len (uniform-vector-length v))
	 (v (vector->gsl-vector v))
	 (result (gsl-poly-eval (gsl-vector-data v) len x)))
    (gsl-vector-free v)
    result))

(define (real-poly-solve v)
  (if (not (real-vector? v))
      (error "V must be a real vector"))
  (let* ((len (uniform-vector-length v))
	 (workspace (gsl-poly-complex-workspace-alloc len))
	 (z (gsl-vector-complex-alloc len))
	 (v (vector->gsl-vector v))
	 (errno (gsl-poly-complex-solve (gsl-vector-data v)
					len workspace
					(gsl-vector-complex-data z)))
	 (result (gsl-vector-complex->vector z)))
    (gsl-poly-complex-workspace-free workspace)
    (gsl-vector-free v)
    (gsl-vector-complex-free z)
    (cond ((= errno 0) result)
	  (else (error (gsl-strerror errno))))))

%}
