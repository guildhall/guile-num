%module "gsl/poly"

%{
#include <gsl/gsl_poly.h>
%}

%include "gsl_poly_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-poly.la"))
(dynamic-call "scm_init_gsl_poly_module" my-so)

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
