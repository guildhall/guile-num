(use-modules (ice-9 format)
	     (math array-fun)
	     (gsl gsl-math)
	     (gsl gsl-vector)
	     (gsl gsl-rng)
	     (gsl gsl-monte))

;;; Computation of the integral,
;;;
;;;   I = int (dx dy dz)/(2pi)^3  1/(1-cos(x)cos(y)cos(z))
;;;
;;; over (-pi,-pi,-pi) to (+pi, +pi, +pi).  The exact answer is
;;; Gamma(1/4)^4/(4 pi^3).  This example is taken from C.Itzykson,
;;; J.M.Drouffe, "Statistical Field Theory - Volume 1", Section 1.1,
;;; p21, which cites the original paper M.L.Glasser, I.J.Zucker,
;;; Proc.Natl.Acad.Sci.USA 74 1800 (1977)

;;; This script runs approximately 15x slower than the compiled
;;; example in C from the GSL reference manual.
;;;
;;;   C compiled: real:  2.8 , user:  1.78
;;;   Guile     : real: 40.94, user: 26.60

(gsl-rng-env-setup)

(define pi (gsl-pi))

(define dims 3)

(define (display-results title exact result abserr)
  (format #t "~A ===========\n" title)
  (format #t "result = ~F\n" result)
  (format #t "sigma = ~F\n" abserr)
  (format #t "exact = ~F\n" exact)
  (format #t "error = ~F = ~F sigma\n"
	  (- result exact)
	  (abs (/ (- result exact) abserr))))

(define C
  (/ 1.0 (* pi pi pi)))
(define (func arg)
  (/ C (- 1.0 (* (cos (vector-ref arg 0))
		 (cos (vector-ref arg 1))
		 (cos (vector-ref arg 2))))))

(define x-lower #i(0.0 0.0 0.0))
(define x-upper (array-scale #i(1.0 1.0 1.0) pi))


(let ((f (gsl-monte-function-alloc func dims))
      (xl (vector->gsl-vector x-lower))
      (xu (vector->gsl-vector x-upper))
      (r (gsl-rng-alloc (gsl-rng-default)))
      (exact 1.393203929685676859184246260325))
  (let ((s (gsl-monte-plain-alloc dims))
	(calls 500000))
    (call-with-values
     (lambda ()
       (gsl-monte-plain-integrate f
				  (gsl-vector-data xl) (gsl-vector-data xu)
				  dims calls r s))
     (lambda (errno result abserr)
       (display-results "plain" exact result abserr)))
    (gsl-monte-plain-free s))

  (let ((s (gsl-monte-miser-alloc dims))
	(calls 500000))
    (call-with-values
     (lambda ()
       (gsl-monte-miser-integrate f
				  (gsl-vector-data xl) (gsl-vector-data xu)
				  dims calls r s))
     (lambda (errno result abserr)
       (display-results "miser" exact result abserr)))
    (gsl-monte-miser-free s))

  (let ((s (gsl-monte-vegas-alloc dims))
	(calls 10000))
    (call-with-values
     (lambda ()
       (gsl-monte-vegas-integrate f
				  (gsl-vector-data xl) (gsl-vector-data xu)
				  dims calls r s))
     (lambda (errno result abserr)
       (display-results "vegas warm-up" exact result abserr)))
    
    (display "converging...\n")
    (let loop ()
      (call-with-values
       (lambda ()
	 (gsl-monte-vegas-integrate f
				    (gsl-vector-data xl)
				    (gsl-vector-data xu)
				    dims (* 10 calls) r s))
       (lambda (errno result abserr)
	 (format #t "result = ~A\nsigma = ~A\nchisq/dof = ~A\n"
		 result
		 abserr
		 (gsl-monte-vegas-state-chisq-get s))
	 (cond ((< (abs (- (gsl-monte-vegas-state-chisq-get s)
			   1.0)) 0.5)
		(display-results "vegas final"
				 exact result abserr))
	       (else (loop))))))
    (gsl-monte-vegas-free s))

  (gsl-monte-function-free f)
  (gsl-vector-free xl)
  (gsl-vector-free xu)
  (gsl-rng-free r))
