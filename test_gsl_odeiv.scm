(use-modules (gsl gsl-odeiv))
(use-modules (gsl gsl-matrix))

(define uref uniform-vector-ref)
(define (list->vct lst)
  (list->uniform-vector 1.0 lst))
(define (list->ary dim lst)
  (list->uniform-array dim 1.0 lst))

(define mu 10.0)
(define dims 2)

(define (func t z)
  (let ((x (uref z 0))
	(y (uref z 1)))
    (list->vct (list x
		     (- 0.0 x (* mu y (- (* x x) 1.0)))))))

(define (jacobian t z)
  (let ((x (uref z 0))
	(y (uref z 1)))
    (list (list->vct 0.0 0.0)
	  (list->ary 2 (list (list 0.0 1.0)
			     (list (- 0.0 (* 2.0 mu x y) 1.0)
				   (- (* mu (- (* x x) 1.0)))))))))

(let ((s (gsl-odeiv-step-alloc (gsl-odeiv-step-rk8pd) dims))
      (c (gsl-odeiv-control-y-new 1e-6 0.0))
      (e (gsl-odeiv-evolve-alloc dims))
      (sys (gsl-odeiv-system-alloc func jacobian dims)))
  (let loop ()
    
