(use-modules (gsl gsl-errno))
(use-modules (gsl gsl-interp))
(use-modules (gsl gsl-vector))

(define len 10)

(let ((x (make-uniform-vector len 1.0))
      (y (make-uniform-vector len 1.0)))

  (display "#m=0,S=2\n")
  (do ((i 0 (+ i 1)))
      ((= i len))
    (let ((xi (+ i (* 0.5 (sin i))))
	  (yi (+ i (cos (* i i)))))
    (uniform-vector-set! x i xi)
    (uniform-vector-set! y i yi)
    (display xi) (display " ") (display yi) (newline)))

  (let ((acc (gsl-interp-accel-alloc))
	(spline (gsl-spline-alloc (gsl-interp-cspline) len))
	(gsl-x (vector->gsl-vector x))
	(gsl-y (vector->gsl-vector y)))
    (gsl-spline-init spline
		     (gsl-vector-data gsl-x)
		     (gsl-vector-data gsl-y)
		     len)

    (display "#m=1,S=0\n")
    (do ((xi (uniform-vector-ref x 0) (+ xi 0.01)))
	((> xi (uniform-vector-ref x (- len 1))))
      (display xi)
      (display " ")
      (display (gsl-spline-eval spline xi acc))
      (newline))

    (gsl-spline-free spline)
    (gsl-interp-accel-free acc)
    (gsl-vector-free gsl-x)
    (gsl-vector-free gsl-y)))
