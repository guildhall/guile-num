(use-modules (gsl errno)
	     (gsl vector)
	     (gsl matrix)
	     (gsl multimin)
	     (ice-9 format))

(define (paraboloid a b)
  (lambda (x)
    (let ((x0 (gsl-vector-get x 0))
	  (x1 (gsl-vector-get x 1)))
      (+ (* 10 (- x0 a) (- x0 a))
	 (* 20 (- x1 b) (- x1 b))
	 30))))

(define (paraboloid-df a b)
  (lambda (y x)
    (let ((x0 (gsl-vector-get x 0))
	  (x1 (gsl-vector-get x 1)))
      (gsl-vector-set y 0 (* 20 (- x0 a)))
      (gsl-vector-set y 1 (* 40 (- x1 b))))))

(let ((f (paraboloid 1.0 2.0))
      (df (paraboloid-df 1.0 2.0))
      (x-init #(5.0 7.0))
      (dim 2))
  (let ((s (gsl-multimin-fdfminimizer-alloc
	    (gsl-multimin-fdfminimizer-conjugate-fr)
	    dim))
	(F (gsl-multimin-function-fdf-alloc f df dim))
	(x (vector->gsl-vector x-init)))

    (format #t "using ~A method\n" (gsl-multimin-fdfminimizer-name s))

    (gsl-multimin-fdfminimizer-set s F x 0.01 1e-4)
    (let loop ((iter 1))
      (gsl-multimin-fdfminimizer-iterate s)

      (let ((status (gsl-multimin-test-gradient 
		     (gsl-multimin-fdfminimizer-gradient-get s)
		     1e-3)))
	(if (= status (GSL-SUCCESS))
	    (format #t "Minimum found at:\n"))

	(format #t
		"~5D ~5F ~5F ~10F\n"
		iter
		(gsl-vector-get (gsl-multimin-fdfminimizer-x-get s) 0)
		(gsl-vector-get (gsl-multimin-fdfminimizer-x-get s) 1)
		(gsl-multimin-fdfminimizer-f-get s))

	(if (and (= status (GSL-CONTINUE))
		 (< iter 100))
	    (loop (+ iter 1)))))

    (gsl-multimin-fdfminimizer-free s)
    (gsl-multimin-function-fdf-free F)
    (gsl-vector-free x)))
