(use-modules (gsl gsl))
(use-modules (gsl gsl-errno))
(use-modules (gsl gsl-vector))
(use-modules (gsl gsl-matrix))
(use-modules (gsl gsl-multiroot-function))
(use-modules (gsl gsl-multiroots))
(use-modules (ice-9 format))

(define (cout . args)
  (apply format (current-output-port) args))

(define (rosenbrock a b)
  (lambda (x)
    (let ((x0 (vector-ref x 0))
	  (x1 (vector-ref x 1)))
;;;      (display "x0: ") (display x0) (newline)
;;;      (display "x1: ") (display x1) (newline)
      (list->vector (list (* a (- 1 x0))
			  (* b (- x1 (* x0 x0))))))))

(define (print-state iter s)
  (cout "iter = ~3D x = ~7F ~7F f(x) = ~7E ~7E\n"
	iter
	(gsl-vector-get (gsl-multiroot-fsolver-x-get s) 0)
	(gsl-vector-get (gsl-multiroot-fsolver-x-get s) 1)
	(gsl-vector-get (gsl-multiroot-fsolver-f-get s) 0)
	(gsl-vector-get (gsl-multiroot-fsolver-f-get s) 1)))

(let ((f (rosenbrock 1.0 10.0))
      (x-init #(-10.0 -5.0))
      (dim 2))
  (let ((s (gsl-multiroot-fsolver-alloc (gsl-multiroot-fsolver-hybrids) dim))
	(F (gsl-multiroot-function-alloc f dim))
	(x (vector->gsl-vector x-init))
	(y (vector->gsl-vector #(0.0 0.0))))

    (cout "using ~A method\n" (gsl-multiroot-fsolver-name s))

    (display "x: ") (display (gsl-vector->vector x)) (newline)
    (gsl-multiroot-function-eval F x y)
    (display "y: ") (display (gsl-vector->vector y)) (newline)
    
    (gsl-multiroot-fsolver-set s F x)
    (let loop ((iter 0)
	       (status (GSL-CONTINUE)))
      (print-state iter s)

      (let ((status (gsl-multiroot-test-residual (gsl-multiroot-fsolver-f s)
						 1E-7)))
	(if (and (= status (GSL-CONTINUE))
		 (< iter 1000))
	    (loop (+ iter 1) (gsl-multiroot-fsolver-iterate s)))))

    (gsl-multiroot-fsolver-free s)
    (gsl-vector-free x)
    (gsl-vector-free y)
))


(define (rosenbrock a b)
  (lambda (y x)
    (let ((x0 (gsl-vector-get x 0))
	  (x1 (gsl-vector-get x 1)))
      (gsl-vector-set y 0 (* a (- 1 x0)))
      (gsl-vector-set y 1 (* b (- x1 (* x0 x0)))))))

(define (rosenbrock-df a b)
  (lambda (J x)
    (let ((x0 (gsl-vector-get x 0))
	  (x1 (gsl-vector-get x 1)))
      (gsl-matrix-set J 0 0 (- a))
      (gsl-matrix-set J 0 1 0)
      (gsl-matrix-set J 1 0 (* -2 b x0))
      (gsl-matrix-set J 1 1 b))))

(define (print-fdf-state iter s)
  (cout "iter = ~3D x = ~7F ~7F f(x) = ~7E ~7E\n"
	iter
	(gsl-vector-get (gsl-multiroot-fdfsolver-x-get s) 0)
	(gsl-vector-get (gsl-multiroot-fdfsolver-x-get s) 1)
	(gsl-vector-get (gsl-multiroot-fdfsolver-f-get s) 0)
	(gsl-vector-get (gsl-multiroot-fdfsolver-f-get s) 1)))

(let ((f (rosenbrock 1.0 10.0))
      (df (rosenbrock-df 1.0 10.0))
      (x-init #(-10.0 -5.0))
      (J-init #2((0.0 0.0) (0.0 0.0)))
      (dim 2))
  (let ((s (gsl-multiroot-fdfsolver-alloc (gsl-multiroot-fdfsolver-gnewton)
					  dim))
	(F (gsl-multiroot-function-fdf-alloc f df dim))
	(x (vector->gsl-vector x-init))
	(J (matrix->gsl-matrix J-init))
	(y (vector->gsl-vector #(0.0 0.0))))

    (cout "using ~S method\n" (gsl-multiroot-fdfsolver-name s))

    (display "x: ") (display (gsl-vector->vector x)) (newline)
    (gsl-multiroot-function-eval-f F x y)
    (display "y: ") (display (gsl-vector->vector y)) (newline)
    (display "x: ") (display (gsl-vector->vector x)) (newline)
    (gsl-multiroot-function-eval-df F x J)
    (display "J: ") (display (gsl-matrix->matrix J)) (newline)
    
    (gsl-multiroot-fdfsolver-set s F x)
    (let loop ((iter 0)
	       (status (GSL-CONTINUE)))
      (print-fdf-state iter s)

      (let ((status (gsl-multiroot-test-residual (gsl-multiroot-fdfsolver-f s)
						 1E-7)))
	(if (and (= status (GSL-CONTINUE))
		 (< iter 1000))
	    (loop (+ iter 1) (gsl-multiroot-fdfsolver-iterate s)))))

    (gsl-multiroot-fdfsolver-free s)
    (gsl-vector-free x)
    (gsl-vector-free y)
))
