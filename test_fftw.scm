(use-modules (fftw fftw)
	     (gsl gsl-vector)
	     (math array-fun)
	     (ice-9 format))

(define (cout . args)
  (apply format (current-output-port) args))

(define (my-fftw-one vct)
  (let ((in (vector->gsl-vector-complex vct))
	(out (vector->gsl-vector-complex (make-vector (vector-length vct) 0)))
	(plan  (fftw-create-plan (vector-length vct)
				 (FFTW-FORWARD)
				 (FFTW-ESTIMATE))))
    (fftw-one plan
	      (gsl-vector-complex-fftw-complex in)
	      (gsl-vector-complex-fftw-complex out))
    (let ((result (gsl-vector-complex->vector out)))
      (fftw-destroy-plan plan)
      (gsl-vector-complex-free in)
      (gsl-vector-complex-free out)
      result)))

(let ((a #(1 0+i -1 0-i 1 0+i -1 0-i)))
  (if (equal? (my-fftw-one a)
	      #c(0.0 0.0 8.0 0.0 0.0 0.0 0.0 0.0))
      (display "PASSED\n")
      (display "FAILED\n")))

