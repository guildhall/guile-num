(use-modules (fftw fftw)
	     (gsl vector)
	     (math array-fun)
	     (ice-9 format))

(define (cout . args)
  (apply format (current-output-port) args))

(define (my-fftw-one vct)
  (let ((in (vector->gsl-vector-complex vct))
	(out (vector->gsl-vector-complex (make-vector (vector-length vct) 0))))
    (let ((fftw-in (fftw-vector-alloc (vector-length vct)))
	  (fftw-out (fftw-vector-alloc (vector-length vct))))
      (let ((plan (fftw-plan-dft-1d (vector-length vct)
				    fftw-in fftw-out
				    (FFTW-FORWARD)
				    (FFTW-ESTIMATE))))
	(copy-gsl-vector-complex-fftw-complex fftw-in in)
	(fftw-execute plan)
	(fftw-destroy-plan plan))
      (copy-fftw-complex-gsl-vector-complex out fftw-out)
      (fftw-free fftw-in)
      (fftw-free fftw-out)

      (let ((result (gsl-vector-complex->vector out)))
	(gsl-vector-complex-free in)
	(gsl-vector-complex-free out)
	result))))

(let ((a #(1 0+i -1 0-i 1 0+i -1 0-i)))
  (let ((result (my-fftw-one a))
	(expect #c(0.0 0.0 8.0 0.0 0.0 0.0 0.0 0.0)))
    (cond ((equal? expect result)
	   (display "PASSED\n"))
	  (else
	   (display result) (newline)
	   (display expect) (newline)
	   (display "FAILED\n")))))

