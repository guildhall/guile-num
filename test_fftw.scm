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
	      (gsl-vector-complex-data in)
	      (gsl-vector-complex-data out))
    (let ((result (gsl-complex-vector->vector out)))
      (fftw-destroy-plan plan)
      (gsl-vector-complex-free in)
      (gsl-vector-complex-free out)
      result)))

;; This test will generate the following error message, due to a
;; mismatch in type between the output of (gsl-complex-vector-data .) 
;; that is (double *) and the expected input type of fftw-one:
;; (fftw-complex *).  The conversion routines need to be written.
;;
;; ERROR: In procedure fftw-one:
;; ERROR: Wrong type argument in position 2: #<swig double * 80565e0>

(let ((a #(1 0+i -1 0-i 1 0+i -1 0-i)))
  (cout (my-fftw-one a)))

