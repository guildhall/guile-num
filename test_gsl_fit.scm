(use-modules (gsl gsl))
(use-modules (gsl gsl-vector))
(use-modules (gsl gsl-fit))
(use-modules (ice-9 format))

(define (cout . args)
  (apply format (current-output-port) args))

(let ((x (vector->gsl-vector #s(1970 1980 1990 2000)))
      (y (vector->gsl-vector #s(  12   11   14   13)))
      (w (vector->gsl-vector #i( 0.1  0.2  0.3  0.4)))
      (n 4))
  (call-with-values
   (lambda ()
     (gsl-fit-wlinear (gsl-vector-data x) 1
		      (gsl-vector-data w) 1
		      (gsl-vector-data y) 1
		      n))
   (lambda (errno c0 c1 cov00 cov01 cov11 chisq)
     (cout "# Plot in Gnuplot: plot 'datafile' with errorbars\n")
     (cout "#\n")
     (cout "# best fit: Y = ~G + ~G X\n" c0 c1)
     (cout "# covariance matrix:\n")
     (cout "# [ ~G ~G\n#   ~G ~G]\n" cov00 cov01 cov01 cov11)
     (cout "# chisq = ~G\n" chisq)
     (cout "#\n")
     
     (do ((i 0 (+ i 1)))
	 ((= i n))
       (cout "# data: ~G ~G ~G\n"
	     (gsl-vector-get x i)
	     (gsl-vector-get y i)
	     (/ 1 (sqrt (gsl-vector-get w i)))))
     (cout "\n")

     (cout "# xf fit hi lo\n")
     (do ((i -30 (+ i 1)))
	 ((= i 130))
       (let ((xf (+ (gsl-vector-get x 0)
		    (* (/ i 100) (- (gsl-vector-get x (- n 1))
				    (gsl-vector-get x 0))))))
	 (call-with-values
	  (lambda ()
	    (gsl-fit-linear-est xf c0 c1 cov00 cov01 cov11))
	  (lambda (errno yf yf_err)
	    (cout "~G ~G ~G ~G\n" xf yf (+ yf yf_err) (- yf yf_err))))))))

  (gsl-vector-free x)
  (gsl-vector-free y)
  (gsl-vector-free w))
