(use-modules (gsl gsl-function))
(use-modules (gsl gsl-chebyshev))
(use-modules (ice-9 format))

(define (f x)
  (if (< x 0.5)
      0.25
      0.75))

(let ((n 10)
      (cs (gsl-cheb-alloc 40))
      (gsl-f (gsl-function-alloc f)))
  (gsl-cheb-init cs gsl-f 0.0 1.0)
  (do ((i 0 (+ i 1)))
      ((= i n))
    (let* ((x (/ i n))
	   (r10 (gsl-cheb-eval-n cs 10 x))
	   (r40 (gsl-cheb-eval cs x)))
      (format (current-output-port)
	      "~G ~G ~G ~G\n"
	      x (gsl-function-eval gsl-f x) r10 r40)))
  (gsl-cheb-free cs)
  (gsl-function-free gsl-f))
