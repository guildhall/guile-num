(use-modules (gsl gsl-integration))
(use-modules (gsl gsl-function))


(let* ((alpha 1.0)
       (f (lambda (x) (/ (log (* alpha x)) (sqrt x))))
       (w (gsl-integration-workspace-alloc 1000))
       (expected -4.0))
  (let ((gsl-f (gsl-function-alloc f)))
    (call-with-values (lambda ()
			(gsl-integration-qags gsl-f 0 1 0 1e-7 1000 w))
		      (lambda (errno result abserr)
			(display "result          = ")
			(display result)
			(newline)

			(display "exact result    = ")
			(display expected)
			(newline)

			(display "estimated error = ")
			(display abserr)
			(newline)

			(display "actual error    = ")
			(display (- result expected))
			(newline)))
    (gsl-function-free gsl-f)
    (gsl-integration-workspace-free w)))
     
     
     
