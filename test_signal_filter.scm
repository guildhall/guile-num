(use-modules (signal filter))

(define (assert-equal? x y)
  (if (equal? x y)
      (display "PASSED\n")
      (display "FAILED\n")))

(display "Testing filtering\n")
(let ((num #(1 2 3))
      (den #(4))
      (x #(1 0 0 0 0 0)))
  (array-map! x (make-fixed-iir-filter num den) x)
  (assert-equal? x
		 #(0.25 0.5 0.75 0.0 0.0 0.0)))

(let ((num #(1))
      (den #(1 0.5))
      (x #(1 0 0 0 0 0)))
  (array-map! x (make-fixed-iir-filter num den) x)
  (assert-equal? x
		 #(1.0 -0.5 0.25 -0.125 0.0625 -0.03125)))

(let ((num #(1))
      (den #(1 0.5))
      (x #(1 0 0 0 0 0)))
  (call-with-values (lambda () (iir-filter num den x))
		    (lambda (y state)
		      (assert-equal? y
				     #(1.0 -0.5 0.25 -0.125 0.0625 -0.03125)))))
