(use-modules (gsl gsl-rng))

;;; (do ((i 0 (+ i 1)))
;;;     ((= i (gsl-rng-type-len)))
;;;   (display (gsl-rng-type-name (gsl-rng-type-ref i))) (newline))

(gsl-rng-env-setup)
(let* ((T (gsl-rng-default))
       (r (gsl-rng-alloc T)))
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (display (gsl-rng-uniform r)) (newline))
  (gsl-rng-free r))
    
