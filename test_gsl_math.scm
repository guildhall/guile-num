(use-modules (gsl math))

(let ((z (gsl-complex-alloc)))
  (gsl-set-complex z 3.0 4.0)
  (display (= 5.0 (gsl-complex-abs z)))
  (newline)

  (gsl-complex-free z))
