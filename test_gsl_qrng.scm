(use-modules (gsl qrng))
(use-modules (gsl vector))

(let ((q (gsl-qrng-alloc (gsl-qrng-sobol) 2))
      (v (gsl-vector-alloc 2)))
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (gsl-qrng-get q (gsl-vector-data v))
    (display (gsl-vector-get v 0))
    (display " ")
    (display (gsl-vector-get v 1))
    (newline))
  (gsl-vector-free v)
  (gsl-qrng-free q))

#!
Results:
0.5 0.5
0.75 0.25
0.25 0.75
0.375 0.375
0.875 0.875
0.625 0.125
0.125 0.625
0.1875 0.3125
0.6875 0.8125
0.9375 0.0625
!#
