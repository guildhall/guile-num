(use-modules (gsl blas)
	     (ice-9 format))


(let ((alpha 1.0)
      (A #2i((0.11 0.12 0.13)
	     (0.21 0.22 0.23)))
      (B #2i((1011.0 1012.0)
	     (1021.0 1022.0)
	     (1031.0 1032.0)))
      (beta 0.0)
      (C #2i((0.0 0.0)
	     (0.0 0.0))))
  (format #t "blas-dgemm: ~A\n"
	  (blas-dgemm 'no-transpose 'no-transpose alpha A B beta C)))

(let ((alpha 1.0+0i)
      (A #2c((0.11   0.12 0.13)
	     (0.21+i 0.22 0.23)))
      (B #2c((1011.0 1012.0)
	     (1021.0 1022.0+i)
	     (1031.0 1032.0)))
      (beta 0.0+0i)
      (C #2c((0.0 0.0)
	     (0.0 0.0))))
  (format #t "blas-zgemm: ~A\n"
	  (blas-zgemm 'no-transpose 'no-transpose alpha A B beta C)))

