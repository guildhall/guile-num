;;; Guile interface to LAPACK
;;; Copyright (C) 2002   Arno W. Peters <a.w.peters@ieee.org>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;; USA

(define-module (lapack lapack)
  :use-module (math array-fun)
  :use-module (gsl gsl)
  :use-module (gsl gsl-vector)
  :use-module (gsl gsl-matrix)
  :use-module (lapack guile-lapack))

(export dgesv
	zgesv
	dgels
	zgels)

(define (lapack-permutation-vector->matrix v)
  (let* ((len (uniform-vector-length v))
	 (P   (gsl-matrix-int-alloc len len)))
    (gsl-matrix-int-set-identity P)
    (do ((i 0 (+ i 1)))
	((= i len) (gsl-matrix-int->matrix P))
      (gsl-matrix-int-swap-columns P i (uniform-vector-ref v i)))))


(define (dgesv A B)
  (let* ((n (array-rows A))
	 (nrhs (array-columns B))
	 (lda n)
	 (ldb n)
	 (IPIV (make-uniform-array 1 n))
	 (a (matrix->gsl-matrix (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix (transpose-array B 1 0)))
	 (ipiv (vector->gsl-vector-int IPIV))
	 (errno (gsl-dgesv n nrhs
			   (gsl-matrix-ptr a 0 0) lda
			   (gsl-vector-int-data ipiv)
			   (gsl-matrix-ptr b 0 0) ldb)))
    (let ((P (lapack-permutation-vector->matrix (gsl-vector-int->vector ipiv)))
	  (LU (transpose-array (gsl-matrix->matrix a) 1 0))
	  (X (transpose-array (gsl-matrix->matrix b) 1 0)))
      (gsl-matrix-free a)
      (gsl-matrix-free b)
      (gsl-vector-int-free ipiv)
      (cond ((= errno 0) (values X LU P))
	    ((< errno 0) (error "one of the arguments had an illegal value"))
	    (else
	     (error "factor U is singular, no solution can be computed"))))))

(define (zgesv A B)
  (let* ((n (array-rows A))
	 (nrhs (array-columns B))
	 (lda n)
	 (ldb n)
	 (IPIV (make-uniform-array 1 n))
	 (a (matrix->gsl-matrix-complex (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix-complex (transpose-array B 1 0)))
	 (ipiv (vector->gsl-vector-int IPIV))
	 (errno (gsl-zgesv n nrhs
			   (gsl-matrix-complex-ptr a 0 0) lda
			   (gsl-vector-int-data ipiv)
			   (gsl-matrix-complex-ptr b 0 0) ldb)))
    (let ((P (lapack-permutation-vector->matrix (gsl-vector-int->vector ipiv)))
	  (LU (transpose-array (gsl-matrix-complex->matrix a) 1 0))
	  (X (transpose-array (gsl-matrix-complex->matrix b) 1 0)))
      (gsl-matrix-complex-free a)
      (gsl-matrix-complex-free b)
      (gsl-vector-int-free ipiv)
      (cond ((= errno 0) (values X LU P))
	    ((< errno 0) (error "one of the arguments had an illegal value"))
	    (else
	     (error "factor U is singular, no solution can be computed"))))))

(define (dgels trans A B)
  (let* ((a (matrix->gsl-matrix (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix (transpose-array B 1 0)))
	 (errno (gsl-dgels trans a b)))
    (let ((LQ (transpose-array (gsl-matrix->matrix a) 1 0))
	  (X  (transpose-array (gsl-matrix->matrix b) 1 0)))
      (gsl-matrix-free a)
      (gsl-matrix-free b)
      (cond ((= errno 0) (values X LQ))
	    (else (error "one of the arguments had an illegal value"))))))

(define (zgels trans A B)
  (let* ((a (matrix->gsl-matrix-complex (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix-complex (transpose-array B 1 0)))
	 (errno (gsl-zgels trans a b)))
    (let ((LQ (transpose-array (gsl-matrix-complex->matrix a) 1 0))
	  (X  (transpose-array (gsl-matrix-complex->matrix b) 1 0)))
      (gsl-matrix-complex-free a)
      (gsl-matrix-complex-free b)
      (cond ((= errno 0) (values X LQ))
	    (else (error "one of the arguments had an illegal value"))))))
