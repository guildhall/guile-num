;;; Array functions for Guile
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

(define-module (math array-fun))

(export array-copy
	array-apply-rows
	array-map
	uniform-array-map
	vector-resize
	array-order
	array-reduce-order
	ensure-array-order
	ensure-vector
	ensure-matrix
	array-prototype-from-contents
	ensure-real-array
	ensure-complex-array
	array->uniform-array
	real-vector?
	real-matrix?
	array-rows
	array-columns
	array-add-constant
	array-scale
	array-add-elements
	array-subtract-elements
	array-multiply-elements
	array-divide-elements)

(define (array-copy array)
  "Returns a copy of @var{array}."
  (let ((result (apply make-array #f (array-shape array))))
    (array-copy! array result)
    result))

(define (array-apply-rows func array)
  "Maps a function over the rows of an array; the resulting array has
one less dimension than the original."
  (define (apply-rows func dim arg)
    (case dim
      ((0) (error "need at least one dimension"))
      ((1) (apply func arg))
      (else
       (list->array (-1 dim)
		    (map (lambda (x) (apply-rows func (-1 dim) x)) arg)))))

  (let ((dims (array-order array)))
    (apply-rows func dims (array->list array))))


;;; array-map : func array1 array2 ... -> array
;;;
(define (array-map func . arrays)
  "apply FUNC to the lists formed by the elementwise concatenation of
ARRAY1, ARRAY2, ... the results are stored at the same position in
ARRAY.  This function is useful for element-wise operations on multiple
arrays."
  (let ((v (apply make-array #f (array-shape (car arrays)))))
    (apply array-map! v func arrays)
    v))

(define (uniform-array-map proto func . arrays)
  "apply FUNC to the lists formed by the elementwise concatenation of
ARRAY1, ARRAY2, ... the results are stored at the same position in
ARRAY.  This function is useful for element-wise operations on multiple
arrays."
  (let ((v (apply make-uniform-array proto (array-shape (car arrays)))))
    (apply array-map! v func arrays)
    v))


(define (vector-resize vct len init)
  (let ((result (make-array init len)))
    (do ((idx 0 (+ idx 1)))
	((= idx (min (uniform-vector-length vct) len)) result)
      (array-set! result (uniform-vector-ref vct idx) idx))))

(define (array-order v)
  (cond ((number? v) 0)
	((vector? v) 1)
	((array? v) (length (array-dimensions v)))
	(else #f)))

(define (array-reduce-order v)
  (let ((dims (array-dimensions v)))
    (cond ((and (= (length dims) 1)
		(= (car dims) 1))
	   (vector-ref v 0))
	  ((= (car dims) 1)
	   (array-reduce-order (list->uniform-array (-1 (array-order v))
						    (array-prototype v)
						    (car (array->list v)))))
	  (else v))))

(define (ensure-array-order n v)
  "Forces an array to have n dimensions.  It is an error if an array has
more dimensions."
  (define (wrap-list n lst)
    (cond ((< n 0)
	   (error "the array has too many dimensions"))
	  ((= n 0) lst)
	  (else (list (wrap-list (-1 n) lst)))))
  (cond ((= (array-order v) 0)
	 (list->uniform-array n (type->prototype v) (wrap-list n v)))
	((= n (array-order v))
	 (array->uniform-array v))
	(else
	 (list->uniform-array n (array-prototype-from-contents v)
			      (wrap-list (- n (array-order v))
					 (array->list v))))))
(define (ensure-vector v) (ensure-array-order 1 v))
(define (ensure-matrix m) (ensure-array-order 2 m))

(define (type->rank x)
  (cond ((boolean? x) 0)
	((char? x) 1)
	((integer? x) 6)
	((real? x) 8)
	((complex? x) 9)))

(define (rank->prototype x)
  (case x
    ((0) #t)
    ((1) #\a)
    ((2) #\nul)
    ((3) 's)
    ((4) 1)
    ((5) -1)
    ((6) 'l)
    ((7) 1.0)
    ((8) 1/3)
    ((9) 0+i)))

(define (type->prototype x)
  (rank->prototype (type->rank x)))

(define (array-prototype-from-contents v)
  (let ((p (array-contents v)))
    (rank->prototype (array-apply-rows max (array-map type->rank p)))))

(define (ensure-real-array v)
  (if (equal? (array-prototype v) 1/3)
      v
      (uniform-array-map 1/3 (lambda (x) (+ x 0.0)) v)))

(define (ensure-complex-array v)
  (cond ((equal? (array-prototype v) 0+i) v)
	((equal? (array-prototype v) #t)
	 (uniform-array-map 0+i (lambda (x) (if x 1.0+0i 0.0+0i)) v))
	(else
	 (uniform-array-map 0+i (lambda (x) (+ x 0+0i)) v))))

(define (array->uniform-array v)
  (cond ((null? (array-prototype v))
	 (let ((proto (array-prototype-from-contents v)))
	   (if (equal? proto 0+i)
	       (ensure-complex-array v)
	       (list->uniform-array (array-order v) proto (array->list v)))))
	(else v)))

(define (real-vector? x)
  (and (uniform-vector? x)
       (equal? (array-prototype x) 1/3)))
(define (complex-vector? x)
  (and (uniform-vector? x)
       (equal? (array-prototype x) 0+i)))
(define (real-matrix? A)
  (and (= (array-order A) 2)
       (equal? (array-prototype A) 1/3)))
(define (complex-matrix? A)
  (and (= (array-order A) 2)
       (equal? (array-prototype A) 0+i)))

(define (array-rows v) (car (array-dimensions v)))
(define (array-columns v) (cadr (array-dimensions v)))

;;; Elementwise addition
(define (array-add-constant a b)
  (array->uniform-array (array-map (lambda (x) (+ x b)) a)))

(define (array-scale a b)
  (array->uniform-array (array-map (lambda (x) (* x b)) a)))

(define (array-add-elements a b)
  (let ((order (max (array-order a)
		    (array-order b))))
    (array->uniform-array (array-map +
				     (ensure-array-order order a)
				     (ensure-array-order order b)))))

(define (array-subtract-elements a b)
  (let ((order (max (array-order a)
		    (array-order b))))
    (array->uniform-array (array-map -
				     (ensure-array-order order a)
				     (ensure-array-order order b)))))

;;; Elementwise multiplication
(define (array-multiply-elements a b)
  (let ((order (max (array-order a)
		    (array-order b))))
    (array->uniform-array (array-map *
				     (ensure-array-order order a)
				     (ensure-array-order order b)))))

(define (array-divide-elements a b)
  (let ((order (max (array-order a)
		    (array-order b))))
    (array->uniform-array (array-map /
				     (ensure-array-order order a)
				     (ensure-array-order order b)))))
