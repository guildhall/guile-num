;;; Signal filtering
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

(define-module (signal filter)
  :use-module (ice-9 slib)
  :use-module (math array-fun))

(export make-fixed-iir-filter
	make-adaptive-iir-filter
	iir-filter)

(require 'macro-by-example)

(define-syntax iterate-filter
  (syntax-rules 
   ()
   ((iterate-filter len num den state)
    (case len
      ((0) (lambda (x)
	     (* (uniform-vector-ref num 0) x)))
      (else (lambda (x)
	      (let ((y (+ (uniform-vector-ref state 0)
			  (* (uniform-vector-ref num 0) x))))
		(do ((idx 0 (+ idx 1)))
		    ((= idx (- len 1)))
		  (vector-set! state idx
			       (+ (uniform-vector-ref state (+ idx 1))
				  (- (* (uniform-vector-ref den (+ idx 1)) y))
				  (* (uniform-vector-ref num (+ idx 1)) x))))
		(vector-set! state (- len 1)
			     (- (* (uniform-vector-ref num len) x)
				(* (uniform-vector-ref den len) y)))
		y)))))))

(define (make-fixed-iir-filter num den . init)
  "Returns a function that filters its argument"
  (if (= (uniform-vector-ref den 0) 0.0)
      (error "the first element of the denominator must be non-zero"))
  (let* ((len   (- (max (uniform-vector-length num)
			(uniform-vector-length den)) 1))
	 (state (if (null? init)
		    (make-array 0.0 len)
		    (car init)))
	 (normalize (lambda (x) (/ x (uniform-vector-ref den 0))))
	 (num   (array-map normalize (vector-resize num (+ len 1) 0.0)))
	 (den   (array-map normalize (vector-resize den (+ len 1) 0.0))))
    (if (not (= len (uniform-vector-length state)))
	(error "state must be a vector of length max (length (a), length (b)) - 1"))
    (iterate-filter len num den state)))

(define (make-adaptive-iir-filter state)
  "Returns a function that filters its argument"
  (let ((len (uniform-vector-length init)))
    (lambda (num den x)
      (if (= (uniform-vector-ref den 0) 0.0)
	  (error "the first element of the denominator must be non-zero"))
      (if (not (= len (-1 (max (uniform-vector-length num)
				(uniform-vector-length den)))))
	  (error "state and filter polynomials are incompatible"))
      (let* ((normalize (lambda (x) (/ x (uniform-vector-ref den 0))))
	     (num   (array-map normalize (vector-resize num (+ len 1) 0.0)))
	     (den   (array-map normalize (vector-resize den (+ len 1) 0.0))))
	(array-map (iterate-filter len num den state) x)))))

(define (iir-filter num den x . init)
  "Filter signal @var{x} with an IIR filter @var{num}/@var{den} with optional
initial values @var{init}."
  (if (= (uniform-vector-ref den 0) 0.0)
      (error "filter: the first element of the denominator must be non-zero"))
  (let* ((len   (-1 (max (uniform-vector-length num)
			  (uniform-vector-length den))))
	 (state (if (null? init)
		    (make-vector len 0.0)
		    (car init)))
	 (normalize (lambda (x) (/ x (uniform-vector-ref den 0))))
	 (num   (array-map normalize (vector-resize num (+ len 1) 0.0)))
	 (den   (array-map normalize (vector-resize den (+ len 1) 0.0))))
    (if (not (= len (uniform-vector-length state)))
	(error "filter: si must be a vector of length max (length (a), length (b)) - 1"))
    (cond ((number? x)
	   (values ((iterate-filter len num den state) x) state))
	  ((vector? x)
	   (values (array-map (iterate-filter len num den state) x) state))
	  (else (error "Type mismatch")))))

