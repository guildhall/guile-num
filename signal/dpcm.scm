;;; Differential Pulse Code Modulation
;;; Copyright (C) 2003   Arno W. Peters <a.w.peters@ieee.org>
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

(define-module (signal dpcm))

(export make-linear-quantiser
	make-linear-dequantiser
	make-riq-quantiser
	make-riq-dequantiser
	make-predictor
	make-generic-dpcm-encoder
	make-generic-dpcm-decoder
	make-dpcm-decoder
	make-dpcm-encoder
	make-riq-dpcm-encoder
	make-riq-dpcm-decoder)

(define (make-linear-quantiser stepsize)
  "make-linear-quantiser : number -> (number -> number)"
  (lambda (sample)
    (inexact->exact (floor (+ 0.5 (/ sample stepsize))))))

(define (make-linear-dequantiser stepsize)
  "make-linear-dequantiser : number -> number"
  (lambda (sample)
    (* stepsize sample)))

(define (make-riq-quantiser stepsize K)
  "make-riq-quantiser : number number -> (number -> listof number)"
  (let* ((x-low  (- (* stepsize (floor (/ (1- K) 2)))))
	 (x-high (+ x-low (* stepsize (1- K))))
	 (tolerance 1e-4)
	 (quantise (make-linear-quantiser stepsize)))

    (define (riq x)
      (cond ((and (<= x-low x)
		  (<= x x-high))
	     (list (quantise x)))

	    ((< x x-low)
	     (cons (quantise x-low) (riq (- x x-low))))

	    ((< x-high x)
	     (cons (quantise x-high) (riq (- x x-high))))

	    (else
	     (error "not reached"))))

    (if (or (< (abs x-low)  tolerance)
	    (< (abs x-high) tolerance))
	(error "parameters unsuitable for quantisation"))

    (lambda (sample)
      (riq sample))))

(define (make-riq-dequantiser stepsize)
  "make-riq-dequantiser : number -> (listof number -> number)"
  (let ((dequantise (make-linear-dequantiser stepsize)))
    (lambda (sample)
      (apply + (map dequantise sample)))))

(define (make-predictor)
  "make-predictor : -> (number -> number)"
  (let ((state 0))
    (lambda (sample)
      (let ((output state))
	(set! state sample)
	output))))

(define (make-generic-dpcm-encoder quantise dequantise predict)
  "make-generic-dpcm-encoder :
    (number -> X) (X -> number) (number -> number)
    -> (number -> X)"
  (let ((x_hat 0))
    (lambda (sample)
      (let ((output (quantise (- sample x_hat))))
	(set! x_hat (predict (+ (dequantise output) x_hat)))
	output))))

(define (make-generic-dpcm-decoder dequantise predict)
  "make-dpcm-decoder :
    (X -> number) (number -> number) -> (X -> number)"
  (let ((x_hat 0))
    (lambda (sample)
      (let ((output (+ (dequantise sample) x_hat)))
	(set! x_hat (predict output))
	output))))

(define (make-dpcm-decoder stepsize)
  "make-dpcm-decoder : number -> (number -> number)"
  (let ((dequantise (make-linear-dequantiser stepsize))
	(predict (make-predictor)))
    (make-generic-dpcm-decoder dequantise predict)))

(define (make-dpcm-encoder stepsize)
  "make-dpcm-encoder : number -> (number -> number)"
  (let ((predict (make-predictor))
	(quantise (make-linear-quantiser stepsize))
	(dequantise (make-linear-dequantiser stepsize)))
    (make-generic-dpcm-encoder quantise dequantise predict)))

(define (make-riq-dpcm-encoder stepsize K)
  "make-riq-dpcm-encoder : number -> (number -> listof number)"
  (let ((predict (make-predictor))
	(quantise (make-riq-quantiser stepsize K))
	(dequantise (make-riq-dequantiser stepsize)))
    (make-generic-dpcm-encoder quantise dequantise predict)))

(define (make-riq-dpcm-decoder stepsize)
  "make-riq-dpcm-decoder : number -> (listof number -> number)"
  (let ((dequantise (make-riq-dequantiser stepsize))
	(predict (make-predictor)))
    (make-generic-dpcm-decoder dequantise predict)))

