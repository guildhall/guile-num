;;; Signal generating elements
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

(define-module (signal generators)
  :use-module (math const))

(export make-amplifier
	make-vco
	make-pll
	fold-phase)

(define (fold-phase p)
  (- p (* (* 2 pi) (round (/ p (* 2 pi))))))

(define (make-amplifier gain)
  (lambda (x)
    (* gain x)))

(define (make-vco center-freq start-phase)
  "Returns a voltage controlled oscillator with a center frequency
@var{center-freq} and starting phase @var{start-phase}.  The center 
frequency range should be between 0 (DC) and pi (1/2 sample frequency)."
  (let ((phase (+ start-phase (- center-freq))))
    (lambda (offset)
      (set! phase (fold-phase (+ phase center-freq offset)))
      (list (cos phase) (sin phase)))))

(run-test (display "voltage controlled oscillator")
	  (let* ((fs 44100)
		 (center-freq (* pi 0.125))
		 (vco (make-vco center-freq 0))
		 (offset #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	    (assert-equal? (vector-map vco offset)
			   (cosine-generator 1 center-freq 0
					     (vector-length offset)))))

;; make-pll : 
(define (make-pll vco sensitivity lpf)
  (let ((y 0))
    (lambda (x)
      (set! y (lpf (* (vco (* sensitivity y)) x)))
      y)))


(run-test (display "phase locked loop")
	  (let* ((cutoff (* pi 0.125))
		 (pll (make-pll cutoff 100
				(make-filter #(1) #(1 -0.125))))
		 (tmax 500)
		 (sine (make-vco (* 1.05 cutoff) 0)))
	    (plot (linear-generator 0 tmax)
		  (serialize (lambda (x) (vector-map pll x))
			     (lambda (x) (vector-map (make-amplifier 100) x))
			     (lambda (x) (vector-map sine x))
			     (lambda () (make-vector tmax 0))))))
