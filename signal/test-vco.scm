;;; VCO testcases
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

(run-test (display "voltage controlled oscillator")
	  (let* ((fs 44100)
		 (center-freq (* pi 0.125))
		 (vco (make-vco center-freq 0))
		 (offset #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
	    (assert-equal? (vector-map vco offset)
			   (cosine-generator 1 center-freq 0
					     (vector-length offset)))))

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
