;;; Conversion functions related to sound
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

(define-module (sound conversion))

(export hertz->bark
	bark->hertz
	hertz->erb
	erb->hertz
	hertz->mel
	mel->hertz)

(define (hertz->bark f)
  "Convert a frequency in Hz to a Bark scale value"
  (- (/ 26.81 (+ 1 (/ 1960 f))) 0.53))

(define (bark->hertz z)
  "Convert a Bark scale value to a frequency in Hz"
  (/ 1960 (- (/ 26.81 (+ z 0.53)) 1)))

(define (hertz->erb f)
  "Convert a frequency in Hz to an ERB-rate scale"
  (let ((a 11.17268)
	(b 46.06538)
	(c 14678.49))
    (* a (log (+ 1 (/ (* b f) (+ f c)))))))

(define (erb->hertz e)
  "Convert an ERB-rate value to a frequency in Hz"
  (let ((a 676170.4)
	(b 47.06538)
	(d 14678.49))
    (- (/ a (- b (exp (* c e)))) d)))

(define (hertz->mel f)
  "Convert a frequency in Hz to a MEL scale value"
  (/ (* 1000 (log (+ 1 (/ f 700)))) (log (+ 1 (/ 1000 700)))))

(define (mel->hertz m)
  "Convert a MEL scale value to a frequency in Hz"
  (* 700 (- (exp (/ (* (log (+ 1 (/ 1000 700))) m) 1000)) 1)))
