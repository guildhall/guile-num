;;; Interface to Gnuplot
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

(define-module (plot gnuplot)
  :use-module (ice-9 slib))

(export plot
	semilogx
	semilogy
	loglog)

(require 'macro-by-example)

(define (write-vectors x y)
  (do ((index 0 (+ index 1)))
      ((= index (min (vector-length x)
		     (vector-length y))))
    (display (vector-ref x index))
    (display "\t")
    (display (vector-ref y index))
    (newline)))

(define-syntax to-gnuplot
  (syntax-rules ()
		((_ x y body1 body2 ...)
		 (cond ((not (= (vector-length x)
				(vector-length y)))
			(error "vectors should have the same length"))
		       (else
			(let ((tmpfile (tmpnam)))
			  (with-output-to-file tmpfile
			    (lambda ()
			      body1 body2 ...
			      (write-vectors x y)
			      (display "e\n")))
			  (system (string-append "gnuplot -persist "
						 tmpfile))
;;;			  (system (string-append "rm " tmpfile))
))))))

(define (plot x y)
  (to-gnuplot x y (display "plot '-' w l\n")))

(define (semilogx x y)
  (to-gnuplot x y
	      (display "set logscale x\n")
	      (display "plot '-' w l\n")))

(define (semilogy x y)
  (to-gnuplot x y
	      (display "set logscale y\n")
	      (display "plot '-' w l\n")))

(define (loglog x y)
  (to-gnuplot x y
	     (display "set logscale xy\n")
	     (display "plot '-' w l\n")))
