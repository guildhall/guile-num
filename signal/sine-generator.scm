;;; Generators
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

(define (linear-generator start stop)
  (let ((output (make-vector (- stop start) 0)))
    (do ((index start (+ index 1)))
	((= index stop))
      (vector-set! output (- index start) index))
    output))

(define (sequence start step stop)
  (cond ((= start stop)
	 #())
	((= step 0)
	 #())
	((and (> start stop)
	      (> step 0))
	 (error "Unable to go from start to stop with this step size"))
	((and (< start stop)
	      (< step 0))
	 (error "Unable to go from start to stop with this step size"))
	(else
	 (let ((output (make-vector (inexact->exact (/ (- stop start) step))
				    0)))
	   (do ((index 0 (+ index 1)))
	       ((= index (vector-length output)))
	     (vector-set! output index (+ (* step index) start)))
	   output))))
	     


(run-test (display "sequence generator")
	  (assert-equal? (sequence 0 0 0) #())
	  (assert-equal? (sequence 0 1 0) #())
	  (assert-equal? (sequence 0 1 5) (linear-generator 0 5))
	  (assert-equal? (sequence 0 0.1 0.5)
			 #(0.0 0.1 0.2 0.3 0.4))
)


(define (cosine-generator A f p len)
  (let loop ((output (make-vector len 0))
	     (index  0)
	     (phase  p))
    (cond ((>= index len)
	   output)
	  (else
	   (vector-set! output index (* A (cos phase)))
	   (loop output (+ index 1) (fold-phase (+ phase f)))))))

;;;(run-test (display "plot sine generation")
;;;	  (let ((fs 44100)
;;;		(A 100)
;;;		(f (* pi 0.125))
;;;		(p 0)
;;;		(tmax 17))
;;;	    (plot (linear-generator 0 tmax) (cosine-generator A f p tmax))))

