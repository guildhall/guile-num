;;; Butterworth filter design
;;; Copyright (C) 2003   Arno W. Peters <a.w.peters@ieee.org>
;;; This version is based on the Octave version and supporting
;;; routines Copyright (C) 1999   Paul Kienzle <pkienzle@cs.indiana.edu>
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

(define-module (signal butter)
  :use-module (math const))

(export sftrans
	bilinear
	butter)

(define (sftrans Sz Sp Sg W stop)
  "(sftrans Sz Sp Sg W stop) -> (values Sz Sp Sg)

Transform band edges of a generic lowpass filter (cutoff at W=1)
represented in splane zero-pole-gain form.  W is the edge of the
target filter (or edges if band pass or band stop). Stop is true for
high pass and band stop filters or false for low pass and band pass
filters. Filter edges are specified in radians, from 0 to pi (the
nyquist frequency)."
  (define (quad-solve ac B)
    (append (map (lambda (b) (+ b (sqrt (- (* b b) ac)))) B)
	    (map (lambda (b) (- b (sqrt (- (* b b) ac)))) B)))
  (define (extend-terms n v)
    (cond ((> n 0)
	   (map (lambda (r)
		  (list-ref v r))
		(map (lambda (k)
		       (remainder k (length v)))
		     (iota (* 2 n)))))
	  (else '())))
  (define (band-stop Sz Sp Sg W)
    (let ((Fl (car W))
	  (Fh (cadr W))
	  (C 1)
	  (p (length Sp))
	  (z (length Sz)))
      (set! Sg (* Sg
		  (real-part
		   (/ (apply * (map (lambda (x) (- x)) Sz))
		      (apply * (map (lambda (x) (- x)) Sp))))))
      (set! Sp (quad-solve (* Fh Fl)
			   (map (lambda (x)
				  (/ (* C (- Fh Fl)) 2 x))
				Sp)))
      (let ((extend (list (sqrt (- (* Fh Fl)))
			  (- (sqrt (- (* Fh Fl)))))))
	(set! Sz (append (quad-solve (* Fh Fl)
				     (map (lambda (x)
					    (/ (* C (- Fh Fl)) 2 x))
					  Sz))
			 (extend-terms (- p z) extend))))
      (values Sz Sp Sg)))
  (define (band-pass Sz Sp Sg W)
    (let ((Fl (car W))
	  (Fh (cadr W))
	  (C 1)
	  (p (length Sp))
	  (z (length Sz)))
      (set! Sg (* Sg
		  (expt (/ C (- Fh Fl))
			(- z p))))
      (set! Sp (quad-solve (* Fh Fl)
			   (map (lambda (x)
				  (* x (/ (- Fh Fl) (* 2 C))))
				Sp)))
      (cond ((null? Sz)
	     (set! Sz (make-list p 0)))
	    (else
	     (set! Sz (quad-solve (* Fh Fl)
				  (map (lambda (x)
					 (* x (/ (- Fh Fl) (* 2 C))))
				       Sz)))
	     (if (> p z)
		 (set! Sz (append Sz (make-list (- p z) 0))))))
      (values Sz Sp Sg)))
  (define (high-pass Sz Sp Sg W)
    (let ((Fc (car W))
	  (C 1)
	  (p (length Sp))
	  (z (length Sz)))
      (set! Sg (* Sg
		  (real-part
		   (/ (apply * (map (lambda (x) (- x)) Sz))
		      (apply * (map (lambda (x) (- x)) Sp))))))
      (set! Sp (map (lambda (x)
		      (/ (* C Fc) x))
		    Sp))
      (cond ((null? Sz)
	     (set! Sz (make-list p 0)))
	    (else
	     (set! Sz (map (lambda (x)
			     (/ (* C Fc) x))
			   Sz))
	     (if (> p z)
		 (set! Sz (append Sz (make-list (- p z) 0))))))
      (values Sz Sp Sg)))
  (define (low-pass Sz Sp Sg W)
    (let ((Fc (car W))
	  (C 1)
	  (p (length Sp))
	  (z (length Sz)))
      (set! Sg (* Sg (expt (/ C Fc) (- z p))))
      (set! Sp (map (lambda (x)
		      (/ (* Fc x) C))
		    Sp))
      (set! Sz (map (lambda (x)
		      (/ (* Fc x) C))
		    Sz))
      (values Sz Sp Sg)))

  (cond ((and (= (length W) 2) stop)
	 ;; (display "Band stop\n")
	 (band-stop Sz Sp Sg W))
	((= (length W) 2)
	 ;; (display "Band pass\n")
	 (band-pass Sz Sp Sg W))
	(stop
	 ;; (display "High pass\n")
	 (high-pass Sz Sp Sg W))
	(else
	 ;; (display "Low pass\n")
	 (low-pass Sz Sp Sg W))))

(define (bilinear Sz Sp Sg T)
  "(bilinear Sz Sp Sg T) -> (values Zz Zp Zg)

Transform a s-plane filter specification into a z-plane
specification. Filters can be specified in either zero-pole-gain or
transfer function form. T is the sampling frequency represented in the
z plane."
  (let ((p (length Sp))
	(z (length Sz))
	(Zz '())
	(Zp '())
	(Zg '()))
    (if (or (> z p) (= p 0))
	(error "bilinear: must have at least as many poles as zeros"))
    (set! Zg (real-part
	      (* Sg (/ (apply * (map (lambda (x) (/ (- 2 (* x T)) T)) Sz))
		       (apply * (map (lambda (x) (/ (- 2 (* x T)) T)) Sp))))))
    (set! Zp (map (lambda (x) (/ (+ 2 (* x T)) (- 2 (* x T)))) Sp))
    (cond ((null? Sz)
	   (set! Zz (make-list p -1)))
	  (else
	   (set! Zz (append (map (lambda (x)
				   (/ (+ 2 (* x T)) (- 2 (* x T))))
				 Sz)
			    (make-list (max 0 (- p z)) -1)))))
    (values Zz Zp Zg)))

(let ((B '())
      (A '(-1))
      (G 1)
      (T 2))
  (call-with-values
   (lambda ()
     (bilinear B A G T))
   (lambda (Zz Zp Zg)
     (display (and (equal? Zz '(-1))
		   (equal? Zp '(0))
		   (= Zg 0.5)))
     (newline))))

(define (butter n W stop digital)
  "(butter n W stop digital) -> (values Sz Sp Sg)

Return an s-plane Butterworth filter specification if DIGITAL is #f,
otherwise return a z-plane Butterworth filter specification.  In case
of a digital filter, the frequencies should lie between 0 and 1
(indicating half the sample frequency).

The W list and the stop parameter control the type of filter:

    W      stop  type of filter
  '(Wc)     #f   low pass filter
  '(Wc)     #t   high pass filter
  '(Wl Wh)  #f   band pass filter
  '(Wl Wh)  #t   band reject filter"
  (cond ((> (length W) 2)
	 (error "butter: frequency must be given as '(w0) or '(w0 w1)"))
	((and (= (length W) 2) (not (< (car W) (cadr W))))
	 (error "butter: first band edge must be smaller than second"))
	((and digital
	      (not (and-map identity
			    (map (lambda (x)
				   (and (>= x 0) (<= x 1)))
				 W))))
	 (error "butter: critical frequencies must be in (0 1)"))
	((and (not digital)
	      (not (and-map identity
			    (map (lambda (x) (>= x 0)) W))))
	 ("butter: critical frequencies must be in (0 inf)")))
  (let ((C 1)
	(T 2))
    (let ((pole (map (lambda (k)
		       (* C (exp (/ (* 0+i pi (+ (* 2 k) n 1))
				    (* 2 n)))))
		     (iota n)))
	  (zero '())
	  (gain (expt C n)))
      (if digital
	  (set! W (map (lambda (w)
			 (* (/ 2 T) (tan (/ (* pi w) T))))
		       W)))
      (if (= (modulo n 2) 1)
	  (list-set! pole (inexact->exact (/ (- n 1) 2)) -1))
      (call-with-values
       (lambda ()
	 (sftrans zero pole gain W stop))
       (lambda (zero pole gain)
	 (if digital
	     (bilinear zero pole gain T)
	     (values zero pole gain)))))))
