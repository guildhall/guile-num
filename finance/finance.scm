;;; Financial functions for Guile
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

(define-module (finance finance))

(export fv fvb fvl
	pv pvb pvl
	pmt pmtb
	nper nperb nperl
	ratel sln syd
	rate irr)

(define (fv rate n pmt . initial)
  (if (null? initial)
      (/ (* pmt (- (expt (+ 1 rate) n) 1)) rate)
      (+ (fv rate n pmt) (fvl rate n (car initial)))))

(define (fvb rate n pmt . initial)
  (if (null? initial)
      (* (fv rate n pmt) (+ 1 rate))
      (+ (fvb rate n pmt) (fvl rate n (car initial)))))

(define (fvl rate n pmt)
  (* pmt (expt (+ 1 rate) n)))

(define (pv rate n pmt . initial)
  (if (null? initial)
      (/ (* pmt (- 1 (expt (+ 1 rate) (- 0.0 n)))) rate)
      (+ (pv rate n pmt) (plv rate n (car initial)))))

(define (pvb rate n pmt)
  (if (null? initial)
      (* (pv rate n pmt) (+ 1 rate))
      (+ (pvb rate n pmt) (plv rate n (car initial)))))

(define (pvl rate n pmt)
  (* pmt (expt (+ 1 rate) (- 0.0 n))))


;;;                                     -1               -2               -3
;;; npv(rate, [a, b, c]) =  a*(1 + rate)   + b*(1 + rate)   + c*(1 + rate)
;;; 
;;;                                         -1               -2
;;; npvb(rate, [a, b, c]) = a + b*(1 + rate)   + c*(1 + rate)

(define (pmt rate n amt x)
  (let ((f (expt (+ 1 rate) (- n))))
    (/ (* rate (- amt (* x f))) (- 1 f))))

(define (pmtb rate n amt x)
  (/ (pmt rate n amt x) (+ 1 rate)))

(define (nper rate pmt amt)
  (- (/ (log (- 1 (/ (* amt rate) pmt))) (log (+ 1 rate)))))

(define (nperb rate pmt amt)
  (- (/ (log (- 1 (/ (* amt rate) (* pmt (+ 1 rate))))) (log (+ 1 rate)))))

(define (nperl rate pmt amt)
  (- (/ (log (/ amt pmt)) (log (+ 1 rate)))))

(define (ratel n pmt amt)
  (- (expt (/ pmt amt) (/ 1 n)) 1))

(define (sln cost salv life)
  (/ (- cost salv) life))

(define (syd cost salv life per)
  (/ (* 2 (- cost salv) (+ life (- per) 1)) (* life (+ 1 life))))

(define (rate num pmt amt)
  (solve 'rate (= (pv rate num pmt) amt)))

(define (irr pmts)
  (solve 'rate (= (npv rate pmts) 0)))
