;;; Test functions for Guile
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

(define-module (testfun unit-test))

(export assert
	unit-test)

(define (bool->number x)
  (if x 1 0))

(define (assert description assertion)
  (let ((msg (string-append (if assertion "PASSED" "FAILED") ": "
			    description)))
    (if assertion
	(begin (display msg)
	       (newline))
	(begin (error msg)))))

;;; expected-pass
;;; expected-fail
;;; unexpected-pass
;;; unexpected-fail
;;; unsupported


(define (unit-test . args)
  (let ((results (map (lambda (x) (x)) lst)))
    (let ((cases (length results))
	  (cases-passed (apply + (map bool->number results)))
	  (cases-failed (apply + (map (lambda (x) (bool->number (not x)))
				      results))))
    (newline)
    (display "Executed ")
    (display (number->string cases))
    (display " testcases.")
    (newline)
    (display (number->string cases-passed))
    (display " cases passed.")
    (newline) 
    (display (number->string cases-failed))
    (display " cases failed.")
    (newline)
 
