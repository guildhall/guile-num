;;; Testcases for DPCM
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

(use-modules (testfun unit-test)
	     (signal dpcm))

(unit-test (let ((quantiser (make-linear-quantiser 2))
		 (input  '(-4 -3 -2 -1.1 -1.0 -0.9 0 0.9 1.0 1.1 2 3 4))
		 (output '(-2 -1 -1 -1    0    0   0 0   1   1   1 2 2)))
	     (equal? (map quantiser input)
		     output))
	   "quantiser test")

(unit-test (let ((quantiser (make-linear-quantiser 2))
		 (dequantiser (make-linear-dequantiser 2))
		 (input  '(-4 -3 -2 -1.1 -1.0 -0.9 0 0.9 1.0 1.1 2 3 4))
		 (output '(-4 -2 -2 -2 0 0 0 0 2 2 2 4 4)))
	     (equal? (map dequantiser (map quantiser input))
		     output))
	   "quantiser-dequantiser test")

(unit-test (let ((quantiser (make-riq-quantiser 2 4))
		 (input  '(0 1 2 3 5 10 11 12 13 15
			     0 -1 -2 -3 -5 -10 -11 -12 -13 -15))
		 (output '((0) (1) (1) (2) (2 1)
			   (2 2 1) (2 2 2) (2 2 2)
			   (2 2 2 1) (2 2 2 2)
			   (0) (0) (-1) (-1 0) (-1 -1 0)
			   (-1 -1 -1 -1 -1) (-1 -1 -1 -1 -1 0)
			   (-1 -1 -1 -1 -1 -1) (-1 -1 -1 -1 -1 -1 0)
			   (-1 -1 -1 -1 -1 -1 -1 0))))
	     (equal? (map quantiser input)
		     output))
	   "recursively indexed quantiser test")

(unit-test (let ((quantiser (make-riq-quantiser 2 4))
		 (dequantiser (make-riq-dequantiser 2))
		 (input  '(0 1 2 3 5 10 11 12 13 15
			     0 -1 -2 -3 -5 -10 -11 -12 -13 -15))
		 (output '(0 2 2 4 6 10 12 12 14 16
			     0  0 -2 -2 -4 -10 -10 -12 -12 -14)))
	     (equal? (map dequantiser (map quantiser input))
		     output))
	   "recursively indexed quantiser-dequantiser test")

(unit-test (let ((predictor (make-predictor))
		 (input     '(0  1 -1  2 -2  3 -3  4 -4))
		 (output    '(0  0  1 -1  2 -2  3 -3  4)))
	     (equal? (map predictor input)
		     output))
	   "predictor test")

(unit-test (let ((decoder (make-dpcm-decoder 1))
		 (input  '(0  1 -1  2 -2  3 -3  4  -4))
		 (output '(0  1 -1  3 -3  6 -6 10 -10)))
	     (equal? (map decoder input)
		     output))
	   "DPCM decoding test")

(unit-test (let ((decoder (make-dpcm-decoder 2))
		 (input  '(0  1 -1  2 -2  3  -3  4  -4))
		 (output '(0  2 -2  6 -6 12 -12 20 -20)))
	     (equal? (map decoder input)
		     output))
	   "DPCM decoding with coarser quantisation test")

(unit-test (let ((encoder (make-dpcm-encoder 1))
		 (input  '(0  1 -1  2 -2  3 -3  4 -4))
		 (output '(0  1 -1  1 -1  1 -1  1 -1)))
	     (equal? (map encoder input)
		     output))
	   "DPCM encoding test")

(unit-test (let ((encoder (make-dpcm-encoder 2))
		 (input  '(0  1 -1  2 -2  3 -3  4 -4  5 -5  6 -6))
		 (output '(0  1  0  0 -1  1  0  0 -1  1  0  0 -1)))
	     (equal? (map encoder input)
		     output))
	   "DPCM encoding with coarser quantisation test")

(unit-test (let ((encoder (make-dpcm-encoder 2))
		 (decoder (make-dpcm-decoder 2))
		 (input  '(0  1 -1  2 -2  3 -3  4 -4  5 -5  6 -6))
		 (output '(0  2  0  2 -2  4 -2  4 -4  6 -4  6 -6)))
	     (equal? (map decoder (map encoder input))
		     output))
	   "DPCM encoding-decoding test")

(unit-test (let ((encoder (make-riq-dpcm-encoder 2 4))
		 (decoder (make-riq-dpcm-decoder 2))
		 (input  '(0  1 -1  2 -2  3 -3  4 -4  5 -5  6 -6))
		 (output '(0  2  0  2 -2  4 -2  4 -4  6 -4  6 -6)))
	     (equal? (map decoder (map encoder input))
		     output))
	   "RIQ DPCM encoding-decoding test")
