(use-modules (testfun unit-test))

;; make-linear-quantiser : number -> (number -> number)
(define (make-linear-quantiser stepsize)
  (lambda (sample)
    (inexact->exact (floor (+ 0.5 (/ sample stepsize))))))

(unit-test (let ((quantiser (make-linear-quantiser 2))
		 (input  '(-4 -3 -2 -1.1 -1.0 -0.9 0 0.9 1.0 1.1 2 3 4))
		 (output '(-2 -1 -1 -1    0    0   0 0   1   1   1 2 2)))
	     (equal? (map quantiser input)
		     output))
	   "quantiser test")


;; make-linear-dequantiser : number -> number
(define (make-linear-dequantiser stepsize)
  (lambda (sample)
    (* stepsize sample)))

(unit-test (let ((quantiser (make-linear-quantiser 2))
		 (dequantiser (make-linear-dequantiser 2))
		 (input  '(-4 -3 -2 -1.1 -1.0 -0.9 0 0.9 1.0 1.1 2 3 4))
		 (output '(-4 -2 -2 -2 0 0 0 0 2 2 2 4 4)))
	     (equal? (map dequantiser (map quantiser input))
		     output))
	   "quantiser-dequantiser test")


;; make-riq-quantiser : number number -> (number -> listof number)
(define (make-riq-quantiser stepsize K)
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


;; make-riq-dequantiser : number -> (listof number -> number)
(define (make-riq-dequantiser stepsize)
  (let ((dequantise (make-linear-dequantiser stepsize)))
    (lambda (sample)
      (apply + (map dequantise sample)))))

(unit-test (let ((quantiser (make-riq-quantiser 2 4))
		 (dequantiser (make-riq-dequantiser 2))
		 (input  '(0 1 2 3 5 10 11 12 13 15
			     0 -1 -2 -3 -5 -10 -11 -12 -13 -15))
		 (output '(0 2 2 4 6 10 12 12 14 16
			     0  0 -2 -2 -4 -10 -10 -12 -12 -14)))
	     (equal? (map dequantiser (map quantiser input))
		     output))
	   "recursively indexed quantiser-dequantiser test")



;; make-predictor : -> (number -> number)
(define (make-predictor)
  (let ((state 0))
    (lambda (sample)
      (let ((output state))
	(set! state sample)
	output))))

(unit-test (let ((predictor (make-predictor))
		 (input     '(0  1 -1  2 -2  3 -3  4 -4))
		 (output    '(0  0  1 -1  2 -2  3 -3  4)))
	     (equal? (map predictor input)
		     output))
	   "predictor test")


;; make-generic-dpcm-encoder :
;;    (number -> X) (X -> number) (number -> number)
;;    -> (number -> X)
(define (make-generic-dpcm-encoder quantise dequantise predict)
  (let ((x_hat 0))
    (lambda (sample)
      (let ((output (quantise (- sample x_hat))))
	(set! x_hat (predict (+ (dequantise output) x_hat)))
	output))))

;; make-dpcm-decoder :
;;    (X -> number) (number -> number) -> (X -> number)
(define (make-generic-dpcm-decoder dequantise predict)
  (let ((x_hat 0))
    (lambda (sample)
      (let ((output (+ (dequantise sample) x_hat)))
	(set! x_hat (predict output))
	output))))


;; make-dpcm-decoder : number -> (number -> number)
(define (make-dpcm-decoder stepsize)
  (let ((dequantise (make-linear-dequantiser stepsize))
	(predict (make-predictor)))
    (make-generic-dpcm-decoder dequantise predict)))

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


;; make-dpcm-encoder : number -> (number -> number)
(define (make-dpcm-encoder stepsize)
  (let ((predict (make-predictor))
	(quantise (make-linear-quantiser stepsize))
	(dequantise (make-linear-dequantiser stepsize)))
    (make-generic-dpcm-encoder quantise dequantise predict)))

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


;; make-riq-dpcm-encoder : number -> (number -> listof number)
(define (make-riq-dpcm-encoder stepsize K)
  (let ((predict (make-predictor))
	(quantise (make-riq-quantiser stepsize K))
	(dequantise (make-riq-dequantiser stepsize)))
    (make-generic-dpcm-encoder quantise dequantise predict)))

;; make-riq-dpcm-decoder : number -> (listof number -> number)
(define (make-riq-dpcm-decoder stepsize)
  (let ((dequantise (make-riq-dequantiser stepsize))
	(predict (make-predictor)))
    (make-generic-dpcm-decoder dequantise predict)))

(unit-test (let ((encoder (make-riq-dpcm-encoder 2 4))
		 (decoder (make-riq-dpcm-decoder 2))
		 (input  '(0  1 -1  2 -2  3 -3  4 -4  5 -5  6 -6))
		 (output '(0  2  0  2 -2  4 -2  4 -4  6 -4  6 -6)))
	     (equal? (map decoder (map encoder input))
		     output))
	   "RIQ DPCM encoding-decoding test")
