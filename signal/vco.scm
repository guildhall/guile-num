
;; gebruik voltage controlled oscilatoren (VCO) als basiselement om
;; het menselijk oor te modelleren.  Een hele reeks VCOs voor
;; verschillende frequentiebereiken zijn misschien in staat een goede
;; benadering te geven voor de perceptie van geluid.
;;
;; Experimenten die duiden op VCO-achtige structuren in het oor.
;;
;; * een toon die in frequentie toeneemt en waarvan een deel vervangen
;; wordt door ruis wordt waargenomen als een enkele toon zonder
;; onderbreking.  Mogelijke verklaring: de VCO is gelocked op de toon
;; en door de traagheid niet in staat om de de ruistoon snel te
;; volgen.  Als de toon vervolgens doorgaat, lockt de VCO weer alsof
;; er niets gebeurd is.
;;
;; * identieke geluidsfragmenten aan het einde van een verschillend
;; geluidsfragment ervoor klinken anders.  Zie ''A critique of ... by
;; ...'' voor een gedetailleerd uitleg.


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

;; (let* ((fs 44100)
;;        (center-freq (* pi 0.125))
;;        (vco (make-vco center-freq 0))
;;        (offset (map (lambda (x) (* 0.01 x))
;; 		    (list 0 1 -1 1 -1 1 -1 1 -1))))
;;   (display (map vco offset))
;;   (newline))

(define (make-ear-element center-freq)
  (let ((vco (make-vco center-freq 0))
	(lf  (make-filter ...))
	(lpf (make-filter ...))
	(x_lf 0))
    (lambda (x_in)
      (let* ((x_vco (vco x_lf))
	     (x_mult1 (* x_in (car x_vco)))
	     (x_mult2 (* x_in (cadr x_vco))))
	(set! x_lf (lf x_mult1))
	(list x_lf (lpf x_mult2))))))


;; make-butterworth-filter : number number -> (number -> number)
;;
;; (make-butterworth-filter f n) returns an n-th order butterworth
;; filter with cutoff frequency f.  The frequency should be between 0
;; (DC) and pi (1/2 sample frequency).
(define (make-butterworth-filter cutoff order)
  (let* ((expt-cutoff (expt (* 2 cutoff) order))
	 (num (poly-mul (make-vector 1 expt-cutoff)
			(poly-expt #(1 1) order)))
	 (den (map-elts + (poly-expt #(-1 1) order) num)))
    (make-filter num den)))

;;; (run-test (display "impulse response butterworth filter")
;;; 	  (let* ((cutoff (* pi 0.125))
;;; 		 (lpf (make-butterworth-filter cutoff 1))
;;; 		 (len 256)
;;; 		 (impulse (list->vector (cons 1 (make-list 255 0)))))
;;; 	    (semilogy (linear-generator 0 len)
;;; 		      (serialize (lambda (x) (vector-map magnitude x))
;;; 				 (lambda (x) (fft x))
;;; 				 (lambda () (vector-map lpf impulse))))))

;; make-pll : 
;;
(define (make-pll center-freq gain lpf)
  (let ((vco (make-vco center-freq 0))
	(u 0)
	(y 0))
    (lambda (x)
      (set! y (lpf u))
      (set! u (* (vco (* gain y)) x))
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
