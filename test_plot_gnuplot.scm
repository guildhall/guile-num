(use-modules (math array-fun))               
(use-modules (plot gnuplot))                 

(define v #(-3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3))
(define w (array-map (lambda (x) (* x x)) v))
(plot v w)                                   
