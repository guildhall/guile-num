(use-modules (gsl vector)
	     (gsl sortvector)
	     (gsl statistics))

(define (gsl-vector-display v)
  (let ((x (gsl-vector->vector v)))
    (display x)))

(let ((v #i(17.2 18.1 16.5 18.3 12.6)))
  (let ((data (vector->gsl-vector v)))
    (display "Original dataset: ")
    (gsl-vector-display data)
    (newline)

    (gsl-sort-vector data)
    (display "Sorted dataset: ")
    (gsl-vector-display data)
    (newline)

    (display "The median is ")
    (display (gsl-stats-median-from-sorted-data (gsl-vector-data data)
						1
						(gsl-vector-length data)))
    (newline)
     
    (display "The upper quartile is ")
    (display (gsl-stats-quantile-from-sorted-data (gsl-vector-data data)
						  1
						  (gsl-vector-length data)
						  0.75))
    (newline)
	     
    (display "The lower quartile is ")
    (display (gsl-stats-quantile-from-sorted-data (gsl-vector-data data)
						  1
						  (gsl-vector-length data)
						  0.25))
    (newline)

    (gsl-vector-free data)))
								 
     

