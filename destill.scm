#!/usr/bin/guile -s
!#

(use-modules (ice-9 readline)
	     (ice-9 regex)
	     (ice-9 format))

(define (postprocess disp file port)
  (let ((line (read-line port)))
    (cond ((eof-object? line)
	   #t)
	  (else
	   (let* ((regex (string-append "^# [1-9][0-9]* \"(.*)\""))
		  (match (string-match regex line)))
	     ;; (format #t "~S\n" match)
	     (cond ((not match)
		    (cond (disp
			   (format #t "~A\n" line)
			   (postprocess #t file port))
			  (else
			   (postprocess #f file port))))
		   ((string=? (match:substring match 1)
			      file)
		    (format #t "// ~A\n" line)
		    (postprocess #t file port))
		   (else
		    (postprocess #f file port))))))))

(postprocess #f "/usr/include/sndfile.h" (current-input-port))
