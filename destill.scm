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

(define program-name (car (command-line)))
(define program-version "0.0.1")

(define help
  (string-append "Usage: " program-name " [OPTION] FILE

Destill from standard input only those parts that have come from the
macro-expanded FILE.

  --help               This message
  --version            Version information
"))

(define version
  (string-append program-name " " program-version))

(define *non-option-args* '())

(for-each (lambda (s)
	    (cond ((string=? s "--help")
		   (format #t "~A\n" help)
		   (quit))
		  ((string=? s "--version")
		   (format #t "~A\n" version)
		   (quit))
		  (else
		   (set! *non-option-args* (cons s *non-option-args*)))))
	  (cdr (command-line)))

(case (length *non-option-args*)
  ((0) (begin (format #t "~A\n" help)
	      (quit))
   (1) (postprocess #f (car *non-option-args*) (current-input-port))
   (else (error "Only support one file argument"))))


