#!/usr/bin/guile -s
!#

(use-modules (ice-9 readline)
	     (ice-9 regex)
	     (ice-9 format))

(define (postprocess disp file-regex port)
  (let ((line (read-line port)))
    (cond ((eof-object? line)
	   #t)
	  (else
	   (let* ((regex (string-append "^# [1-9][0-9]* \"(.*)\""))
		  (match (string-match regex line)))
	     (cond ((not match)
		    (cond (disp
			   (format #t "~A\n" line)
			   (postprocess #t file-regex port))
			  (else
			   (postprocess #f file-regex port))))
		   ((string-match file-regex (match:substring match 1))
		    (format #t "// ~A\n" line)
		    (postprocess #t file-regex port))
		   (else
		    (postprocess #f file-regex port))))))))

(define program-name (car (command-line)))
(define program-version "0.0.1")

(define help
  (string-append "Usage: " program-name " [OPTION] FILE-REGEX

Destill from standard input only those parts that have come from the
macro-expanded FILE-REGEX.

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
  ((0)
   (format #t "~A\n" help)
   (quit))
  ((1)
   ;; (format (current-error-port) "Processing ~A\n" (car *non-option-args*))
   (postprocess #f (car *non-option-args*) (current-input-port)))
  (else (error "Only support one regex argument")))
