%module "gsl/gsl-errno"

%{
#include <gsl/gsl_errno.h>
#include <stdio.h>

gsl_error_handler_t *previous_error_handler;

%}

%include "gsl_errno.inc"

%wrapper %{
void guile_gsl_error_handler (const char * reason, const char * file,
			      int line, int gsl_errno)
{
#if 0
	fprintf(stdout, "reason: %s\n", reason);
	fprintf(stdout, "file: %s\n", file);
	fprintf(stdout, "line: %d\n", line);
	fprintf(stdout, "error: %s\n", gsl_strerror (gsl_errno));
#endif
}
%}

%init %{
previous_error_handler = gsl_set_error_handler(guile_gsl_error_handler);
%}

%scheme %{
(define my-so (dynamic-link "libguile-gsl-errno.la"))
(dynamic-call "SWIG_init" my-so)
%}
