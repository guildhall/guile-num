%module "gsl/gsl-integration"

%multiple_values;

%{
#include <gsl/gsl_integration.h>
%}

typedef enum {
	GSL_INTEG_COSINE,
	GSL_INTEG_SINE
} gsl_integration_qawo_enum;


extern int gsl_integration_qng (const gsl_function *F, double A, double B, double EPSABS, double EPSREL, double * OUTPUT, double * OUTPUT, size_t * NEVAL);

extern gsl_integration_workspace * gsl_integration_workspace_alloc (size_t N);
extern void gsl_integration_workspace_free (gsl_integration_workspace * W);
extern int gsl_integration_qag (const gsl_function *F, double A, double B, double EPSABS, double EPSREL, size_t LIMIT, int KEY, gsl_integration_workspace * WORKSPACE, double * OUTPUT, double * OUTPUT);
extern int gsl_integration_qags (const gsl_function * F, double A, double B, double EPSABS, double EPSREL, size_t LIMIT, gsl_integration_workspace * WORKSPACE, double *OUTPUT, double *OUTPUT);
extern int gsl_integration_qagp (const gsl_function * F, double *PTS, size_t NPTS, double EPSABS, double EPSREL, size_t LIMIT, gsl_integration_workspace * WORKSPACE, double *OUTPUT, double *OUTPUT);
extern int gsl_integration_qagi (gsl_function * F, double EPSABS, double EPSREL, size_t LIMIT, gsl_integration_workspace * WORKSPACE, double *OUTPUT, double *OUTPUT);
extern int gsl_integration_qagiu (gsl_function * F, double A, double EPSABS, double EPSREL, size_t LIMIT, gsl_integration_workspace * WORKSPACE, double *OUTPUT, double *OUTPUT);
extern int gsl_integration_qagil (gsl_function * F, double B, double EPSABS, double EPSREL, size_t LIMIT, gsl_integration_workspace * WORKSPACE, double *OUTPUT, double *OUTPUT);
extern int gsl_integration_qawc (gsl_function *F, double A, double B, double C, double EPSABS, double EPSREL, size_t LIMIT, gsl_integration_workspace * WORKSPACE, double * OUTPUT, double * OUTPUT);

extern gsl_integration_qaws_table * gsl_integration_qaws_table_alloc (double ALPHA, double BETA, int MU, int NU);
extern int gsl_integration_qaws_table_set (gsl_integration_qaws_table * T, double ALPHA, double BETA, int MU, int NU);
extern void gsl_integration_qaws_table_free (gsl_integration_qaws_table * T);
extern int gsl_integration_qaws (gsl_function * F, const double A, const double B, gsl_integration_qaws_table * T, const double EPSABS, const double EPSREL, const size_t LIMIT, gsl_integration_workspace * WORKSPACE, double *OUTPUT, double *OUTPUT);
extern gsl_integration_qawo_table * gsl_integration_qawo_table_alloc (double OMEGA, double L, enum gsl_integration_qawo_enum SINE, size_t N);
extern int gsl_integration_qawo_table_set (gsl_integration_qawo_table * T, double OMEGA, double L, enum gsl_integration_qawo_enum SINE);
extern int gsl_integration_qawo_table_set_length (gsl_integration_qawo_table * T, double L);
extern void gsl_integration_qawo_table_free (gsl_integration_qawo_table * T);
extern int gsl_integration_qawo (gsl_function * F, const double A, const double EPSABS, const double EPSREL, const size_t LIMIT, gsl_integration_workspace * WORKSPACE, gsl_integration_qawo_table * WF, double *OUTPUT, double *OUTPUT);
extern int gsl_integration_qawf (gsl_function * F, const double A, const double EPSABS, const size_t LIMIT, gsl_integration_workspace * WORKSPACE, gsl_integration_workspace * CYCLE_WORKSPACE, gsl_integration_qawo_table * WF, double *OUTPUT, double *OUTPUT);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-integration.la"))
(dynamic-call "SWIG_init" my-so)
%}
