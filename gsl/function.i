%module "gsl/function"

%{
#include <gsl/gsl_math.h>

static double
apply_guile_function (double x, void *params)
{
	SCM result;
	SCM proc;
	SCM arg;

	proc = (SCM) params;
	arg = gh_double2scm(x);

	result = gh_call1(proc, arg);
	return gh_scm2double(result);
}

double gsl_function_eval(gsl_function *f, double x)
{
	return GSL_FN_EVAL(f, x);
}

static double
apply_guile_function_fdf_f (double x, void *params)
{
	SCM result;
	SCM proc;
	SCM arg;

	proc = SCM_CAR((SCM) params);
	arg = gh_double2scm(x);

	result = gh_call1(proc, arg);
	return gh_scm2double(result);
}

static double
apply_guile_function_fdf_df (double x, void *params)
{
	SCM result;
	SCM proc;
	SCM arg;

	proc = SCM_CADR((SCM) params);
	arg = gh_double2scm(x);

	result = gh_call1(proc, arg);
	return gh_scm2double(result);
}

static void
apply_guile_function_fdf (double x, void *params, double *f, double *df)
{
	*f = apply_guile_function_fdf_f(x, params);
	*df = apply_guile_function_fdf_df(x, params);
}


double gsl_function_fdf_eval_f(gsl_function_fdf *fdf, double x)
{
	return GSL_FN_FDF_EVAL_F(fdf, x);
}

double gsl_function_fdf_eval_df(gsl_function_fdf *fdf, double x)
{
	return GSL_FN_FDF_EVAL_DF(fdf, x);
}



%}

double gsl_function_eval(gsl_function *f, double x);
double gsl_function_fdf_eval_f(gsl_function_fdf *fdf, double x);
double gsl_function_fdf_eval_df(gsl_function_fdf *fdf, double x);

%init %{
%}

%scheme %{
(define my-so (dynamic-link "libguile-gsl-function.la"))
(dynamic-call "scm_init_gsl_function_module" my-so)
%}
