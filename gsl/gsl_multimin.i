%module "gsl/gsl-multimin"

%multiple_values;

%{
#include <gsl/gsl_errno.h>
#include <gsl/gsl_multimin.h>


static double
_wrap_guile_multimin_function_fdf_f (const gsl_vector * x,
				     void *params)
{
#define FUNC_NAME "wrap-guile-multimin-function-fdf-f"
	SCM result;
	SCM proc;
	SCM arg;

	gh_defer_ints();
	proc = SCM_CAR((SCM) params);
        arg = SWIG_Guile_MakePtr ((gsl_vector *) x, SWIGTYPE_p_gsl_vector);
	result = gh_call1(proc, arg);
	gh_allow_ints();
	return gh_scm2double(result);
#undef FUNC_NAME
}

static void
_wrap_guile_multimin_function_fdf_df (const gsl_vector * x,
				      void * params,
				      gsl_vector * G)
{
#define FUNC_NAME "wrap-guile-multimin-function-fdf-df"
	SCM result;
	SCM proc;
	SCM arg;

	gh_defer_ints();
	proc = SCM_CADR((SCM) params);
        arg = SWIG_Guile_MakePtr ((gsl_vector *) x, SWIGTYPE_p_gsl_vector);
        result = SWIG_Guile_MakePtr (G, SWIGTYPE_p_gsl_vector);
	gh_call2(proc, result, arg);
	gh_allow_ints();
#undef FUNC_NAME
}

static void
_wrap_guile_multimin_function_fdf_fdf (const gsl_vector * x,
				       void *params,
				       double * f,
				       gsl_vector * G)
{
	*f = _wrap_guile_multimin_function_fdf_f(x, params);
	_wrap_guile_multimin_function_fdf_df(x, params, G);
}

static SCM
_wrap_gsl_multimin_function_fdf_alloc (SCM s_0, SCM s_1, SCM s_2)
{
#define FUNC_NAME "gsl-multimin-function-fdf-alloc"
	SCM params;
	gsl_multimin_function_fdf *result;
	SCM gswig_result;
	int gswig_list_p = 0;
	size_t len;
	
	gh_defer_ints();
	result = (gsl_multimin_function_fdf *)
		malloc(sizeof(gsl_multimin_function_fdf));
	result->f = _wrap_guile_multimin_function_fdf_f;
	result->df = _wrap_guile_multimin_function_fdf_df;
	result->fdf = _wrap_guile_multimin_function_fdf_fdf;

	result->n = gh_scm2long(s_2);
	result->params = (void *) SCM_LIST2(s_0, s_1);

	gh_allow_ints();
	gswig_result = SWIG_Guile_MakePtr (result,
					   SWIGTYPE_p_gsl_multimin_function_fdf);
	return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_gsl_multimin_function_fdf_free (SCM s_0)
{
#define FUNC_NAME "gsl-multimin-function-fdf-free"
	gsl_multimin_function_fdf *arg1 ;
	SCM gswig_result;
	int gswig_list_p = 0;

        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1,
			      SWIGTYPE_p_gsl_multimin_function_fdf))
		scm_wrong_type_arg(FUNC_NAME, 1, s_0);

	gh_defer_ints();
	free(arg1);

	gh_allow_ints();
	gswig_result = GH_UNSPECIFIED;
	return gswig_result;
#undef FUNC_NAME
}

double gsl_multimin_function_eval_f(gsl_multimin_function_fdf * f,
				    const gsl_vector * x)
{
	return GSL_MULTIMIN_FN_EVAL_F(f, x);
}

void gsl_multimin_function_eval_df(gsl_multimin_function_fdf * f,
				   const gsl_vector * x,
				   gsl_vector * dy)
{
	GSL_MULTIMIN_FN_EVAL_DF(f, x, dy);
}

void gsl_multimin_function_eval_fdf(gsl_multimin_function_fdf * f,
				     const gsl_vector * x,
				     double * y,
				     gsl_vector * dy)
{
	GSL_MULTIMIN_FN_EVAL_F_DF(f, x, y, dy);
}

%}

%include "gsl_multimin_inc.i"

double gsl_multimin_function_eval_f(gsl_multimin_function_fdf * f, const gsl_vector * x);
void gsl_multimin_function_eval_df(gsl_multimin_function_fdf * f, const gsl_vector * x, gsl_vector * dy);
void gsl_multimin_function_eval_fdf(gsl_multimin_function_fdf * f, const gsl_vector * x, double * y, gsl_vector * dy);

%init %{
    gh_new_procedure("gsl-multimin-function-fdf-alloc", (swig_guile_proc) _wrap_gsl_multimin_function_fdf_alloc, 3, 0, 0);
    gh_new_procedure("gsl-multimin-function-fdf-free", (swig_guile_proc) _wrap_gsl_multimin_function_fdf_free, 1, 0, 0);
%}

%scheme %{
(define my-so (dynamic-link "libguile-gsl-multimin.la"))
(dynamic-call "SWIG_init" my-so)

(export gsl-multimin-function-fdf-alloc gsl-multimin-function-fdf-free)
%}
