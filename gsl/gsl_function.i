%module "gsl/gsl-function"

%{
#include <gsl/gsl_math.h>

static double
_wrap_guile_function (double x, void *params)
{
	SCM result;
	SCM proc;
	SCM arg;

	proc = (SCM) params;
	arg = gh_double2scm(x);

	result = gh_call1(proc, arg);
	return gh_scm2double(result);
}

static SCM
_wrap_gsl_function_alloc (SCM s_0)
{
    #define FUNC_NAME "gsl-function-alloc"
    SCM params;
    gsl_function *result;
    SCM gswig_result;
    int gswig_list_p = 0;

    gh_defer_ints();
    result = (gsl_function *) malloc(sizeof(gsl_function));
    result->function = _wrap_guile_function;
    result->params = (void *) s_0;
    
    gh_allow_ints();
    {
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_function);
    }
    return gswig_result;
    #undef FUNC_NAME
}


static SCM
_wrap_gsl_function_free (SCM s_0)
{
    #define FUNC_NAME "gsl-function-free"
    gsl_function *arg1 ;
    SCM gswig_result;
    int gswig_list_p = 0;
    
    {
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_function))
        scm_wrong_type_arg(FUNC_NAME, 1, s_0);
    }
    gh_defer_ints();
    free(arg1);
    
    gh_allow_ints();
    gswig_result = GH_UNSPECIFIED;
    return gswig_result;
    #undef FUNC_NAME
}

double gsl_function_eval(gsl_function *f, double x)
{
	return GSL_FN_EVAL(f, x);
}



static double
_wrap_guile_function_fdf_f (double x, void *params)
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
_wrap_guile_function_fdf_df (double x, void *params)
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
_wrap_guile_function_fdf (double x, void *params, double *f, double *df)
{
	*f = _wrap_guile_function_fdf_f(x, params);
	*df = _wrap_guile_function_fdf_df(x, params);
}


double gsl_function_fdf_eval_f(gsl_function_fdf *fdf, double x)
{
	return GSL_FN_FDF_EVAL_F(fdf, x);
}

double gsl_function_fdf_eval_df(gsl_function_fdf *fdf, double x)
{
	return GSL_FN_FDF_EVAL_DF(fdf, x);
}

static SCM
_wrap_gsl_function_fdf_alloc (SCM s_0, SCM s_1)
{
    #define FUNC_NAME "gsl-function-fdf-alloc"
    SCM params;
    gsl_function_fdf *result;
    SCM gswig_result;
    int gswig_list_p = 0;

    gh_defer_ints();
    result = (gsl_function_fdf *) malloc(sizeof(gsl_function_fdf));
    result->f = _wrap_guile_function_fdf_f;
    result->df = _wrap_guile_function_fdf_df;
    result->fdf = _wrap_guile_function_fdf;
    result->params = (void *) SCM_LIST2(s_0, s_1);
    
    gh_allow_ints();
    {
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_function_fdf);
    }
    return gswig_result;
    #undef FUNC_NAME
}


static SCM
_wrap_gsl_function_fdf_free (SCM s_0)
{
    #define FUNC_NAME "gsl-function-fdf-free"
    gsl_function_fdf *arg1 ;
    SCM gswig_result;
    int gswig_list_p = 0;
    
    {
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_function_fdf))
        scm_wrong_type_arg(FUNC_NAME, 1, s_0);
    }
    gh_defer_ints();
    free(arg1);
    
    gh_allow_ints();
    gswig_result = GH_UNSPECIFIED;
    return gswig_result;
    #undef FUNC_NAME
}


%}

double gsl_function_eval(gsl_function *f, double x);
double gsl_function_fdf_eval_f(gsl_function_fdf *fdf, double x);
double gsl_function_fdf_eval_df(gsl_function_fdf *fdf, double x);

%init %{
    gh_new_procedure("gsl-function-alloc", (swig_guile_proc) _wrap_gsl_function_alloc, 1, 0, 0);
    gh_new_procedure("gsl-function-free", (swig_guile_proc) _wrap_gsl_function_free, 1, 0, 0);
    gh_new_procedure("gsl-function-fdf-alloc", (swig_guile_proc) _wrap_gsl_function_fdf_alloc, 2, 0, 0);
    gh_new_procedure("gsl-function-fdf-free", (swig_guile_proc) _wrap_gsl_function_fdf_free, 1, 0, 0);
%}

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-function.la"))
(dynamic-call "SWIG_init" my-so)

(export gsl-function-alloc gsl-function-free
        gsl-function-fdf-alloc gsl-function-fdf-free)
%}
