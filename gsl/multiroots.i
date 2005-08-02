%module "gsl/multiroots"

%multiple_values;

%{
#include <gsl/gsl_errno.h>
#include <gsl/gsl_multiroots.h>

static int
_wrap_guile_multiroot_function (const gsl_vector * x,
				void *params,
				gsl_vector * f)
{
#define FUNC_NAME "wrap-guile-multiroot-function"
	SCM result;
	SCM proc;
	SCM arg;

	gh_defer_ints();
	proc = (SCM) params;
        arg = SWIG_Guile_MakePtr ((gsl_vector *) x, SWIGTYPE_p_gsl_vector);
        result = SWIG_Guile_MakePtr (f, SWIGTYPE_p_gsl_vector);
	gh_call2(proc, result, arg);
	gh_allow_ints();

	return GSL_SUCCESS;
#undef FUNC_NAME
}

static SCM
_wrap_gsl_multiroot_function_alloc (SCM s_0, SCM s_1)
{
#define FUNC_NAME "gsl-multiroot-function-alloc"
    SCM params;
    gsl_multiroot_function *result;
    SCM gswig_result;
    int gswig_list_p = 0;
    size_t len;
    
    gh_defer_ints();
    result = (gsl_multiroot_function *) malloc(sizeof(gsl_multiroot_function));
    result->f = _wrap_guile_multiroot_function;
    result->n = gh_scm2long(s_1);
    result->params = (void *) s_0;
    
    gh_allow_ints();
    {
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_multiroot_function_struct);
    }
    return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_gsl_multiroot_function_free (SCM s_0)
{
    #define FUNC_NAME "gsl-multiroot-function-free"
    gsl_multiroot_function *arg1 ;
    SCM gswig_result;
    int gswig_list_p = 0;
    
    {
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_multiroot_function_struct))
        scm_wrong_type_arg(FUNC_NAME, 1, s_0);
    }
    gh_defer_ints();
    free(arg1);

    gh_allow_ints();
    gswig_result = GH_UNSPECIFIED;
    return gswig_result;
    #undef FUNC_NAME
}

void gsl_multiroot_function_eval(gsl_multiroot_function *f,
				 const gsl_vector *x,
				 gsl_vector *y)
{
	GSL_MULTIROOT_FN_EVAL(f, x, y);
}

static int
_wrap_guile_multiroot_function_fdf_f (const gsl_vector * x,
				      void *params,
				      gsl_vector * f)
{
#define FUNC_NAME "_wrap_guile_multiroot-function-fdf-f"
	SCM result;
	SCM proc;
	SCM arg;

	gh_defer_ints();
	proc = SCM_CAR((SCM) params);
        arg = SWIG_Guile_MakePtr (x, SWIGTYPE_p_gsl_vector);
        result = SWIG_Guile_MakePtr (f, SWIGTYPE_p_gsl_vector);
	gh_call2(proc, result, arg);
	gh_allow_ints();
	return GSL_SUCCESS;
#undef FUNC_NAME
}

static int
_wrap_guile_multiroot_function_fdf_df (const gsl_vector * x,
				       void *params,
				       gsl_matrix * J)
{
#define FUNC_NAME "_wrap_guile_multiroot-function-fdf-df"
	SCM result;
	SCM proc;
	SCM arg;

	gh_defer_ints();
	proc = SCM_CADR((SCM) params);
        arg = SWIG_Guile_MakePtr (x, SWIGTYPE_p_gsl_vector);
        result = SWIG_Guile_MakePtr (J, SWIGTYPE_p_gsl_matrix);
	gh_call2(proc, result, arg);
	gh_allow_ints();
	return GSL_SUCCESS;
#undef FUNC_NAME
}

static int
_wrap_guile_multiroot_function_fdf_fdf (const gsl_vector * x,
					void *params,
					gsl_vector * f,
					gsl_matrix * J)
{
	_wrap_guile_multiroot_function_fdf_f(x, params, f);
	_wrap_guile_multiroot_function_fdf_df(x, params, J);
	return GSL_SUCCESS;
}

static SCM
_wrap_gsl_multiroot_function_fdf_alloc (SCM s_0, SCM s_1, SCM s_2)
{
#define FUNC_NAME "gsl-multiroot-function-fdf-alloc"
	SCM params;
	gsl_multiroot_function_fdf *result;
	SCM gswig_result;
	int gswig_list_p = 0;
	size_t len;
	
	gh_defer_ints();
	result = (gsl_multiroot_function_fdf *)
		malloc(sizeof(gsl_multiroot_function_fdf));
	result->f = _wrap_guile_multiroot_function_fdf_f;
	result->df = _wrap_guile_multiroot_function_fdf_df;
	result->fdf = _wrap_guile_multiroot_function_fdf_fdf;

	result->n = gh_scm2long(s_2);
	result->params = (void *) SCM_LIST2(s_0, s_1);

	gh_allow_ints();
	gswig_result = SWIG_Guile_MakePtr (result,
					   SWIGTYPE_p_gsl_multiroot_function_fdf_struct);
	return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_gsl_multiroot_function_fdf_free (SCM s_0)
{
#define FUNC_NAME "gsl-multiroot-function-fdf-free"
	gsl_multiroot_function_fdf *arg1 ;
	SCM gswig_result;
	int gswig_list_p = 0;

        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_multiroot_function_fdf_struct))
		scm_wrong_type_arg(FUNC_NAME, 1, s_0);

	gh_defer_ints();
	free(arg1);

	gh_allow_ints();
	gswig_result = GH_UNSPECIFIED;
	return gswig_result;
#undef FUNC_NAME
}

void gsl_multiroot_function_eval_f(gsl_multiroot_function_fdf * f,
				   const gsl_vector * x,
				   gsl_vector * y)
{
	GSL_MULTIROOT_FN_EVAL_F(f, x, y);
}

void gsl_multiroot_function_eval_df(gsl_multiroot_function_fdf * f,
				    const gsl_vector * x,
				    gsl_matrix * dy)
{
	GSL_MULTIROOT_FN_EVAL_DF(f, x, dy);
}

void gsl_multiroot_function_eval_fdf(gsl_multiroot_function_fdf * f,
				     const gsl_vector * x,
				     gsl_vector * y,
				     gsl_matrix * dy)
{
	GSL_MULTIROOT_FN_EVAL_F_DF(f, x, y, dy);
}

%}

void gsl_multiroot_function_eval(gsl_multiroot_function *f, const gsl_vector *x, gsl_vector *y);
void gsl_multiroot_function_eval_f(gsl_multiroot_function_fdf * f, const gsl_vector * x, gsl_vector * y);
void gsl_multiroot_function_eval_df(gsl_multiroot_function_fdf * f, const gsl_vector * x, gsl_matrix * dy);
void gsl_multiroot_function_eval_fdf(gsl_multiroot_function_fdf * f, const gsl_vector * x, gsl_vector * y, gsl_matrix * dy);

%include "gsl_multiroots_inc.i"

%init %{
    gh_new_procedure("gsl-multiroot-function-alloc", (swig_guile_proc) _wrap_gsl_multiroot_function_alloc, 2, 0, 0);
    gh_new_procedure("gsl-multiroot-function-free", (swig_guile_proc) _wrap_gsl_multiroot_function_free, 1, 0, 0);
    gh_new_procedure("gsl-multiroot-function-fdf-alloc", (swig_guile_proc) _wrap_gsl_multiroot_function_fdf_alloc, 3, 0, 0);
    gh_new_procedure("gsl-multiroot-function-fdf-free", (swig_guile_proc) _wrap_gsl_multiroot_function_fdf_free, 1, 0, 0);
%}


%scheme %{
(define my-so (dynamic-link "libguile-gsl-multiroots.la"))
(dynamic-call "scm_init_gsl_multiroots_module" my-so)

(export gsl-multiroot-function-alloc gsl-multiroot-function-free
        gsl-multiroot-function-fdf-alloc gsl-multiroot-function-fdf-free)
%}
