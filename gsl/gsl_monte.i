%module "gsl/gsl-monte"

%multiple_values;

%{
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_plain.h>
#include <gsl/gsl_monte_miser.h>
#include <gsl/gsl_monte_vegas.h>

static double
_wrap_guile_monte_function (double * x, size_t dim, void *params)
{
	SCM result;
	SCM proc;
	SCM vect;
	size_t i;

	proc = (SCM) params;
	vect = gh_make_vector(gh_int2scm(dim),
			      gh_double2scm(0.0));
	for (i = 0; i < dim; i++) {
		scm_vector_set_x(vect, gh_int2scm(i),
				 gh_double2scm(*(x + i)));
	}

	result = gh_call1(proc, vect);
	return gh_scm2double(result);
}

static SCM
_wrap_gsl_monte_function_alloc (SCM s_0, SCM s_1)
{
    #define FUNC_NAME "gsl-monte-function-alloc"
    SCM params;
    gsl_monte_function *result;
    SCM gswig_result;
    int gswig_list_p = 0;

    gh_defer_ints();
    result = (gsl_monte_function *) malloc(sizeof(gsl_monte_function));
    result->f = _wrap_guile_monte_function;
    result->dim = gh_scm2int(s_1);
    result->params = (void *) s_0;
    
    gh_allow_ints();
    {
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_monte_function_struct);
    }
    return gswig_result;
    #undef FUNC_NAME
}


static SCM
_wrap_gsl_monte_function_free (SCM s_0)
{
    #define FUNC_NAME "gsl-monte-function-free"
    gsl_monte_function *arg1 ;
    SCM gswig_result;
    int gswig_list_p = 0;
    
    {
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_monte_function_struct))
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

%include "gsl_monte.inc"

%init %{
    gh_new_procedure("gsl-monte-function-alloc", (swig_guile_proc) _wrap_gsl_monte_function_alloc, 2, 0, 0);
    gh_new_procedure("gsl-monte-function-free", (swig_guile_proc) _wrap_gsl_monte_function_free, 1, 0, 0);
%}

%scheme %{
(define my-so (dynamic-link "libguile-gsl-monte.la"))
(dynamic-call "SWIG_init" my-so)

(export gsl-monte-function-alloc gsl-monte-function-free)
%}
