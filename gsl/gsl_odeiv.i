%module "gsl/gsl-odeiv"

%multiple_values;

%{
#include <gsl/gsl_errno.h>
#include <gsl/gsl_odeiv.h>


#if 0
static int
_wrap_guile_ode_function (double t,
			  const double y[],
			  double dydt[],
			  void * params)
{
	SCM result;
	SCM proc;
	SCM arg;
	size_t dim;
	size_t i;

	dim = gh_scm2long(SCM_CAR((SCM) params));
	proc = SCM_CADR((SCM) params);
	arg = scm_make_uve(dim, gh_double2scm(1.0));
	for (i = 0; i < dim; i++) {
		scm_array_set_x(arg, gh_double2scm(y[i]),
				gh_long2scm(i));
	}

	result = gh_call2(proc, gh_double2scm(t), arg);

	for (i = 0; i < dim; i++) {
		dydt[i] = scm_uniform_vector_ref(result, gh_int2scm(i));
	}
	return GSL_SUCCESS;
}

static int
_wrap_guile_ode_jacobian (double t,
			  const double y[],
			  double * dfdy,
			  double dfdt[],
			  void * params)
{
	SCM result;
	SCM proc;
	SCM arg;
	size_t dim;
	size_t i, j;

	dim = gh_scm2long(SCM_CAR((SCM) params));
	proc = SCM_CADDR((SCM) params);

	arg = scm_make_uve(dim, gh_double2scm(1.0));
	for (i = 0; i < dim; i++) {
		scm_array_set_x(arg, gh_double2scm(y[i]),
				gh_long2scm(i));
	}
	result = gh_call2(proc, gh_double2scm(t), arg);

	for (i = 0; i < dim; i++) {
		dfdt[i] = scm_uniform_vector_ref(SCM_CAR(result),
						 gh_int2scm(i));
		for (j = 0; j < dim; j++) {
			dfdy[i * dim + j] =
				scm_uniform_vector_ref(SCM_CADR(result),
						       SCM_CONS(gh_long2scm(i),
								gh_long2scm(j)));
		}
	}
	return GSL_SUCCESS;
}
#endif
static SCM
_wrap_gsl_odeiv_system_alloc (SCM s_0, SCM s_1, SCM s_2)
{
    #define FUNC_NAME "gsl-odeiv-system-alloc"
    SCM params;
    gsl_odeiv_system *result;
    SCM gswig_result;
    int gswig_list_p = 0;

    gh_defer_ints();
    result = (gsl_odeiv_system *) malloc(sizeof(gsl_odeiv_system));
#if 0
    result->function = _wrap_guile_ode_function;
    result->jacobian = _wrap_guile_ode_jacobian;
#endif
    result->dimension = gh_scm2int(s_2);
    result->params = (void *) SCM_LIST3(s_2, s_0, s_1);
    
    gh_allow_ints();
    {
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_odeiv_system);
    }
    return gswig_result;
    #undef FUNC_NAME
}


static SCM
_wrap_gsl_odeiv_system_free (SCM s_0)
{
    #define FUNC_NAME "gsl-odeiv-system-free"
    gsl_odeiv_system *arg1 ;
    SCM gswig_result;
    int gswig_list_p = 0;
    
    {
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_odeiv_system))
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

%include "gsl_odeiv.inc"

%init %{
    gh_new_procedure("gsl-odeiv-system-alloc", (swig_guile_proc) _wrap_gsl_odeiv_system_alloc, 3, 0, 0);
    gh_new_procedure("gsl-odeiv-system-free", (swig_guile_proc) _wrap_gsl_odeiv_system_free, 1, 0, 0);
%}


