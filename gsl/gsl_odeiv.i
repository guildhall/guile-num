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


extern const gsl_odeiv_step_type *gsl_odeiv_step_rk2;
extern const gsl_odeiv_step_type *gsl_odeiv_step_rk4;
extern const gsl_odeiv_step_type *gsl_odeiv_step_rkf45;
extern const gsl_odeiv_step_type *gsl_odeiv_step_rkck;
extern const gsl_odeiv_step_type *gsl_odeiv_step_rk8pd;
extern const gsl_odeiv_step_type *gsl_odeiv_step_rk2imp;
extern const gsl_odeiv_step_type *gsl_odeiv_step_rk4imp;
extern const gsl_odeiv_step_type *gsl_odeiv_step_bsimp;
extern const gsl_odeiv_step_type *gsl_odeiv_step_gear1;
extern const gsl_odeiv_step_type *gsl_odeiv_step_gear2;


extern gsl_odeiv_step * gsl_odeiv_step_alloc (const gsl_odeiv_step_type * T, size_t DIM);
extern int gsl_odeiv_step_reset (gsl_odeiv_step * S);
extern void gsl_odeiv_step_free (gsl_odeiv_step * S);
extern const char * gsl_odeiv_step_name (const gsl_odeiv_step * S);
extern unsigned int gsl_odeiv_step_order (const gsl_odeiv_step * S);
extern int gsl_odeiv_step_apply (gsl_odeiv_step * S, double T, double H, double y[], double yerr[], const double dydt_in[], double dydt_out[], const gsl_odeiv_system * DYDT);

extern gsl_odeiv_control * gsl_odeiv_control_standard_new (double EPS_ABS, double EPS_REL, double A_Y, double A_DYDT);
extern gsl_odeiv_control * gsl_odeiv_control_y_new (double EPS_ABS, double EPS_REL);
extern gsl_odeiv_control * gsl_odeiv_control_yp_new (double EPS_ABS, double EPS_REL);
extern gsl_odeiv_control * gsl_odeiv_control_alloc (const gsl_odeiv_control_type * T);
extern int gsl_odeiv_control_init (gsl_odeiv_control * C, double EPS_ABS, double EPS_REL, double A_Y, double A_DYDT);
extern void gsl_odeiv_control_free (gsl_odeiv_control * C);
extern int gsl_odeiv_control_hadjust (gsl_odeiv_control * C, gsl_odeiv_step * S, const double y0[], const double yerr[], const double dydt[], double * H);
extern const char * gsl_odeiv_control_name (const gsl_odeiv_control * C);

extern gsl_odeiv_evolve * gsl_odeiv_evolve_alloc (size_t DIM);
extern int gsl_odeiv_evolve_apply (gsl_odeiv_evolve * E, gsl_odeiv_control * CON, gsl_odeiv_step * STEP, const gsl_odeiv_system * DYDT, double * T, double T1, double * H, double y[]);
extern int gsl_odeiv_evolve_reset (gsl_odeiv_evolve * E);
extern void gsl_odeiv_evolve_free (gsl_odeiv_evolve * E);

%init %{
    gh_new_procedure("gsl-odeiv-system-alloc", (swig_guile_proc) _wrap_gsl_odeiv_system_alloc, 3, 0, 0);
    gh_new_procedure("gsl-odeiv-system-free", (swig_guile_proc) _wrap_gsl_odeiv_system_free, 1, 0, 0);
%}


