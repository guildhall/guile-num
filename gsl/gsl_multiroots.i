%module "gsl/gsl-multiroots"

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
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_multiroot_function);
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
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_multiroot_function))
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
					   SWIGTYPE_p_gsl_multiroot_function_fdf);
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

        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_multiroot_function_fdf))
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

typedef struct {
	const gsl_multiroot_fsolver_type * type;
	gsl_multiroot_function * function ;
	gsl_vector * x ;
	gsl_vector * f ;
	gsl_vector * dx ;
	void *state;
} gsl_multiroot_fsolver;

typedef struct {
	const gsl_multiroot_fdfsolver_type * type;
	gsl_multiroot_function_fdf * fdf ;
	gsl_vector * x;
	gsl_vector * f;
	gsl_matrix * J;
	gsl_vector * dx;
	void *state;
} gsl_multiroot_fdfsolver;


extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_dnewton;
extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_broyden;
extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_hybrid;
extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_hybrids;

extern gsl_multiroot_fsolver * gsl_multiroot_fsolver_alloc (const gsl_multiroot_fsolver_type * T, size_t N);
extern int gsl_multiroot_fsolver_set (gsl_multiroot_fsolver * S, gsl_multiroot_function * F, gsl_vector * X);
extern void gsl_multiroot_fsolver_free (gsl_multiroot_fsolver * S);
extern const char * gsl_multiroot_fsolver_name (const gsl_multiroot_fsolver * S);

extern int gsl_multiroot_fsolver_iterate (gsl_multiroot_fsolver * S);
extern gsl_vector * gsl_multiroot_fsolver_root (const gsl_multiroot_fsolver * S);
extern gsl_vector * gsl_multiroot_fsolver_f (const gsl_multiroot_fsolver * S);
extern gsl_vector * gsl_multiroot_fsolver_dx (const gsl_multiroot_fsolver * S);

extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_newton;
extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_gnewton;
extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_hybridj;
extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_hybridsj;

extern gsl_multiroot_fdfsolver * gsl_multiroot_fdfsolver_alloc (const gsl_multiroot_fdfsolver_type * T, size_t N);
extern int gsl_multiroot_fdfsolver_set (gsl_multiroot_fdfsolver * S, gsl_multiroot_function_fdf * FDF, gsl_vector * X);
extern void gsl_multiroot_fdfsolver_free (gsl_multiroot_fdfsolver * S);
extern const char * gsl_multiroot_fdfsolver_name (const gsl_multiroot_fdfsolver * S);

extern int gsl_multiroot_fdfsolver_iterate (gsl_multiroot_fdfsolver * S);
extern gsl_vector * gsl_multiroot_fdfsolver_root (const gsl_multiroot_fdfsolver * S);
extern  gsl_vector * gsl_multiroot_fdfsolver_f (const gsl_multiroot_fdfsolver * S);
extern gsl_vector * gsl_multiroot_fdfsolver_dx (const gsl_multiroot_fdfsolver * S);

extern int gsl_multiroot_test_delta (const gsl_vector * DX, const gsl_vector * X, double EPSABS, double EPSREL);
extern int gsl_multiroot_test_residual (const gsl_vector * F, double EPSABS);

void gsl_multiroot_function_eval(gsl_multiroot_function *f, const gsl_vector *x, gsl_vector *y);
void gsl_multiroot_function_eval_f(gsl_multiroot_function_fdf * f, const gsl_vector * x, gsl_vector * y);
void gsl_multiroot_function_eval_df(gsl_multiroot_function_fdf * f, const gsl_vector * x, gsl_matrix * dy);
void gsl_multiroot_function_eval_fdf(gsl_multiroot_function_fdf * f, const gsl_vector * x, gsl_vector * y, gsl_matrix * dy);

%init %{
    gh_new_procedure("gsl-multiroot-function-alloc", (swig_guile_proc) _wrap_gsl_multiroot_function_alloc, 2, 0, 0);
    gh_new_procedure("gsl-multiroot-function-free", (swig_guile_proc) _wrap_gsl_multiroot_function_free, 1, 0, 0);
    gh_new_procedure("gsl-multiroot-function-fdf-alloc", (swig_guile_proc) _wrap_gsl_multiroot_function_fdf_alloc, 3, 0, 0);
    gh_new_procedure("gsl-multiroot-function-fdf-free", (swig_guile_proc) _wrap_gsl_multiroot_function_fdf_free, 1, 0, 0);
%}


%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-multiroots.la"))
(dynamic-call "SWIG_init" my-so)

(export gsl-multiroot-function-alloc gsl-multiroot-function-free
        gsl-multiroot-function-fdf-alloc gsl-multiroot-function-fdf-free)
%}
