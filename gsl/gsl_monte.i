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
        gswig_result = SWIG_Guile_MakePtr (result, SWIGTYPE_p_gsl_monte_function);
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
        if (SWIG_Guile_GetPtr(s_0, (void **) &arg1, SWIGTYPE_p_gsl_monte_function))
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

%init %{
    gh_new_procedure("gsl-monte-function-alloc", (swig_guile_proc) _wrap_gsl_monte_function_alloc, 2, 0, 0);
    gh_new_procedure("gsl-monte-function-free", (swig_guile_proc) _wrap_gsl_monte_function_free, 1, 0, 0);
%}

extern gsl_monte_plain_state * gsl_monte_plain_alloc (size_t DIM);
extern int gsl_monte_plain_init (gsl_monte_plain_state* S);
extern int gsl_monte_plain_integrate (const gsl_monte_function * F, const double XL[], const double XU[], size_t DIM, size_t CALLS, gsl_rng * R, gsl_monte_plain_state * S, double * OUTPUT, double * OUTPUT);
extern void gsl_monte_plain_free (gsl_monte_plain_state* S);

typedef struct {
    size_t min_calls;
    size_t min_calls_per_bisection;
    double dither;
    double estimate_frac;
    double alpha;
    size_t dim;
    int estimate_style;
    int depth;
    int verbose;
    double * x;
    double * xmid;
    double * sigma_l;
    double * sigma_r;
    double * fmax_l;
    double * fmax_r;
    double * fmin_l;
    double * fmin_r;
    double * fsum_l;
    double * fsum_r;
    double * fsum2_l;
    double * fsum2_r;
    size_t * hits_l;
    size_t * hits_r;
} gsl_monte_miser_state; 

extern gsl_monte_miser_state * gsl_monte_miser_alloc (size_t DIM);
extern int gsl_monte_miser_init (gsl_monte_miser_state* S);
extern int gsl_monte_miser_integrate (gsl_monte_function * F, const double XL[], const double XU[], size_t DIM, size_t CALLS, gsl_rng * R, gsl_monte_miser_state * S, double * OUTPUT, double * OUTPUT);
extern void gsl_monte_miser_free (gsl_monte_miser_state* S);

typedef struct {
    /* grid */
    size_t dim;
    size_t bins_max;
    unsigned int bins;
    unsigned int boxes; /* these are both counted along the axes */
    double * xi;
    double * xin;
    double * delx;
    double * weight;
    double vol;

    double * x;
    int * bin;
    int * box;

    /* distribution */
    double * d;

    /* control variables */
    double alpha;
    int mode;
    int verbose;
    unsigned int iterations;
    int stage;

    /* scratch variables preserved between calls to vegas1/2/3  */
    double jac;
    double wtd_int_sum; 
    double sum_wgts;
    double chi_sum;
    double chisq;

    double result;
    double sigma;

    unsigned int it_start;
    unsigned int it_num;
    unsigned int samples;
    unsigned int calls_per_box;

    FILE * ostream;
} gsl_monte_vegas_state;

enum {
	GSL_VEGAS_MODE_IMPORTANCE,
	GSL_VEGAS_MODE_STRATIFIED,
	GSL_VEGAS_MODE_IMPORTANCE_ONLY
};

extern gsl_monte_vegas_state * gsl_monte_vegas_alloc (size_t DIM);
extern int gsl_monte_vegas_init (gsl_monte_vegas_state* S);
extern int gsl_monte_vegas_integrate (gsl_monte_function * F, double * XL, double * XU, size_t DIM, size_t CALLS, gsl_rng * R, gsl_monte_vegas_state * S, double * OUTPUT, double * OUTPUT);
extern void gsl_monte_vegas_free (gsl_monte_vegas_state* S);

