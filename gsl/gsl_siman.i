%module "gsl/gsl-siman"

%multiple_values;

%{
#include <gsl/gsl_siman.h>

typedef struct siman_params {
	SCM Efunc;
	SCM step;
	SCM distance;
	SCM print;
	SCM copy;
	SCM config;
} siman_params_t;


static double gsl_siman_Efunc (void *xp)
{
	struct siman_params *p;
	SCM result;

	p = (struct siman_params *) xp;
	result = gh_call1(p->Efunc, p->config);
	return gh_scm2double(result);
}

static void gsl_siman_step (const gsl_rng *r,
			    void *xp, double step_size)
{
	struct siman_params *p;
	SCM rng;
	SCM step;
	SCM result;

	p = (struct siman_params *) xp;
	rng = SWIG_Guile_MakePtr ((gsl_rng *) r, SWIGTYPE_p_gsl_rng);
	step = gh_double2scm(step_size);
	result = gh_call3(p->step, p->config, rng, step);
}

static double gsl_siman_metric (void *xp, void *yp)
{
	struct siman_params *p;
	struct siman_params *q;
	SCM result;

	p = (struct siman_params *) xp;
	q = (struct siman_params *) yp;
	result = gh_call2(p->distance, p->config, q->config);
	return gh_scm2double(result);
}

static void gsl_siman_print (void *xp)
{
	struct siman_params *p;

	p = (struct siman_params *) xp;
	gh_call1(p->print, p->config);
}

static void gsl_siman_copy (void *source, void *dest)
{
	struct siman_params *src;
	struct siman_params *dst;

	src = (struct siman_params *) source;
	dst = (struct siman_params *) dest;
	gh_call2(src->copy, dst->config, src->config);
}

static void * gsl_siman_copy_construct (void *xp)
{
	struct siman_params *p;
	struct siman_params *q;
	SCM result;

	p = (struct siman_params *) xp;
	q = (struct siman_params *) malloc(sizeof(struct siman_params));
	memcpy(q, p, sizeof(struct siman_params));

	result = gh_call1(p->copy, p->config);
	q->config = result;

	return (void *) q;
}

static void gsl_siman_destroy (void *xp)
{
	struct siman_params *p;

	p = (struct siman_params *) xp;
	free(p);
}

SCM _wrap_gsl_siman_solve(SCM x0, SCM param_list,
			  SCM rng, SCM Efunc, SCM step, SCM distance,
			  SCM print, SCM copy)
{
#define FUNC_NAME "gsl-siman-solve"
	struct siman_params *gp;
	gsl_rng *r;
	SCM result;
	void *x0_p;
	gsl_siman_params_t params;

	if (SWIG_Guile_GetPtr(rng, (void **) &r, SWIGTYPE_p_gsl_rng))
		scm_wrong_type_arg(FUNC_NAME, 2, rng);

	gp = (struct siman_params *) malloc(sizeof(struct siman_params));
	gp->Efunc = Efunc;
	gp->step = step;
	gp->distance = distance;
	gp->print = print;
	gp->copy = copy;
	gp->config = x0;


	params.n_tries =
		gh_scm2long(scm_list_ref(param_list, gh_long2scm(0)));
	params.iters_fixed_T =
		gh_scm2long(scm_list_ref(param_list, gh_long2scm(1)));
	params.step_size =
		gh_scm2double(scm_list_ref(param_list, gh_long2scm(2)));
	params.k =
		gh_scm2double(scm_list_ref(param_list, gh_long2scm(3)));
	params.t_initial =
		gh_scm2double(scm_list_ref(param_list, gh_long2scm(4)));
	params.mu_t =
		gh_scm2double(scm_list_ref(param_list, gh_long2scm(5)));
	params.t_min =
		gh_scm2double(scm_list_ref(param_list, gh_long2scm(6)));

	x0_p = (void *) gp;
	gsl_siman_solve(r, x0_p,
			gsl_siman_Efunc,
			gsl_siman_step,
			gsl_siman_metric,
			gsl_siman_print,
			gsl_siman_copy,
			gsl_siman_copy_construct,
			gsl_siman_destroy,
			0,
			params);
	gp = (struct siman_params *) x0_p;
	result = gp->config;
	free(gp);

	return result;
#undef FUNC_NAME
}
%}

double gsl_siman_Efunc (void *xp);
void gsl_siman_step (const gsl_rng *r, void *xp, double step_size);
double gsl_siman_metric (void *xp, void *yp);
void gsl_siman_print (void *xp);
void gsl_siman_copy (void *source, void *dest);
void * gsl_siman_copy_construct (void *xp);
void gsl_siman_destroy (void *xp);


%init %{
    gh_new_procedure("gsl-siman-solve", (swig_guile_proc) _wrap_gsl_siman_solve, 8, 0, 0);
%}


%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-siman.la"))
(dynamic-call "SWIG_init" my-so)

(export gsl-siman-solve)
%}
