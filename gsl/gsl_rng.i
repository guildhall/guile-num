%module "gsl/gsl-rng"

%{
#include <gsl/gsl_rng.h>

size_t gsl_rng_type_len(void)
{
	const gsl_rng_type **t, **t0;
	size_t len;
          
	t0 = gsl_rng_types_setup ();
	len = 0;
	for (t = t0; *t != 0; t++) {
		len++;
	}
	return len;
}
	

const gsl_rng_type * gsl_rng_type_ref(size_t i)
{
	const gsl_rng_type **t0;
          
	if (i < 0 || i >= gsl_rng_type_len())
		return NULL;

	t0 = gsl_rng_types_setup ();
	return t0[i];
}

const char * gsl_rng_type_name(const gsl_rng_type * t)
{
	return t->name;
}

%}

size_t gsl_rng_type_len(void);
const gsl_rng_type * gsl_rng_type_ref(size_t i);
const char * gsl_rng_type_name(const gsl_rng_type * T);

extern gsl_rng * gsl_rng_alloc (const gsl_rng_type * T);
extern void gsl_rng_set (const gsl_rng * R, unsigned long int S);
extern void gsl_rng_free (gsl_rng * R);
extern unsigned long int gsl_rng_get (const gsl_rng * R);
extern double gsl_rng_uniform (const gsl_rng * R);
extern double gsl_rng_uniform_pos (const gsl_rng * R);
extern unsigned long int gsl_rng_uniform_int (const gsl_rng * R, unsigned long int N);
extern const char * gsl_rng_name (const gsl_rng * R);
extern  unsigned long int gsl_rng_max (const gsl_rng * R);
extern unsigned long int gsl_rng_min (const gsl_rng * R);
extern void * gsl_rng_state (const gsl_rng * R);
extern size_t gsl_rng_size (const gsl_rng * R);
extern const gsl_rng_type ** gsl_rng_types_setup (void);
extern const gsl_rng_type * gsl_rng_env_setup (void);
extern int gsl_rng_memcpy (gsl_rng * DEST, const gsl_rng * SRC);
extern gsl_rng * gsl_rng_clone (const gsl_rng * R);
extern void gsl_rng_print_state (const gsl_rng * R);

extern const gsl_rng_type *gsl_rng_default;
extern const gsl_rng_type *gsl_rng_borosh13;
extern const gsl_rng_type *gsl_rng_coveyou;
extern const gsl_rng_type *gsl_rng_cmrg;
extern const gsl_rng_type *gsl_rng_fishman18;
extern const gsl_rng_type *gsl_rng_fishman20;
extern const gsl_rng_type *gsl_rng_fishman2x;
extern const gsl_rng_type *gsl_rng_gfsr4;
extern const gsl_rng_type *gsl_rng_knuthran;
extern const gsl_rng_type *gsl_rng_knuthran2;
extern const gsl_rng_type *gsl_rng_lecuyer21;
extern const gsl_rng_type *gsl_rng_minstd;
extern const gsl_rng_type *gsl_rng_mrg;
extern const gsl_rng_type *gsl_rng_mt19937;
extern const gsl_rng_type *gsl_rng_mt19937_1999;
extern const gsl_rng_type *gsl_rng_mt19937_1998;
extern const gsl_rng_type *gsl_rng_r250;
extern const gsl_rng_type *gsl_rng_ran0;
extern const gsl_rng_type *gsl_rng_ran1;
extern const gsl_rng_type *gsl_rng_ran2;
extern const gsl_rng_type *gsl_rng_ran3;
extern const gsl_rng_type *gsl_rng_rand;
extern const gsl_rng_type *gsl_rng_rand48;
extern const gsl_rng_type *gsl_rng_random128_bsd;
extern const gsl_rng_type *gsl_rng_random128_glibc2;
extern const gsl_rng_type *gsl_rng_random128_libc5;
extern const gsl_rng_type *gsl_rng_random256_bsd;
extern const gsl_rng_type *gsl_rng_random256_glibc2;
extern const gsl_rng_type *gsl_rng_random256_libc5;
extern const gsl_rng_type *gsl_rng_random32_bsd;
extern const gsl_rng_type *gsl_rng_random32_glibc2;
extern const gsl_rng_type *gsl_rng_random32_libc5;
extern const gsl_rng_type *gsl_rng_random64_bsd;
extern const gsl_rng_type *gsl_rng_random64_glibc2;
extern const gsl_rng_type *gsl_rng_random64_libc5;
extern const gsl_rng_type *gsl_rng_random8_bsd;
extern const gsl_rng_type *gsl_rng_random8_glibc2;
extern const gsl_rng_type *gsl_rng_random8_libc5;
extern const gsl_rng_type *gsl_rng_random_bsd;
extern const gsl_rng_type *gsl_rng_random_glibc2;
extern const gsl_rng_type *gsl_rng_random_libc5;
extern const gsl_rng_type *gsl_rng_randu;
extern const gsl_rng_type *gsl_rng_ranf;
extern const gsl_rng_type *gsl_rng_ranlux;
extern const gsl_rng_type *gsl_rng_ranlux389;
extern const gsl_rng_type *gsl_rng_ranlxd1;
extern const gsl_rng_type *gsl_rng_ranlxd2;
extern const gsl_rng_type *gsl_rng_ranlxs0;
extern const gsl_rng_type *gsl_rng_ranlxs1;
extern const gsl_rng_type *gsl_rng_ranlxs2;
extern const gsl_rng_type *gsl_rng_ranmar;
extern const gsl_rng_type *gsl_rng_slatec;
extern const gsl_rng_type *gsl_rng_taus;
extern const gsl_rng_type *gsl_rng_taus2;
extern const gsl_rng_type *gsl_rng_taus113;
extern const gsl_rng_type *gsl_rng_transputer;
extern const gsl_rng_type *gsl_rng_tt800;
extern const gsl_rng_type *gsl_rng_uni;
extern const gsl_rng_type *gsl_rng_uni32;
extern const gsl_rng_type *gsl_rng_vax;
extern const gsl_rng_type *gsl_rng_waterman14;
extern const gsl_rng_type *gsl_rng_zuf;

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-rng.so"))
(dynamic-call "SWIG_init" my-so)
%}
