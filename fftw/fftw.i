%module "fftw/fftw"

/*
 * Copyright (c) 2003 Matteo Frigo
 * Copyright (c) 2003 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

%{
#include <fftw3.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_vector.h>

static fftw_complex* fftw_vector_alloc(size_t n)
{
	return fftw_malloc(sizeof(fftw_complex) * n);
}
			

static void copy_gsl_vector_complex_fftw_complex(fftw_complex *w, gsl_vector_complex *v)
{
	memcpy(w, v->data, sizeof(fftw_complex) * v->size);
}

static void copy_fftw_complex_gsl_vector_complex(gsl_vector_complex *v, fftw_complex *w)
{
	memcpy(v->data, w, sizeof(fftw_complex) * v->size);
}


%}

enum fftw_r2r_kind_do_not_use_me {
     FFTW_R2HC=0, FFTW_HC2R=1, FFTW_DHT=2,
     FFTW_REDFT00=3, FFTW_REDFT01=4, FFTW_REDFT10=5, FFTW_REDFT11=6,
     FFTW_RODFT00=7, FFTW_RODFT01=8, FFTW_RODFT10=9, FFTW_RODFT11=10
};

struct fftw_iodim_do_not_use_me {
     int n;
     int is;
     int os;
};

typedef double fftw_complex[2];
typedef struct fftw_plan_s *fftw_plan;
typedef struct fftw_iodim_do_not_use_me fftw_iodim;
typedef enum fftw_r2r_kind_do_not_use_me fftw_r2r_kind;
extern void fftw_execute(const fftw_plan p);
extern fftw_plan fftw_plan_dft(int rank, const int *n, fftw_complex *in, fftw_complex *out, int sign, unsigned flags);
extern fftw_plan fftw_plan_dft_1d(int n, fftw_complex *in, fftw_complex *out, int sign, unsigned flags);
extern fftw_plan fftw_plan_dft_2d(int nx, int ny, fftw_complex *in, fftw_complex *out, int sign, unsigned flags);
extern fftw_plan fftw_plan_dft_3d(int nx, int ny, int nz, fftw_complex *in, fftw_complex *out, int sign, unsigned flags);
extern fftw_plan fftw_plan_many_dft(int rank, const int *n, int howmany, fftw_complex *in, const int *inembed, int istride, int idist, fftw_complex *out, const int *onembed, int ostride, int odist, int sign, unsigned flags);
extern fftw_plan fftw_plan_guru_dft(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, fftw_complex *in, fftw_complex *out, int sign, unsigned flags);
extern fftw_plan fftw_plan_guru_split_dft(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, double *ri, double *ii, double *ro, double *io, unsigned flags);
extern void fftw_execute_dft(const fftw_plan p, fftw_complex *in, fftw_complex *out);
extern void fftw_execute_split_dft(const fftw_plan p, double *ri, double *ii, double *ro, double *io);
extern fftw_plan fftw_plan_many_dft_r2c(int rank, const int *n, int howmany, double *in, const int *inembed, int istride, int idist, fftw_complex *out, const int *onembed, int ostride, int odist, unsigned flags);
extern fftw_plan fftw_plan_dft_r2c(int rank, const int *n, double *in, fftw_complex *out, unsigned flags);
extern fftw_plan fftw_plan_dft_r2c_1d(int n,double *in,fftw_complex *out,unsigned flags);
extern fftw_plan fftw_plan_dft_r2c_2d(int nx, int ny, double *in, fftw_complex *out, unsigned flags);
extern fftw_plan fftw_plan_dft_r2c_3d(int nx, int ny, int nz, double *in, fftw_complex *out, unsigned flags);
extern fftw_plan fftw_plan_many_dft_c2r(int rank, const int *n, int howmany, fftw_complex *in, const int *inembed, int istride, int idist, double *out, const int *onembed, int ostride, int odist, unsigned flags);
extern fftw_plan fftw_plan_dft_c2r(int rank, const int *n, fftw_complex *in, double *out, unsigned flags);
extern fftw_plan fftw_plan_dft_c2r_1d(int n,fftw_complex *in,double *out,unsigned flags);
extern fftw_plan fftw_plan_dft_c2r_2d(int nx, int ny, fftw_complex *in, double *out, unsigned flags);
extern fftw_plan fftw_plan_dft_c2r_3d(int nx, int ny, int nz, fftw_complex *in, double *out, unsigned flags);
extern fftw_plan fftw_plan_guru_dft_r2c(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, double *in, fftw_complex *out, unsigned flags);
extern fftw_plan fftw_plan_guru_dft_c2r(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, fftw_complex *in, double *out, unsigned flags);
extern fftw_plan fftw_plan_guru_split_dft_r2c(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, double *in, double *ro, double *io, unsigned flags);
extern fftw_plan fftw_plan_guru_split_dft_c2r(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, double *ri, double *ii, double *out, unsigned flags);
extern void fftw_execute_dft_r2c(const fftw_plan p, double *in, fftw_complex *out);
extern void fftw_execute_dft_c2r(const fftw_plan p, fftw_complex *in, double *out);
extern void fftw_execute_split_dft_r2c(const fftw_plan p, double *in, double *ro, double *io);
extern void fftw_execute_split_dft_c2r(const fftw_plan p, double *ri, double *ii, double *out);
extern fftw_plan fftw_plan_many_r2r(int rank, const int *n, int howmany, double *in, const int *inembed, int istride, int idist, double *out, const int *onembed, int ostride, int odist, const fftw_r2r_kind *kind, unsigned flags);
extern fftw_plan fftw_plan_r2r(int rank, const int *n, double *in, double *out, const fftw_r2r_kind *kind, unsigned flags);
extern fftw_plan fftw_plan_r2r_1d(int n, double *in, double *out, fftw_r2r_kind kind, unsigned flags);
extern fftw_plan fftw_plan_r2r_2d(int nx, int ny, double *in, double *out, fftw_r2r_kind kindx, fftw_r2r_kind kindy, unsigned flags);
extern fftw_plan fftw_plan_r2r_3d(int nx, int ny, int nz, double *in, double *out, fftw_r2r_kind kindx, fftw_r2r_kind kindy, fftw_r2r_kind kindz, unsigned flags);
extern fftw_plan fftw_plan_guru_r2r(int rank, const fftw_iodim *dims, int howmany_rank, const fftw_iodim *howmany_dims, double *in, double *out, const fftw_r2r_kind *kind, unsigned flags);
extern void fftw_execute_r2r(const fftw_plan p, double *in, double *out);
extern void fftw_destroy_plan(fftw_plan p);
extern void fftw_forget_wisdom(void);
extern void fftw_cleanup(void);
extern void fftw_plan_with_nthreads(int nthreads);
extern int fftw_init_threads(void);
extern void fftw_cleanup_threads(void);
extern void fftw_export_wisdom_to_file(FILE *output_file);
extern char *fftw_export_wisdom_to_string(void);
extern void fftw_export_wisdom(void (*write_char)(char c, void *), void *data);
extern int fftw_import_system_wisdom(void);
extern int fftw_import_wisdom_from_file(FILE *input_file);
extern int fftw_import_wisdom_from_string(const char *input_string);
extern int fftw_import_wisdom(int (*read_char)(void *), void *data);
extern void fftw_fprint_plan(const fftw_plan p, FILE *output_file);
extern void fftw_print_plan(const fftw_plan p);
extern void *fftw_malloc(size_t n);
extern void fftw_free(void *p);
extern void fftw_flops(const fftw_plan p, double *add, double *mul, double *fma);
extern const char fftw_version[];
extern const char fftw_cc[];
extern const char fftw_codelet_optim[];
typedef float fftwf_complex[2];
typedef struct fftwf_plan_s *fftwf_plan;
typedef struct fftw_iodim_do_not_use_me fftwf_iodim;
typedef enum fftw_r2r_kind_do_not_use_me fftwf_r2r_kind;
extern void fftwf_execute(const fftwf_plan p);
extern fftwf_plan fftwf_plan_dft(int rank, const int *n, fftwf_complex *in, fftwf_complex *out, int sign, unsigned flags);
extern fftwf_plan fftwf_plan_dft_1d(int n, fftwf_complex *in, fftwf_complex *out, int sign, unsigned flags);
extern fftwf_plan fftwf_plan_dft_2d(int nx, int ny, fftwf_complex *in, fftwf_complex *out, int sign, unsigned flags);
extern fftwf_plan fftwf_plan_dft_3d(int nx, int ny, int nz, fftwf_complex *in, fftwf_complex *out, int sign, unsigned flags);
extern fftwf_plan fftwf_plan_many_dft(int rank, const int *n, int howmany, fftwf_complex *in, const int *inembed, int istride, int idist, fftwf_complex *out, const int *onembed, int ostride, int odist, int sign, unsigned flags);
extern fftwf_plan fftwf_plan_guru_dft(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, fftwf_complex *in, fftwf_complex *out, int sign, unsigned flags);
extern fftwf_plan fftwf_plan_guru_split_dft(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, float *ri, float *ii, float *ro, float *io, unsigned flags);
extern void fftwf_execute_dft(const fftwf_plan p, fftwf_complex *in, fftwf_complex *out);
extern void fftwf_execute_split_dft(const fftwf_plan p, float *ri, float *ii, float *ro, float *io);
extern fftwf_plan fftwf_plan_many_dft_r2c(int rank, const int *n, int howmany, float *in, const int *inembed, int istride, int idist, fftwf_complex *out, const int *onembed, int ostride, int odist, unsigned flags);
extern fftwf_plan fftwf_plan_dft_r2c(int rank, const int *n, float *in, fftwf_complex *out, unsigned flags);
extern fftwf_plan fftwf_plan_dft_r2c_1d(int n,float *in,fftwf_complex *out,unsigned flags);
extern fftwf_plan fftwf_plan_dft_r2c_2d(int nx, int ny, float *in, fftwf_complex *out, unsigned flags);
extern fftwf_plan fftwf_plan_dft_r2c_3d(int nx, int ny, int nz, float *in, fftwf_complex *out, unsigned flags);
extern fftwf_plan fftwf_plan_many_dft_c2r(int rank, const int *n, int howmany, fftwf_complex *in, const int *inembed, int istride, int idist, float *out, const int *onembed, int ostride, int odist, unsigned flags);
extern fftwf_plan fftwf_plan_dft_c2r(int rank, const int *n, fftwf_complex *in, float *out, unsigned flags);
extern fftwf_plan fftwf_plan_dft_c2r_1d(int n,fftwf_complex *in,float *out,unsigned flags);
extern fftwf_plan fftwf_plan_dft_c2r_2d(int nx, int ny, fftwf_complex *in, float *out, unsigned flags);
extern fftwf_plan fftwf_plan_dft_c2r_3d(int nx, int ny, int nz, fftwf_complex *in, float *out, unsigned flags);
extern fftwf_plan fftwf_plan_guru_dft_r2c(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, float *in, fftwf_complex *out, unsigned flags);
extern fftwf_plan fftwf_plan_guru_dft_c2r(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, fftwf_complex *in, float *out, unsigned flags);
extern fftwf_plan fftwf_plan_guru_split_dft_r2c(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, float *in, float *ro, float *io, unsigned flags);
extern fftwf_plan fftwf_plan_guru_split_dft_c2r(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, float *ri, float *ii, float *out, unsigned flags);
extern void fftwf_execute_dft_r2c(const fftwf_plan p, float *in, fftwf_complex *out);
extern void fftwf_execute_dft_c2r(const fftwf_plan p, fftwf_complex *in, float *out);
extern void fftwf_execute_split_dft_r2c(const fftwf_plan p, float *in, float *ro, float *io);
extern void fftwf_execute_split_dft_c2r(const fftwf_plan p, float *ri, float *ii, float *out);
extern fftwf_plan fftwf_plan_many_r2r(int rank, const int *n, int howmany, float *in, const int *inembed, int istride, int idist, float *out, const int *onembed, int ostride, int odist, const fftwf_r2r_kind *kind, unsigned flags);
extern fftwf_plan fftwf_plan_r2r(int rank, const int *n, float *in, float *out, const fftwf_r2r_kind *kind, unsigned flags);
extern fftwf_plan fftwf_plan_r2r_1d(int n, float *in, float *out, fftwf_r2r_kind kind, unsigned flags);
extern fftwf_plan fftwf_plan_r2r_2d(int nx, int ny, float *in, float *out, fftwf_r2r_kind kindx, fftwf_r2r_kind kindy, unsigned flags);
extern fftwf_plan fftwf_plan_r2r_3d(int nx, int ny, int nz, float *in, float *out, fftwf_r2r_kind kindx, fftwf_r2r_kind kindy, fftwf_r2r_kind kindz, unsigned flags);
extern fftwf_plan fftwf_plan_guru_r2r(int rank, const fftwf_iodim *dims, int howmany_rank, const fftwf_iodim *howmany_dims, float *in, float *out, const fftwf_r2r_kind *kind, unsigned flags);
extern void fftwf_execute_r2r(const fftwf_plan p, float *in, float *out);
extern void fftwf_destroy_plan(fftwf_plan p);
extern void fftwf_forget_wisdom(void);
extern void fftwf_cleanup(void);
extern void fftwf_plan_with_nthreads(int nthreads);
extern int fftwf_init_threads(void);
extern void fftwf_cleanup_threads(void);
extern void fftwf_export_wisdom_to_file(FILE *output_file);
extern char *fftwf_export_wisdom_to_string(void);
extern void fftwf_export_wisdom(void (*write_char)(char c, void *), void *data);
extern int fftwf_import_system_wisdom(void);
extern int fftwf_import_wisdom_from_file(FILE *input_file);
extern int fftwf_import_wisdom_from_string(const char *input_string);
extern int fftwf_import_wisdom(int (*read_char)(void *), void *data);
extern void fftwf_fprint_plan(const fftwf_plan p, FILE *output_file);
extern void fftwf_print_plan(const fftwf_plan p);
extern void *fftwf_malloc(size_t n);
extern void fftwf_free(void *p);
extern void fftwf_flops(const fftwf_plan p, double *add, double *mul, double *fma);
typedef long double fftwl_complex[2];
typedef struct fftwl_plan_s *fftwl_plan;
typedef struct fftw_iodim_do_not_use_me fftwl_iodim;
typedef enum fftw_r2r_kind_do_not_use_me fftwl_r2r_kind;
extern void fftwl_execute(const fftwl_plan p);
extern fftwl_plan fftwl_plan_dft(int rank, const int *n, fftwl_complex *in, fftwl_complex *out, int sign, unsigned flags);
extern fftwl_plan fftwl_plan_dft_1d(int n, fftwl_complex *in, fftwl_complex *out, int sign, unsigned flags);
extern fftwl_plan fftwl_plan_dft_2d(int nx, int ny, fftwl_complex *in, fftwl_complex *out, int sign, unsigned flags);
extern fftwl_plan fftwl_plan_dft_3d(int nx, int ny, int nz, fftwl_complex *in, fftwl_complex *out, int sign, unsigned flags);
extern fftwl_plan fftwl_plan_many_dft(int rank, const int *n, int howmany, fftwl_complex *in, const int *inembed, int istride, int idist, fftwl_complex *out, const int *onembed, int ostride, int odist, int sign, unsigned flags);
extern fftwl_plan fftwl_plan_guru_dft(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, fftwl_complex *in, fftwl_complex *out, int sign, unsigned flags);
extern fftwl_plan fftwl_plan_guru_split_dft(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, long double *ri, long double *ii, long double *ro, long double *io, unsigned flags);
extern void fftwl_execute_dft(const fftwl_plan p, fftwl_complex *in, fftwl_complex *out);
extern void fftwl_execute_split_dft(const fftwl_plan p, long double *ri, long double *ii, long double *ro, long double *io);
extern fftwl_plan fftwl_plan_many_dft_r2c(int rank, const int *n, int howmany, long double *in, const int *inembed, int istride, int idist, fftwl_complex *out, const int *onembed, int ostride, int odist, unsigned flags);
extern fftwl_plan fftwl_plan_dft_r2c(int rank, const int *n, long double *in, fftwl_complex *out, unsigned flags);
extern fftwl_plan fftwl_plan_dft_r2c_1d(int n,long double *in,fftwl_complex *out,unsigned flags);
extern fftwl_plan fftwl_plan_dft_r2c_2d(int nx, int ny, long double *in, fftwl_complex *out, unsigned flags);
extern fftwl_plan fftwl_plan_dft_r2c_3d(int nx, int ny, int nz, long double *in, fftwl_complex *out, unsigned flags);
extern fftwl_plan fftwl_plan_many_dft_c2r(int rank, const int *n, int howmany, fftwl_complex *in, const int *inembed, int istride, int idist, long double *out, const int *onembed, int ostride, int odist, unsigned flags);
extern fftwl_plan fftwl_plan_dft_c2r(int rank, const int *n, fftwl_complex *in, long double *out, unsigned flags);
extern fftwl_plan fftwl_plan_dft_c2r_1d(int n,fftwl_complex *in,long double *out,unsigned flags);
extern fftwl_plan fftwl_plan_dft_c2r_2d(int nx, int ny, fftwl_complex *in, long double *out, unsigned flags);
extern fftwl_plan fftwl_plan_dft_c2r_3d(int nx, int ny, int nz, fftwl_complex *in, long double *out, unsigned flags);
extern fftwl_plan fftwl_plan_guru_dft_r2c(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, long double *in, fftwl_complex *out, unsigned flags);
extern fftwl_plan fftwl_plan_guru_dft_c2r(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, fftwl_complex *in, long double *out, unsigned flags);
extern fftwl_plan fftwl_plan_guru_split_dft_r2c(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, long double *in, long double *ro, long double *io, unsigned flags);
extern fftwl_plan fftwl_plan_guru_split_dft_c2r(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, long double *ri, long double *ii, long double *out, unsigned flags);
extern void fftwl_execute_dft_r2c(const fftwl_plan p, long double *in, fftwl_complex *out);
extern void fftwl_execute_dft_c2r(const fftwl_plan p, fftwl_complex *in, long double *out);
extern void fftwl_execute_split_dft_r2c(const fftwl_plan p, long double *in, long double *ro, long double *io);
extern void fftwl_execute_split_dft_c2r(const fftwl_plan p, long double *ri, long double *ii, long double *out);
extern fftwl_plan fftwl_plan_many_r2r(int rank, const int *n, int howmany, long double *in, const int *inembed, int istride, int idist, long double *out, const int *onembed, int ostride, int odist, const fftwl_r2r_kind *kind, unsigned flags);
extern fftwl_plan fftwl_plan_r2r(int rank, const int *n, long double *in, long double *out, const fftwl_r2r_kind *kind, unsigned flags);
extern fftwl_plan fftwl_plan_r2r_1d(int n, long double *in, long double *out, fftwl_r2r_kind kind, unsigned flags);
extern fftwl_plan fftwl_plan_r2r_2d(int nx, int ny, long double *in, long double *out, fftwl_r2r_kind kindx, fftwl_r2r_kind kindy, unsigned flags);
extern fftwl_plan fftwl_plan_r2r_3d(int nx, int ny, int nz, long double *in, long double *out, fftwl_r2r_kind kindx, fftwl_r2r_kind kindy, fftwl_r2r_kind kindz, unsigned flags);
extern fftwl_plan fftwl_plan_guru_r2r(int rank, const fftwl_iodim *dims, int howmany_rank, const fftwl_iodim *howmany_dims, long double *in, long double *out, const fftwl_r2r_kind *kind, unsigned flags);
extern void fftwl_execute_r2r(const fftwl_plan p, long double *in, long double *out);
extern void fftwl_destroy_plan(fftwl_plan p);
extern void fftwl_forget_wisdom(void);
extern void fftwl_cleanup(void);
extern void fftwl_plan_with_nthreads(int nthreads);
extern int fftwl_init_threads(void);
extern void fftwl_cleanup_threads(void);
extern void fftwl_export_wisdom_to_file(FILE *output_file);
extern char *fftwl_export_wisdom_to_string(void);
extern void fftwl_export_wisdom(void (*write_char)(char c, void *), void *data);
extern int fftwl_import_system_wisdom(void);
extern int fftwl_import_wisdom_from_file(FILE *input_file);
extern int fftwl_import_wisdom_from_string(const char *input_string);
extern int fftwl_import_wisdom(int (*read_char)(void *), void *data);
extern void fftwl_fprint_plan(const fftwl_plan p, FILE *output_file);
extern void fftwl_print_plan(const fftwl_plan p);
extern void *fftwl_malloc(size_t n);
extern void fftwl_free(void *p);
extern void fftwl_flops(const fftwl_plan p, double *add, double *mul, double *fma);

#define FFTW_FORWARD (-1)
#define FFTW_BACKWARD (+1)

/* documented flags */
#define FFTW_MEASURE (0U)
#define FFTW_DESTROY_INPUT (1U << 0)
#define FFTW_UNALIGNED (1U << 1)
#define FFTW_CONSERVE_MEMORY (1U << 2)
#define FFTW_EXHAUSTIVE (1U << 3) /* NO_EXHAUSTIVE is default */
#define FFTW_PRESERVE_INPUT (1U << 4) /* cancels FFTW_DESTROY_INPUT */
#define FFTW_PATIENT (1U << 5) /* IMPATIENT is default */
#define FFTW_ESTIMATE (1U << 6)

/* undocumented beyond-guru flags */
#define FFTW_ESTIMATE_PATIENT (1U << 7)
#define FFTW_BELIEVE_PCOST (1U << 8)
#define FFTW_DFT_R2HC_ICKY (1U << 9)
#define FFTW_NONTHREADED_ICKY (1U << 10)
#define FFTW_NO_BUFFERING (1U << 11)
#define FFTW_NO_INDIRECT_OP (1U << 12)
#define FFTW_ALLOW_LARGE_GENERIC (1U << 13) /* NO_LARGE_GENERIC is default */
#define FFTW_NO_RANK_SPLITS (1U << 14)
#define FFTW_NO_VRANK_SPLITS (1U << 15)
#define FFTW_NO_VRECURSE (1U << 16)

#define FFTW_NO_SIMD (1U << 17)


static fftw_complex* fftw_vector_alloc(size_t n);
static void copy_gsl_vector_complex_fftw_complex(fftw_complex *w, gsl_vector_complex *v);
static void copy_fftw_complex_gsl_vector_complex(gsl_vector_complex *v, fftw_complex *w);

%scheme %{
(define my-so (dynamic-link "libguile-fftw.la"))
(dynamic-call "scm_init_fftw_fftw_module" my-so)
%}
