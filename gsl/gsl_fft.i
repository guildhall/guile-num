%module "gsl/gsl-fft"

%{
#include <gsl/gsl_fft_complex.h>
#include <gsl/gsl_fft_real.h>
#include <gsl/gsl_fft_halfcomplex.h>
%}

typedef gsl_complex * gsl_complex_packed_array;
typedef enum { forward, backward } gsl_fft_direction;

extern int gsl_fft_complex_radix2_forward (gsl_complex_packed_array data,
					   const size_t stride,
					   const size_t n);
extern int gsl_fft_complex_radix2_backward (gsl_complex_packed_array data,
					    const size_t stride,
					    const size_t n);
extern int gsl_fft_complex_radix2_inverse (gsl_complex_packed_array data,
					   const size_t stride,
					   const size_t n);
extern int gsl_fft_complex_radix2_transform (gsl_complex_packed_array data,
					     const size_t stride,
					     const size_t n,
					     const gsl_fft_direction sign);
extern int gsl_fft_complex_radix2_dif_forward (gsl_complex_packed_array data,
					       const size_t stride,
					       const size_t n);
extern int gsl_fft_complex_radix2_dif_backward (gsl_complex_packed_array data,
						const size_t stride,
						const size_t n);
extern int gsl_fft_complex_radix2_dif_inverse (gsl_complex_packed_array data,
					       const size_t stride,
					       const size_t n);
extern int gsl_fft_complex_radix2_dif_transform (gsl_complex_packed_array data,
						 const size_t stride,
						 const size_t n,
						 const gsl_fft_direction sign);
extern int gsl_fft_complex_bitreverse_order (gsl_complex_packed_array data,
					     size_t stride,
					     size_t n,
					     size_t n_bits);

extern gsl_fft_complex_wavetable * gsl_fft_complex_wavetable_alloc (size_t N);
extern void gsl_fft_complex_wavetable_free (gsl_fft_complex_wavetable * WAVETABLE);
extern gsl_fft_complex_workspace * gsl_fft_complex_workspace_alloc (size_t N);
extern void gsl_fft_complex_workspace_free (gsl_fft_complex_workspace * WORKSPACE);
extern int gsl_fft_complex_forward (gsl_complex_packed_array DATA, size_t STRIDE, size_t N, const gsl_fft_complex_wavetable * WAVETABLE, gsl_fft_complex_workspace * WORK);
int gsl_fft_complex_transform (gsl_complex_packed_array data,
                               const size_t stride, const size_t n,
                               const gsl_fft_complex_wavetable * wavetable,
                               gsl_fft_complex_workspace * work,
                               const gsl_fft_direction sign);
extern int gsl_fft_complex_backward (gsl_complex_packed_array DATA, size_t STRIDE, size_t N, const gsl_fft_complex_wavetable * WAVETABLE, gsl_fft_complex_workspace * WORK);
extern int gsl_fft_complex_inverse (gsl_complex_packed_array DATA, size_t STRIDE, size_t N, const gsl_fft_complex_wavetable * WAVETABLE, gsl_fft_complex_workspace * WORK);

extern int gsl_fft_real_radix2_transform (double DATA[], size_t STRIDE, size_t N);
extern int gsl_fft_halfcomplex_radix2_inverse (double DATA[], size_t STRIDE, size_t N);
extern int gsl_fft_halfcomplex_radix2_backward (double DATA[], size_t STRIDE, size_t N);
extern gsl_fft_real_wavetable * gsl_fft_real_wavetable_alloc (size_t N);
extern gsl_fft_halfcomplex_wavetable * gsl_fft_halfcomplex_wavetable_alloc (size_t N);
extern void gsl_fft_real_wavetable_free (gsl_fft_real_wavetable * WAVETABLE);
extern void gsl_fft_halfcomplex_wavetable_free (gsl_fft_halfcomplex_wavetable * WAVETABLE);
extern gsl_fft_real_workspace * gsl_fft_real_workspace_alloc (size_t N);
extern void gsl_fft_real_workspace_free (gsl_fft_real_workspace * WORKSPACE);
extern int gsl_fft_real_transform (double DATA[], size_t STRIDE, size_t N, const gsl_fft_real_wavetable * WAVETABLE, gsl_fft_real_workspace * WORK);
extern int gsl_fft_halfcomplex_transform (double DATA[], size_t STRIDE, size_t N, const gsl_fft_halfcomplex_wavetable * WAVETABLE, gsl_fft_real_workspace * WORK);
extern int gsl_fft_real_unpack (const double REAL_COEFFICIENT[], gsl_complex_packed_array COMPLEX_COEFFICIENT, size_t STRIDE, size_t N);
extern int gsl_fft_halfcomplex_unpack (const double HALFCOMPLEX_COEFFICIENT[], gsl_complex_packed_array COMPLEX_COEFFICIENT, size_t STRIDE, size_t N);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-fft.la"))
(dynamic-call "SWIG_init" my-so)
%}
