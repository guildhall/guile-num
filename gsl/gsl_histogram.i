%module "gsl/gsl-histogram"

%{
#include <stdio.h>
#include <gsl/gsl_histogram.h>
#include <gsl/gsl_histogram2d.h>
%}

%multiple_values;

extern gsl_histogram * gsl_histogram_alloc (size_t N);
extern int gsl_histogram_set_ranges (gsl_histogram * H, const double RANGE[], size_t SIZE);
extern int gsl_histogram_set_ranges_uniform (gsl_histogram * H, double XMIN, double XMAX);
extern void gsl_histogram_free (gsl_histogram * h);

extern int gsl_histogram_memcpy (gsl_histogram * DEST, const gsl_histogram * SRC);
extern gsl_histogram * gsl_histogram_clone (const gsl_histogram * SRC);

extern int gsl_histogram_increment (gsl_histogram * h, double x);
extern int gsl_histogram_accumulate (gsl_histogram * H, double X, double WEIGHT);
extern double gsl_histogram_get (const gsl_histogram * H, size_t I);
extern int gsl_histogram_get_range (const gsl_histogram * H, size_t I, double * LOWER, double * UPPER);
extern double gsl_histogram_max (const gsl_histogram * H);
extern double gsl_histogram_min (const gsl_histogram * H);
extern size_t gsl_histogram_bins (const gsl_histogram * H);
extern void gsl_histogram_reset (gsl_histogram * H);

extern int gsl_histogram_find (const gsl_histogram * H, double X, size_t * OUTPUT);

extern double gsl_histogram_max_val (const gsl_histogram * H);
extern size_t gsl_histogram_max_bin (const gsl_histogram * H);
extern double gsl_histogram_min_val (const gsl_histogram * H);
extern size_t gsl_histogram_min_bin (const gsl_histogram * H);
extern double gsl_histogram_mean (const gsl_histogram * H);
extern double gsl_histogram_sigma (const gsl_histogram * H);
extern double gsl_histogram_sum (const gsl_histogram * H);

extern int gsl_histogram_equal_bins_p (const gsl_histogram *H1, const gsl_histogram *H2);
extern int gsl_histogram_add (gsl_histogram *H1, const gsl_histogram *H2);
extern int gsl_histogram_mul (gsl_histogram *H1, const gsl_histogram *H2);
extern int gsl_histogram_div (gsl_histogram *H1, const gsl_histogram *H2);
extern int gsl_histogram_scale (gsl_histogram *H, double SCALE);
extern int gsl_histogram_shift (gsl_histogram *H, double OFFSET);

extern int gsl_histogram_fwrite (FILE * STREAM, const gsl_histogram * H);
extern int gsl_histogram_fread (FILE * STREAM, gsl_histogram * H);
extern int gsl_histogram_fprintf (FILE * STREAM, const gsl_histogram * H, const char * RANGE_FORMAT, const char * BIN_FORMAT);
extern int gsl_histogram_fscanf (FILE * STREAM, gsl_histogram * H);

extern gsl_histogram_pdf * gsl_histogram_pdf_alloc (size_t n);
extern int gsl_histogram_pdf_init (gsl_histogram_pdf * P, const gsl_histogram * H);
extern void gsl_histogram_pdf_free (gsl_histogram_pdf * P);
extern double gsl_histogram_pdf_sample (const gsl_histogram_pdf * P, double R);

extern gsl_histogram2d * gsl_histogram2d_alloc (size_t NX, size_t NY);
extern int gsl_histogram2d_set_ranges (gsl_histogram2d * H, const double XRANGE[], size_t XSIZE, const double YRANGE[], size_t YSIZE);
extern int gsl_histogram2d_set_ranges_uniform (gsl_histogram2d * H, double XMIN, double XMAX, double YMIN, double YMAX);
extern void gsl_histogram2d_free (gsl_histogram2d * H);

extern int gsl_histogram2d_memcpy (gsl_histogram2d * DEST, const gsl_histogram2d * SRC);
extern gsl_histogram2d * gsl_histogram2d_clone (const gsl_histogram2d * SRC);

extern int gsl_histogram2d_increment (gsl_histogram2d * H, double X, double Y);
extern int gsl_histogram2d_accumulate (gsl_histogram2d * H, double X, double Y, double WEIGHT);
extern double gsl_histogram2d_get (const gsl_histogram2d * H, size_t I, size_t J);
extern int gsl_histogram2d_get_xrange (const gsl_histogram2d * H, size_t I, double * XLOWER, double * XUPPER);
extern int gsl_histogram2d_get_yrange (const gsl_histogram2d * H, size_t J, double * YLOWER, double * YUPPER);
extern double gsl_histogram2d_xmax (const gsl_histogram2d * H);
extern double gsl_histogram2d_xmin (const gsl_histogram2d * H);
extern size_t gsl_histogram2d_nx (const gsl_histogram2d * H);
extern double gsl_histogram2d_ymax (const gsl_histogram2d * H);
extern double gsl_histogram2d_ymin (const gsl_histogram2d * H);
extern size_t gsl_histogram2d_ny (const gsl_histogram2d * H);
extern void gsl_histogram2d_reset (gsl_histogram2d * H);

extern int gsl_histogram2d_find (const gsl_histogram2d * H, double X, double Y, size_t * I, size_t * J);

extern void gsl_histogram2d_max_bin (const gsl_histogram2d * H, size_t * I, size_t * J);
extern double gsl_histogram2d_min_val (const gsl_histogram2d * H);
extern void gsl_histogram2d_min_bin (const gsl_histogram2d * H, size_t * I, size_t * J);
extern double gsl_histogram2d_xmean (const gsl_histogram2d * H);
extern double gsl_histogram2d_ymean (const gsl_histogram2d * H);
extern double gsl_histogram2d_xsigma (const gsl_histogram2d * H);
extern double gsl_histogram2d_ysigma (const gsl_histogram2d * H);
extern double gsl_histogram2d_cov (const gsl_histogram2d * H);
extern double gsl_histogram2d_sum (const gsl_histogram2d * H);

extern int gsl_histogram2d_equal_bins_p (const gsl_histogram2d *H1, const gsl_histogram2d *H2);
extern int gsl_histogram2d_add (gsl_histogram2d *H1, const gsl_histogram2d *H2);
extern int gsl_histogram2d_sub (gsl_histogram2d *H1, const gsl_histogram2d *H2);
extern int gsl_histogram2d_mul (gsl_histogram2d *H1, const gsl_histogram2d *H2);
extern int gsl_histogram2d_div (gsl_histogram2d *H1, const gsl_histogram2d *H2);
extern int gsl_histogram2d_scale (gsl_histogram2d *H, double SCALE);
extern int gsl_histogram2d_shift (gsl_histogram2d *H, double OFFSET);

extern int gsl_histogram2d_fwrite (FILE * STREAM, const gsl_histogram2d * H);
extern int gsl_histogram2d_fread (FILE * STREAM, gsl_histogram2d * H);
extern int gsl_histogram2d_fprintf (FILE * STREAM, const gsl_histogram2d * H, const char * RANGE_FORMAT, const char * BIN_FORMAT);
extern int gsl_histogram2d_fscanf (FILE * STREAM, gsl_histogram2d * H);

extern gsl_histogram2d_pdf * gsl_histogram2d_pdf_alloc (size_t nx, size_t ny);
extern int gsl_histogram2d_pdf_init (gsl_histogram2d_pdf * P, const gsl_histogram2d * H);
extern void gsl_histogram2d_pdf_free (gsl_histogram2d_pdf * P);
extern int gsl_histogram2d_pdf_sample (const gsl_histogram2d_pdf * P, double R1, double R2, double * X, double * Y);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-histogram.so"))
(dynamic-call "SWIG_init" my-so)
%}
