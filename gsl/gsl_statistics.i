%module "gsl/gsl-statistics"

%{
#include <gsl/gsl_statistics.h>
%}

%multiple_values;

extern double gsl_stats_mean (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_variance (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_variance_m (const double DATA[], size_t STRIDE, size_t N, double MEAN);
extern double gsl_stats_sd (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_variance_with_fixed_mean (const double DATA[], size_t STRIDE, size_t N, double MEAN);
extern double gsl_stats_sd_with_fixed_mean (const double DATA[], size_t STRIDE, size_t N, double MEAN);

extern double gsl_stats_absdev (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_absdev_m (const double DATA[], size_t STRIDE, size_t N, double MEAN);

extern double gsl_stats_skew (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_skew_m_sd (const double DATA[], size_t STRIDE, size_t N, double MEAN, double SD);
extern double gsl_stats_kurtosis (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_kurtosis_m_sd (const double DATA[], size_t STRIDE, size_t N, double MEAN, double SD);

extern double gsl_stats_lag1_autocorrelation (const double data[], const size_t STRIDE, const size_t N);
extern double gsl_stats_lag1_autocorrelation_m (const double data[], const size_t STRIDE, const size_t N, const double MEAN);

extern double gsl_stats_covariance (const double DATA1[], const size_t STRIDE1, const double data2[], const size_t STRIDE2, const size_t N);
extern double gsl_stats_covariance_m (const double DATA1[], const size_t STRIDE1, const double DATA2[], const size_t STRIDE2, const size_t N, const double MEAN1, const double MEAN2);

extern double gsl_stats_wmean (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_wvariance (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_wvariance_m (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, double WMEAN);
extern double gsl_stats_wsd (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_wsd_m (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, double WMEAN);
extern double gsl_stats_wvariance_with_fixed_mean (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, const double MEAN);
extern double gsl_stats_wsd_with_fixed_mean (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, const double MEAN);
extern double gsl_stats_wabsdev (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_wabsdev_m (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, double WMEAN);
extern double gsl_stats_wskew (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_wskew_m_sd (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, double WMEAN, double WSD);
extern double gsl_stats_wkurtosis (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_wkurtosis_m_sd (const double W[], size_t WSTRIDE, const double DATA[], size_t STRIDE, size_t N, double WMEAN, double WSD);

extern double gsl_stats_max (const double DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_min (const double DATA[], size_t STRIDE, size_t N);
extern  void gsl_stats_minmax (double * OUTPUT, double * OUTPUT, const double DATA[], size_t STRIDE, size_t N);
extern size_t gsl_stats_max_index (const double DATA[], size_t STRIDE, size_t N);
extern size_t gsl_stats_min_index (const double DATA[], size_t STRIDE, size_t N);
extern void gsl_stats_minmax_index (size_t * OUTPUT, size_t * OUTPUT, const double DATA[], size_t STRIDE, size_t N);

extern double gsl_stats_median_from_sorted_data (const double SORTED_DATA[], size_t STRIDE, size_t N);
extern double gsl_stats_quantile_from_sorted_data (const double SORTED_DATA[], size_t STRIDE, size_t N, double F);

%scheme %{
(define my-so (dynamic-link "libguile-gsl-statistics.la"))
(dynamic-call "SWIG_init" my-so)
%}
