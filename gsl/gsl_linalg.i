%module "gsl/gsl-linalg"

%{
#include <gsl/gsl_linalg.h>
%}

extern int gsl_linalg_LU_decomp (gsl_matrix * A, gsl_permutation * P, int *SIGNUM);
extern int gsl_linalg_complex_LU_decomp (gsl_matrix_complex * A, gsl_permutation * P, int *SIGNUM);
extern int gsl_linalg_LU_solve (const gsl_matrix * LU, const gsl_permutation * P, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_complex_LU_solve (const gsl_matrix_complex * LU, const gsl_permutation * P, const gsl_vector_complex * B, gsl_vector_complex * X);
extern int gsl_linalg_LU_svx (const gsl_matrix * LU, const gsl_permutation * P, gsl_vector * X);
extern int gsl_linalg_complex_LU_svx (const gsl_matrix_complex * LU, const gsl_permutation * P, gsl_vector_complex * X);
extern int gsl_linalg_LU_refine (const gsl_matrix * A, const gsl_matrix * LU, const gsl_permutation * P, const gsl_vector * B, gsl_vector * X, gsl_vector * RESIDUAL);
extern int gsl_linalg_complex_LU_refine (const gsl_matrix_complex * A, const gsl_matrix_complex * LU, const gsl_permutation * P, const gsl_vector_complex * B, gsl_vector_complex * X, gsl_vector_complex * RESIDUAL);
extern int gsl_linalg_LU_invert (const gsl_matrix * LU, const gsl_permutation * P, gsl_matrix * INVERSE);
extern int gsl_complex_linalg_LU_invert (const gsl_matrix_complex * LU, const gsl_permutation * P, gsl_matrix_complex * INVERSE);
extern double gsl_linalg_LU_det (gsl_matrix * LU, int SIGNUM);
extern gsl_complex gsl_linalg_complex_LU_det (gsl_matrix_complex * LU, int SIGNUM);
extern double gsl_linalg_LU_lndet (gsl_matrix * LU);
extern double gsl_linalg_complex_LU_lndet (gsl_matrix_complex * LU);
extern int gsl_linalg_LU_sgndet (gsl_matrix * LU, int SIGNUM);
extern gsl_complex gsl_linalg_complex_LU_sgndet (gsl_matrix_complex * LU, int SIGNUM);
extern int gsl_linalg_QR_solve (const gsl_matrix * QR, const gsl_vector * TAU, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_QR_svx (const gsl_matrix * QR, const gsl_vector * TAU, gsl_vector * X);
extern int gsl_linalg_QR_lssolve (const gsl_matrix * QR, const gsl_vector * TAU, const gsl_vector * B, gsl_vector * X, gsl_vector * RESIDUAL);
extern int gsl_linalg_QR_decomp (gsl_matrix * A, gsl_vector * TAU);
extern int gsl_linalg_QR_QTvec (const gsl_matrix * QR, const gsl_vector * TAU, gsl_vector * V);
extern int gsl_linalg_QR_Qvec (const gsl_matrix * QR, const gsl_vector * TAU, gsl_vector * V);
extern int gsl_linalg_QR_Rsolve (const gsl_matrix * QR, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_QR_Rsvx (const gsl_matrix * QR, gsl_vector * X);
extern int gsl_linalg_QR_unpack (const gsl_matrix * QR, const gsl_vector * TAU, gsl_matrix * Q, gsl_matrix * R);
extern int gsl_linalg_QR_QRsolve (gsl_matrix * Q, gsl_matrix * R, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_QR_update (gsl_matrix * Q, gsl_matrix * R, gsl_vector * W, const gsl_vector * V);
extern int gsl_linalg_R_solve (const gsl_matrix * R, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_R_svx (const gsl_matrix * R, gsl_vector * X);

extern int gsl_linalg_QRPT_decomp (gsl_matrix * A, gsl_vector * TAU, gsl_permutation * P, int *SIGNUM, gsl_vector * NORM);
extern int gsl_linalg_QRPT_decomp2 (const gsl_matrix * A, gsl_matrix * Q, gsl_matrix * R, gsl_vector * TAU, gsl_permutation * P, int *SIGNUM, gsl_vector * NORM);
extern int gsl_linalg_QRPT_solve (const gsl_matrix * QR, const gsl_vector * TAU, const gsl_permutation * P, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_QRPT_svx (const gsl_matrix * QR, const gsl_vector * TAU, const gsl_permutation * P, gsl_vector * X);
extern int gsl_linalg_QRPT_QRsolve (const gsl_matrix * Q, const gsl_matrix * R, const gsl_permutation * P, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_QRPT_update (gsl_matrix * Q, gsl_matrix * R, const gsl_permutation * P, gsl_vector * U, const gsl_vector * V);
extern  int gsl_linalg_QRPT_Rsolve (const gsl_matrix * QR, const gsl_permutation * P, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_QRPT_Rsvx (const gsl_matrix * QR, const gsl_permutation * P, gsl_vector * X);

extern int gsl_linalg_SV_decomp (gsl_matrix * A, gsl_matrix * V, gsl_vector * S, gsl_vector * WORK);
extern int gsl_linalg_SV_decomp_mod (gsl_matrix * A, gsl_matrix * X, gsl_matrix * V, gsl_vector * S, gsl_vector * WORK);
extern int gsl_linalg_SV_decomp_jacobi (gsl_matrix * A, gsl_matrix * V, gsl_vector * S);
extern int gsl_linalg_SV_solve (const gsl_matrix * U, const gsl_matrix * Q, const gsl_vector * S, const gsl_vector * B, gsl_vector * X);


extern int gsl_linalg_cholesky_decomp (gsl_matrix * A);
extern int gsl_linalg_cholesky_solve (const gsl_matrix * CHOLESKY, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_cholesky_svx (const gsl_matrix * CHOLESKY, gsl_vector * X);

extern int gsl_linalg_symmtd_decomp (gsl_matrix * A, gsl_vector * TAU);
extern int gsl_linalg_symmtd_unpack (const gsl_matrix * A, const gsl_vector * TAU, gsl_matrix * Q, gsl_vector * DIAG, gsl_vector * SUBDIAG);
extern  int gsl_linalg_symmtd_unpack_T (const gsl_matrix * A, gsl_vector * DIAG, gsl_vector * SUBDIAG);

extern int gsl_linalg_hermtd_decomp (gsl_matrix_complex * A, gsl_vector_complex * TAU);
extern int gsl_linalg_hermtd_unpack (const gsl_matrix_complex * A, const gsl_vector_complex * TAU, gsl_matrix_complex * Q, gsl_vector * DIAG, gsl_vector * SUBDIAG);
extern int gsl_linalg_hermtd_unpack_T (const gsl_matrix_complex * A, gsl_vector * DIAG, gsl_vector * SUBDIAG);

extern int gsl_linalg_bidiag_decomp (gsl_matrix * A, gsl_vector * TAU_U, gsl_vector * TAU_V);
extern int gsl_linalg_bidiag_unpack (const gsl_matrix * A, const gsl_vector * TAU_U, gsl_matrix * U, const gsl_vector * TAU_V, gsl_matrix * V, gsl_vector * DIAG, gsl_vector * SUPERDIAG);
extern int gsl_linalg_bidiag_unpack2 (gsl_matrix * A, gsl_vector * TAU_U, gsl_vector * TAU_V, gsl_matrix * V);
extern int gsl_linalg_bidiag_unpack_B (const gsl_matrix * A, gsl_vector * DIAG, gsl_vector * SUPERDIAG);

extern int gsl_linalg_HH_solve (gsl_matrix * A, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_HH_svx (gsl_matrix * A, gsl_vector * X);
extern int gsl_linalg_solve_symm_tridiag (const gsl_vector * DIAG, const gsl_vector * E, const gsl_vector * B, gsl_vector * X);
extern int gsl_linalg_solve_symm_cyc_tridiag (const gsl_vector * DIAG, const gsl_vector * E, const gsl_vector * B, gsl_vector * X);
