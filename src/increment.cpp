/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// [[Rcpp::export]]
void incr_FBM_mat(Environment BM,
                  const NumericMatrix& mat) {

  XPtr<FBM_RW> xpBM = BM["address_rw"];
  if (xpBM->matrix_type() != 8)
    Rcpp::stop("'big_increment()' works for 'double' FBMs only.");

  BMAcc_RW<double> macc(xpBM);

  size_t n = macc.nrow();
  size_t m = macc.ncol();

  myassert_size(mat.rows(), n);
  myassert_size(mat.cols(), m);

  for (size_t j = 0; j < m; j++)
    for (size_t i = 0; i < n; i++)
      macc(i, j) += mat(i, j);
}

/******************************************************************************/

// [[Rcpp::export]]
void incr_FBM_vec(Environment BM,
                  const NumericVector& vec) {

  XPtr<FBM_RW> xpBM = BM["address_rw"];
  if (xpBM->matrix_type() != 8)
    Rcpp::stop("'big_increment()' works for 'double' FBMs only.");

  BMAcc_RW<double> macc(xpBM);

  size_t K = macc.size();
  myassert_size(vec.size(), K);

  for (size_t k = 0; k < K; k++)
    macc[k] += vec[k];
}

/******************************************************************************/
