#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp ;
using namespace Eigen ;

void destructive_threshold_matrix(MatrixXd& m, double tol){
  for(int i = 0; i < m.rows(); ++i)
    for(int j = 0; j < m.cols(); ++j)
      if(abs(m(i,j)) < tol)
	m(i,j) = 0;
}

// [[Rcpp::export]]
MatrixXd matrix_power(MatrixXd m, double power, double tol, int k){
  int n = m.rows();
  int p = m.cols();
  BDCSVD<MatrixXd> solver(m, ComputeThinU | ComputeThinV);
  //JacobiSVD<MatrixXd> solver(m, ComputeThinU | ComputeThinV);
  
  MatrixXd u = solver.matrixU();
  MatrixXd v = solver.matrixV();
  VectorXd d = solver.singularValues();

  //Compute the rank given tolerance
  int rank = 0;
  for(int i = 0; i < d.size(); ++i){
    if(d(i) * d(i) <  tol) break;
    ++rank;
  }

  // Rcout << u.rows() << " " << solver.matrixU().cols() << '\n';
  // Rcout << v.rows() << " " << v.cols() << '\n';
  // Rcout << rank << '\n';
  // Rcout << d.head(10) << "\n";

  if(k < rank)
    rank = k;
  
  MatrixXd lru = u.block(0,0,n,rank);
  MatrixXd lrv = v.block(0,0,p,rank);
  VectorXd lrd = d.head(rank);

  if(!lrd.allFinite())
    stop("Matrix is not diagonalizable, maybe try centering/scaling");

  // Zero lru and lrv when small (probably not necessary, but quick)
  destructive_threshold_matrix(lru, tol);
  destructive_threshold_matrix(lrv, tol);

  // Exponentiate singular values
  for(int j = 0; j < rank; ++j)
    lrd(j) = pow(lrd(j), power);


  // Multiply exponentiated singular values back in
  for(int j = 0; j < rank; ++j)
    for(int i = 0; i < n; ++i)
      lru(i,j) *= lrd(j);

  MatrixXd res = lru * lrv.transpose();
  destructive_threshold_matrix(res, tol);

  return(res);
}
