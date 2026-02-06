

#include <Rcpp.h>
using namespace Rcpp;

/* -----------------------------------------------------------
  Auxiliary routines
----------------------------------------------------------- */
  
  static void m_multiply(const std::vector<double>& A,
                         const std::vector<double>& B,
                         std::vector<double>& C,
                         int m)
{
  for(int i = 0; i < m; i++)
    
    for(int j = 0; j < m; j++) {
      double s = 0.0;
      
      for(int k = 0; k < m; k++)
        s += A[i * m + k] * B[k * m + j];
      
      C[i * m + j] = s;
    }
  }

static void m_power(const std::vector<double>& A, int eA,
                    std::vector<double>& V, int& eV,
                    int m, int n)
{
  if(n == 1) {
    V = A;
    eV = eA;
    return;
  }
  
  std::vector<double> B(m * m);
  m_power(A, eA, V, eV, m, n / 2);
  m_multiply(V, V, B, m);
  int eB = 2 * eV;
  
  if((n % 2) == 0) {
    V = B;
    eV = eB;
  } else {
    m_multiply(A, B, V, m);
    eV = eA + eB;
  }
  
  if(V[(m/2) * m + (m/2)] > 1e140) {
    for(double& v : V) v *= 1e-140;
    eV += 140;
  }
}

static double K(int n, double d)
{
  int k = static_cast<int>(n * d) + 1;
  int m = 2 * k - 1;
  double h = k - n * d;
  
  std::vector<double> H(m * m, 0.0);
  std::vector<double> Q(m * m, 0.0);
  
  for(int i = 0; i < m; i++)
    for(int j = 0; j < m; j++)
      if(i - j + 1 >= 0)
        H[i * m + j] = 1.0;
  
  for(int i = 0; i < m; i++) {
    H[i * m] -= std::pow(h, i + 1);
    H[(m - 1) * m + i] -= std::pow(h, m - i);
  }
  
  if(2 * h - 1 > 0)
    H[(m - 1) * m] += std::pow(2 * h - 1, m);
  
  for(int i = 0; i < m; i++)
    for(int j = 0; j < m; j++)
      if(i - j + 1 > 0)
        for(int g = 1; g <= i - j + 1; g++)
          H[i * m + j] /= g;
  
  int eH = 0, eQ = 0;
  m_power(H, eH, Q, eQ, m, n);
  
  double s = Q[(k - 1) * m + (k - 1)];
  for(int i = 1; i <= n; i++) {
    s *= static_cast<double>(i) / n;
    if(s < 1e-140) {
      s *= 1e140;
      eQ -= 140;
    }
  }
  
  return s * std::pow(10.0, eQ);
}

/* -----------------------------------------------------------
  Two-sample exact distribution
----------------------------------------------------------- */
  
  static double psmirnov2x(double x, int m, int n)
{
  if(m > n) std::swap(m, n);
  
  double md = static_cast<double>(m);
  double nd = static_cast<double>(n);
  
  double q = (0.5 + std::floor(x * md * nd - 1e-7)) / (md * nd);
  
  std::vector<double> u(n + 1);
  for(int j = 0; j <= n; j++)
    u[j] = (static_cast<double>(j) / nd > q) ? 0.0 : 1.0;
  
  for(int i = 1; i <= m; i++) {
    double w = static_cast<double>(i) / (i + n);
    u[0] = (static_cast<double>(i) / md > q) ? 0.0 : w * u[0];
    
    for(int j = 1; j <= n; j++) {
      if(std::fabs(static_cast<double>(i)/md - static_cast<double>(j)/nd) > q)
        u[j] = 0.0;
      else
        u[j] = w * u[j] + u[j - 1];
    }
  }
  return u[n];
  }

/* -----------------------------------------------------------
  Two-sample asymptotic distribution
----------------------------------------------------------- */
  
  static void pkstwo(NumericVector& x, double tol)
{
  int n = x.size();
  int k_max = static_cast<int>(std::sqrt(2 - std::log(tol)));
  
  for(int i = 0; i < n; i++) {
    
    if(x[i] < 1.0) {
      double z = -(M_PI_2 * M_PI_4) / (x[i] * x[i]);
      double w = std::log(x[i]);
      double s = 0.0;
      
      for(int k = 1; k < k_max; k += 2)
        s += std::exp(k * k * z - w);
        
      x[i] = s / M_1_SQRT_2PI;
      
    } else {
      double z = -2 * x[i] * x[i];
      double sgn = -1.0;
      double oldv = 0.0, newv = 1.0;
      int k = 1;
      
      while(std::fabs(oldv - newv) > tol) {
        oldv = newv;
        newv += 2 * sgn * std::exp(z * k * k);
        sgn *= -1.0;
        k++;
      }
      
      x[i] = newv;
    }
  }
  }

/* -----------------------------------------------------------
  Rcpp exports
----------------------------------------------------------- */
  
  // [[Rcpp::export]]
double pSmirnov2x(double statistic, int nx, int ny)
{
  return psmirnov2x(statistic, nx, ny);
}

// [[Rcpp::export]]
NumericVector pKS2(NumericVector statistic, double tol)
{
  NumericVector ans = clone(statistic);
  pkstwo(ans, tol);
  return ans;
}

// [[Rcpp::export]]
double pKolmogorov2x(double statistic, int n)
{
  return K(n, statistic);
}
