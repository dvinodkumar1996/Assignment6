
#include <Rcpp.h>
using namespace Rcpp;


//' Rcc method implementation for knapsack dynamic programming
//' 
//' @param MaxWeight numeric vector indicating maximum capacity of knapsack
//' @param weights numeric vector indicating weight of individual element in the datase
//' @param value value of individual element in the dataset
//' @param length number of objects in the data set
//' @return a matrix with sequence of all weights and values.
//' @export
// [[Rcpp::export]]
NumericMatrix knapSackdynamic_cpp(int MaxWeight, NumericVector weights, NumericVector value, int length)
{
  int i,w;
  NumericMatrix k(length+1, MaxWeight+1);
  for (i = 0; i <= length; i++)
  {
    for (w = 0; w <= MaxWeight; w++)
    {
      if (i == 0 || w == 0)
      {
        k(i,w) = 0;
      }
      else if (weights[i - 1] <= w)
        {
          int temp =  weights(i-1);
          k(i,w) = std::max((value(i-1) + k((i - 1),(w - temp))), k((i - 1),w));
        }
      else
      {
        k(i,w) = k((i - 1),w);
      }
    }
  }
  return k ;
}
  
