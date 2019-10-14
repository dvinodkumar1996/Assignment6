#' Finds the optimal solution for given knapsack problem
#' @param x,W,fast  Takes in two arguments, the first one is a dataframe which contains the weights and values vectors and second one is the maximum weight that a knapsack can hold.
#' @author Vinod kumar Dasari and Gowtham Kukkemane Mahalingabhat
#' @references https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem
#' @examples
#' \dontrun{ 
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame( w=sample(1:4000, size = n, replace = TRUE),  
#'                                v=runif(n = n, 0, 10000))
#' dynamic_knapsack(x = knapsack_objects[1:20,], W = 2000)
#' dynamic_knapsack(x = knapsack_objects[1:10,], W = 2000)
#' }
#' @return Returns the highest value which satisfying the given weight constraint.
#' @useDynLib knapsack, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export

knapsack_dynamic<-function(x,W,fast=FALSE)
{
  
  stopifnot(is.data.frame(x) , is.numeric(W))
  len<-length(x$w)
  if(names(x[1])=='w'&& names(x[2])=='v' && W>0)
  {
    if(  any(x[1]<0)||any(x[2]<0)){stop('invalid negative inputs')}
    else
    {
      mat <- matrix(0, nrow = (len+1), ncol = (W+1))
      row.names(mat)<-c(0:len)
      colnames(mat)<-c(0:W)
      if(fast==FALSE)
      {
      for(i in 2:(len+1))
      {
        for (j in 1:(W+1))
        {
          if(j>x$w[i-1])
          {
          secondpart<-mat[(i-1),(j-x$w[i-1])]+x$v[i-1]
          mat[i,j]<-max(mat[(i-1),j],secondpart) 
          }
          else
          {
            mat[i,j]<-(mat[(i-1),j])
          }
        }
      }
      }
       else
       {
      #   # Rcpp::cppFunction('NumericMatrix knapSackdynamic_cpp(int MaxWeight, NumericVector weights, NumericVector value, int length)
      #   #   {
      #   #     int i,w;
      #   #     NumericMatrix K(length + 1, MaxWeight + 1);
      #   #     for (i = 0; i <= length; i++)
      #   #     {
      #   #       for (w = 0; w <= MaxWeight; w++)
      #   #       {
      #   #         if (i == 0 || w == 0){
      #   #         K(i,w) = 0;}
      #   #         else if (weights[i - 1] <= w){
      #   #         int temp =  weights[i-1];
      #   #         K(i,w) = std::max((value[i - 1] + K((i - 1),(w - temp))), K((i - 1),w));}
      #   #         else
      #   #         {
      #   #           K(i,w) = K((i - 1),w);
      #   #         }
      #   #       }
      #   #     }
      #   #     return K ;
      #   #   }')MaxWeight, NumericVector weights, NumericVector value, int length
         
        mat<-knapSackdynamic_cpp(MaxWeight=W,weights=x$w,value=x$v,length=len)
         }
      highest<-max(mat[len+1,])
      temp<-highest
      k<-len+1
      h<-1
      elements<-vector()
      while(k>=1)
      {
        if(any(mat[k,]==highest))
          k<-k-1
        else
        {
          elements[h]<-k
          h<-h+1
          highest<-highest-x$v[k]
        }
      }
        
       lid<-list('value'=temp,'elements'=elements) 
        
      return(lid)
    }
  }
  else
  {
    stop('invalid inputs') 
  } 
  
  
}




