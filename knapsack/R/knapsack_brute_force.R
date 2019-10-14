#' Finds the optimal solution for given knapsack problem
#' @param x,W  Takes in two arguments, the first one is a dataframe which contains the weights and values vectors and second one is the maximum weight that a knapsack can hold.
#' @author Vinod kumar Dasari and Gowtham Kukkemane Mahalingabhat
#' @references https://en.wikipedia.org/wiki/Knapsack_problem
#' @examples
#' \dontrun{ 
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame( w=sample(1:4000, size = n, replace = TRUE),  
#'                                v=runif(n = n, 0, 10000))
#' knapsack_brute_force(x = knapsack_objects[1:20,], we = 2000)
#' knapsack_brute_force(x = knapsack_objects[1:10,], we = 2000)
#' }
#' @return Returns the highest value which satisfying the given weight constraint.
#' @export


brute_force_knapsack<-function(x, W) 
{
  stopifnot(is.data.frame(x) , is.numeric(W))
  len<-length(x$w)
  flag<-1
  bigint<-0
  temp<-0
  lis = NULL
  if(names(x[1])=='w'&& names(x[2])=='v'&& W>0)
  {
    if(  any(x[1]<0)||any(x[2]<0)){stop('invalid negative inputs')}
    else
    {
      bigint<-2^len
      
      for (j in 1:(bigint-1))
      {
        binary<-intToBits(j)
        ind<-which(binary==1)
        weight<-0
        value<-0
        weight<-sum(x[ind,1])
        value<-sum(x[ind,2])
        if(weight<=W)
        {
          if(value>temp)
          {
            temp<-value
            elements<-ind
            lis<-list('value'=temp,'elements'=elements)
          }
        }
      }
      if(is.null(lis))
      return(lis = c("the given Weight is smaller all possible weights"))
     
      return(lis)
    }
  }
  else
  {
    stop('invalid inputs') 
  }
}




