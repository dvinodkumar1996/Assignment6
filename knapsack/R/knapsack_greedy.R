#' Finds the optimal solution for given knapsack problem
#' @param x,W  Takes in two arguments, the first one is a dataframe which contains the weights and values vectors and second one is the maximum weight that a knapsack can hold.
#' @author Vinod kumar Dasari and Gowtham Kukkemane Mahalingabhat
#' @references https://en.wikipedia.org/wiki/Knapsack problem#Greedy approximation algorithm
#' @examples
#' \dontrun{ 
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame( w=sample(1:4000, size = n, replace = TRUE),  
#'                                v=runif(n = n, 0, 10000))
#' knapsack_greedy(x = knapsack_objects[1:20,], W = 2000)
#' knapsack_greedy(x = knapsack_objects[1:10,], W = 2000)
#' }
#' @return Returns the highest value which satisfying the given weight constraint.
#' @export

greedy_knapsack<-function(x,W)
{
  stopifnot(is.data.frame(x) , is.numeric(W))
  if(names(x[1])=='w'&& names(x[2])=='v'&& W>0)
  {
    if(  any(x[1]<0)||any(x[2]<0)){stop('invalid negative inputs')}
    else
    {
      x$peritem<-x$v/x$w
      x<-x[order(-x$peritem),]
      rnames<-rownames(x)
      i<-1
      j<-1
      weight<-0
      value<-0
      elements<-vector()
      while(weight<=W)
      {
        if((W-weight)>x$w[i])
        {
        weight<-weight+x$w[i]
        value<-value+x$v[i]
        elements[j]<-as.numeric(rnames[i])
        i<-i+1
        j<-j+1
        }
       else
          break
        
      }
    }
    li<-list('value'=value,'elements'=elements)
    return(li)
  }
  else
  {
    stop('invalid inputs') 
  }
  
}


