Pone <- function(n, k, strategy, nreps = 1000)
{ 
  if (strategy == 1)
  {
    prob <- 1
    
    for (i in 0:(n-1))
    {
      prob <- prob / ((2 * n) - i)  
    }
    return(prob)
  }
}

prob <- 1
n <- 4

for (i in 0:(n-1))
{
  print(i)  
}
prob

n <- 1000000 ## number of groups
bd <- matrix(sample(1:366,30*n,replace=TRUE,prob=c(rep(4,365),1)),n,30) ## birthdays
p <- mean(apply(bd,1,function(x) length(unique(x)))!=30)


n<-4
alist<-list()
c1<-c(1:(2*n))
c2<-c(sample(1:(2*n)))
for (i in 1:(2*n))
{
  alist<-append(alist,list(c(c1[i],c2[i])))
}
alist


Pone <- function(n, k, strategy, nreps = 1000)
{ 
  if (strategy == 1)
  {
    prob <- 1
    
    for (i in 0:(n-1))
    {
      prob <- prob / ((2 * n) - i)  
    }
    return(prob)
  }
}

for (i in 1:(2*n))
{
  
}

