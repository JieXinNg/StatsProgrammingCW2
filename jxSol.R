### taken from notes
n <- 1000000 ## number of groups
bd <- matrix(sample(1:366,30*n,replace=TRUE,prob=c(rep(4,365),1)),n,30) ## birthdays
p <- mean(apply(bd,1,function(x) length(unique(x)))!=30)
###

### testing
n<-6
random_allocation<-list()
box_numbers<-c(1:(2*n))
card_numbers<-c(sample(1:(2*n)))
for (i in 1:(2*n))
{
  random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
}
k <- 3
rearranged_list <- list()
card_array <- c(k) # k value as first element of card_array
stop_outer_loop <- FALSE
for (x in 1:(2*n)) # loop over sequence of card numbers
{
  for (i in 1:(2*n)) # loop over list 
  {
    
    if (random_allocation[[i]][1] == card_array[x]) # if the box number is equal to the previous card number
    {
      card_array <- c(card_array, random_allocation[[i]][2]) # add new card number to array 1 
      rearranged_list <- append(rearranged_list, list(random_allocation[[i]]))
      
      if (random_allocation[[i]][2] == k) #(random_allocation[[i]][2] == k)
      {
        card_array <- head(card_array,-1) # remove the repeated card number (the last element) 
        #rearranged_list <- append(rearranged_list, list(random_allocation[[i]]))
        stop_outer_loop = TRUE # stop outer loop
        break # stop loop
      }
    }

  }
  if (stop_outer_loop)
  {
    break
  }
}

rearranged_list
###


# Q1 # strategy one working
Pone <- function(n, k, strategy, nreps = 10000)
{ 
  if (strategy == 1)
  {
    counter <- 0
    for (y in 1:nreps)
    {
      random_allocation<-list()
      box_numbers<-c(1:(2*n))
      card_numbers<-c(sample(1:(2*n)))
      for (i in 1:(2*n))
      {
        random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
      }
      
      rearranged_list <- list()
      card_array <- c(k) # k value as first element of card_array
      stop_outer_loop <- FALSE
      for (x in 1:(2*n)) # loop over sequence of card numbers
      {
        for (i in 1:(2*n)) # loop over list 
        {
          
          if (random_allocation[[i]][1] == card_array[x]) # if the box number is equal to the previous card number
          {
            card_array <- c(card_array, random_allocation[[i]][2]) # add new card number to array 1 
            rearranged_list <- append(rearranged_list, list(random_allocation[[i]]))
            
            if (random_allocation[[i]][2] == k) 
            {
              card_array <- head(card_array,-1) # remove the repeated card number (the last element) 
              #rearranged_list <- append(rearranged_list, list(random_allocation[[i]]))
              stop_outer_loop = TRUE # stop outer loop
              break # stop loop
            }
          }
          
        }
        if (stop_outer_loop)
        {
          break
        }
      }
      if (length(rearranged_list) <= n)
      {
        counter <- counter + 1
      }
    }
    
    return(counter / nreps)
  }
}

