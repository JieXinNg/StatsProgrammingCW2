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



### testing
n <- 2
box_numbers<-c(1:(2*n))
card_numbers<-c(sample(1:(2*n)))
random_allocation<-list()
for (i in 1:(2*n))
{
  random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
}
random_allocation

loop1A<-list()
len_array<-list()
loop2A<-c(1:(2*n))
stop_outer_loop <- FALSE
# loop 2
for (y in 1:(2*n)){
  k <- y
  rearranged_list <- list()
  card_array <- c(k) # k value as first element of card_array
  if (y %in% loop2A){
    for (x in 1:(2*n)) # loop over sequence of card numbers
    {
      for (i in 1:(2*n)) # loop over box 
      {
        
        if (random_allocation[[i]][1] == card_array[x]) # if the box number is equal to the previous card number
        {
          card_array <- c(card_array, random_allocation[[i]][2]) # add new card number to array 1 
          rearranged_list <- append(rearranged_list, list(random_allocation[[i]]))
          #loop2A <- loop2A[!loop2A %in% random_allocation[[i]][1]] # remove used number
          print("here 1")
          
          if (random_allocation[[i]][2] == k) 
          {
            print("end loop")
            card_array <- head(card_array,-1) # remove the repeated card number (the last element) 
            #loop2A <- loop2A[!loop2A %in% k]
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
  }
  #if(length(rearranged_list) != 0)
  #{
  loop1A <- append(loop1A,rearranged_list)
  #print(rearranged_list)
  #}
  
}
loop1A
###



### testing
n<-6
random_allocation<-list()
box_numbers<-c(sample(1:(2*n)))
card_numbers<-c(sample(1:(2*n)))
for (i in 1:(2*n))
{
  random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
}

###










# Q1 
# Pone is used to estimate the probability of finding card = k on or before opening n boxes, there are 3 strategies.
# Strategy 1 is to start at box k, then going to the subsequent box with the number gotten from the card in box k, 
# this process is repeated until card = k is found
# Startegy 2 is the same as Strategy 1, but starting at a random box instead
# Strategy 3 is opening boxes randomly until card = k is found
# n = number of boxes opened to find k card
# k = prisoner's number
# strategy = strategy chosen to find k card
# nreps = number of repetitions of simulation to estimate probability
Pone <- function(n, k, strategy, nreps = 10000)
{ 
  # strategy 1 = start at box k, then move on to the box with number of card opened at box k, repeat the process until card k is found
  if (strategy == 1) { 
    counter <- 0 # start a counter to find the number of times prisoner successfully found card k on or before opening n boxes 
    
    # repeat the simulation nreps times
    for (y in 1:nreps){
      random_allocation<-list() # simulate the random allocation of cards in boxes
      box_numbers<-c(1:(2*n)) # in strategy 1, box numbers are arranged in order (from 1 to 2n), this does not affect the results
      card_numbers<-c(sample(1:(2*n))) # order of card numbers is random (cards from 1 to 2n not in order)
      
      # loop to pair up the boxes and cards
      for (i in 1:(2*n)){
        random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
      }
      
      rearranged_list <- list() # rearrange random_allocation in the order of box opened according to strategy 1
      card_array <- c(k) # card_array is the order of box opened (k box is the first box), this is used to pick the subsequent box number
      stop_outer_loop <- FALSE # once prisoner finds the k card, we will stop the loop
      
      # loop over sequence of card numbers
      for (x in 1:(2*n)) {
        # loop over random_allocation 
        for (i in 1:(2*n)) {
          # if the box number is equal to the previous box's card number
          if (random_allocation[[i]][1] == card_array[x]) {
            card_array <- c(card_array, random_allocation[[i]][2]) # add new card number to array 1 
            rearranged_list <- append(rearranged_list, list(random_allocation[[i]])) # add the pairs of boxes and cards in order of box opened
            
            # if prisoner finds the k card in box i
            if (random_allocation[[i]][2] == k){
              card_array <- head(card_array,-1) # remove the repeated card number (the last element) 
              stop_outer_loop = TRUE # stop outer loop once prisoner finds own number, so that opened boxes are not repeated as k card will loop to the first box again
              break # stop loop
            }
          }
        }
        
        # stop the loop over sequence of card numbers
        if (stop_outer_loop){
          break
        }
      }
      
      # if the number of opened boxes <= n
      if (length(rearranged_list) <= n) {
        counter <- counter + 1 # prisoner successfully found k card on or before opening n boxes, add 1 to counter 
      }
    }
    
    return(counter / nreps) # probability = number of successes / total simulations (nreps)
  }
  
  # same as strategy 1 but pick a random box as starting point
  if (strategy == 2) {
    random_start <- sample(2*n, 1) # start at a random box
    counter <- 0 # start a counter to find the number of times prisoner successfully found card k on or before opening n boxes 
    
    # repeat the simulation nreps times
    for (y in 1:nreps) {
      random_allocation<-list() # simulate the random allocation of cards in boxes
      box_numbers<-c(1:(2*n)) # in strategy 1, box numbers are arranged in order (from 1 to 2n), this does not affect the results
      card_numbers<-c(sample(1:(2*n))) # order of card numbers is random (cards from 1 to 2n not in order)
      
      # loop to pair up the boxes and cards
      for (i in 1:(2*n)) {
        random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
      }
      
      rearranged_list <- list() # rearrange random_allocation in the order of box opened according to strategy 1
      card_array <- c(random_start) # card_array is the order of box opened (random_start is the first box), this is used to pick the subsequent box number
      stop_outer_loop <- FALSE # once prisoner finds the k card, we will stop the loop
      
      # loop over sequence of card numbers
      for (x in 1:(2*n)){
        # loop over random_allocation 
        for (i in 1:(2*n)){
          # if the box number is equal to the previous box's card number
          if (random_allocation[[i]][1] == card_array[x]) {
            card_array <- c(card_array, random_allocation[[i]][2]) # add new card number to array 1 
            rearranged_list <- append(rearranged_list, list(random_allocation[[i]])) # add the pairs of boxes and cards in order of box opened
            
            # if prisoner finds the k card in box i
            if (random_allocation[[i]][2] == k){
              card_array <- head(card_array,-1) # remove the repeated card number (the last element) 
              stop_outer_loop = TRUE # stop outer loop once prisoner finds own number, so that opened boxes are not repeated as k card will loop to the first box again
              break # stop loop
            }
          }
        }
        # stop the loop over sequence of card numbers
        if (stop_outer_loop){
          break
        }
      }
      # if the number of opened boxes <= n
      if (length(rearranged_list) <= n){
        counter <- counter + 1 # prisoner successfully found k card on or before opening n boxes, add 1 to counter 
      }
    }
    
    return(counter / nreps) # probability = number of successes / total simulations (nreps)
  }
  
  # the boxes are randomly sorted, so we follow the random sorting of boxes and find the number of boxes needed to find k
  if (strategy == 3){
    counter <- 0 # start a counter to find the number of times prisoner successfully found card k on or before opening n boxes 
    
    # repeat the simulation nreps times
    for (y in 1:nreps){
      random_allocation<-list() # simulate the random allocation of cards in boxes
      box_numbers<-c(sample(1:(2*n))) # in strategy 3, box numbers are not arranged in order 
      card_numbers<-c(sample(1:(2*n))) # order of card numbers is random (cards from 1 to 2n not in order)
      
      # loop to pair up the boxes and cards
      for (i in 1:(2*n)){
        random_allocation <- append(random_allocation,list(c(box_numbers[i],card_numbers[i]))) 
      }
      
      # loop over random_allocation 
      for (i in 1:(2*n)){
        # if prisoner finds the k card after opening i boxes
        if (random_allocation[[i]][2] == k) {
          # if number of boxes opened <= n
          if (i <= n){
            counter <- counter + 1 # prisoner successfully found k card on or before opening n boxes, add 1 to counter 
            break # stop loop over random_allocation 
          }
        }
      }
    }
    return(counter / nreps) # probability = number of successes / total simulations (nreps)
  }
}


Pone(100,43,1,10000)
Pone(100,43,2,10000)
Pone(100,43,3,10000)



n<-50
p <- 1
for (i in 1:(2*n)){
  p <- p * Pone(n,i,1,5000)
}
p








