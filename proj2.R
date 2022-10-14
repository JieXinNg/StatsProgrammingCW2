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
