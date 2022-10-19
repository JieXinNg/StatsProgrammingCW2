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






# Q1 
# Pone is used to estimate the probability of finding card = k by the n-th opened boxes, there are 3 strategies.
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
    counter <- 0 # start a counter to find the number of times prisoner successfully found card k by the n-th opened box 
    
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
        counter <- counter + 1 # prisoner successfully found k card by the n-th opened box, add 1 to counter 
      }
    }
    
    return(counter / nreps) # probability = number of successes / total simulations (nreps)
  }
  
  # same as strategy 1 but pick a random box as starting point
  if (strategy == 2) {
    random_start <- sample(2*n, 1) # start at a random box
    counter <- 0 # start a counter to find the number of times prisoner successfully found card k by the n-th opened box 
    
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
        counter <- counter + 1 # prisoner successfully found k card by the n-th opened box, add 1 to counter 
      }
    }
    
    return(counter / nreps) # probability = number of successes / total simulations (nreps)
  }
  
  # the boxes are randomly sorted, so we follow the random sorting of boxes and find the number of boxes needed to find k
  if (strategy == 3){
    counter <- 0 # start a counter to find the number of times prisoner successfully found card k by the n-th opened box 
    
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
            counter <- counter + 1 # prisoner successfully found k card by the n-th opened box, add 1 to counter 
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


# Q2
# function to decompose cycles
cycle_decompose <- function(card_array)
{
  #card_array<-c(sample((2*n)))
  n <- length(card_array) /2
  uniq_list<-list() # list to collect the unique cycles
  big_list<-list() # list of all cycles and their permutations, there will be 2n in total
  for (i in 1:(2*n))
  {
    k<-i
    cycle = c(k) # array to keep track of cycles
    for (x in 1:(2*n))
    {
      if (card_array[cycle[[x]]] %in% cycle) # when the cycle starts to repeat
      {
        big_list<-append(big_list,list(cycle)) # break loop when reaching the end of cycle
        break
      }
      cycle<-append(cycle,card_array[cycle[[x]]]) # add each element of the cycle
    }
  }
  
  # keep adding the unique cycles to uniq_list while removing it from 
  # big_list until all elements are removed from big_list
  while (length(big_list) > 0)
  {
    longLen<- 0
    ulist<- c() # array to match against other cycles  
    for (i in 1:length(big_list))
    {
      if (length(big_list[[i]]) > longLen)
      {
        longLen = length(big_list[[i]])
        ulist <- big_list[[i]]
      }
    } 
    
    uniq_list <- append(uniq_list, list(ulist))
    targetArr <- c()
    for (y in 1:length(big_list))# loop from 1 till (2n)-1 # remove same loops from big_list
    {
      c3<-match(ulist,big_list[[y]])
      if (! (is.na(c3[1])))
      {
        targetArr <- c(targetArr, y)
      }
    }
    
    big_list[targetArr] <- NULL
    
  }
  
  return(uniq_list)
}

#n<-50
#p <- 1
#for (i in 1:(2*n)){
#  p <- p * Pone(n,i,1,5000)
#}

# Pall is used to estimate the probability that all prisoners found their card number by the n-th box
Pall <- function(n, strategy, nreps = 10000)
{
  if (strategy == 1) {
    counter <- 0
   for (y in 1:nreps) {
     success_prisoner <- 0
     card_array <- c(sample(1:(2*n)))
     uniq_list <- cycle_decompose(card_array)
     for (k in 1:(2*n)) {
       for (u in 1:length(uniq_list)) {
         # if prisoner k is in the certain cycle and prisoner found k card by n-th box
         if (k %in% uniq_list[[u]] && length(uniq_list[[u]]) <= n) {
           # count the number of prisoners who found their card
           success_prisoner <- success_prisoner + 1
         }
       }
     }
     # if all prisoners found their card for this simulation
     if (success_prisoner == (2*n)) {
       counter <- counter + 1
     }
   }
    # the probability of all prisoners finding their number 
    # is the number of successful simulations divided by nreps 
     return(counter / nreps)
  }
  
  if (strategy == 2) {
    counter <- 0
    for (y in 1:nreps) {
      rand <- sample(2*n)
      success_prisoner <- 0
      card_array <- c(sample(1:(2*n)))
      uniq_list <- cycle_decompose(card_array)
      for (k in 1:(2*n)) {
        for (u in 1:length(uniq_list)) {
          # if first box is rand and k is in the same cycle as rand and prisoner found k card by n-th box
          if (rand %in% uniq_list[[u]] && length(uniq_list[[u]] <= n) && k %in% uniq_list[[u]]) {
            # count the number of prisoners who found their card
            success_prisoner <- success_prisoner + 1
          }
        }
      }
      # if all prisoners found their card for this simulation
      if (success_prisoner == (2*n)) {
        counter <- counter + 1
      }
    }
    # the probability of all prisoners finding their number 
    # is the number of successful simulations divided by nreps 
    return(counter / nreps)
  }

  if (strategy == 3) {
    counter <- 0
    for (y in 1:nreps) {
      success_prisoner <- 0
      card_array <- c(sample(1:(2*n)))
      # loop over each prisoner
      for (k in 1:(2*n)) {
        # open n random boxes
        for (u in 1:n) {
          # if first box is rand and k is in the same cycle as rand and prisoner found k card by n-th box
          if (k == card_array[u]) {
            # count the number of prisoners who found their card
            success_prisoner <- success_prisoner + 1
            break # break loop to stop search once found card k
          }
        }
      }
      # if all prisoners found their card for this simulation
      if (success_prisoner == (2*n)) {
        counter <- counter + 1
      }
    }
    # the probability of all prisoners finding their number 
    # is the number of successful simulations divided by nreps 
    return(counter / nreps)
  }
}
Pall(50,1,1000)
Pall(50,2,1000)
Pall(50,3,1000)


# Q3
ind_prob <- c(Pone(5,2,1,1000), Pone(5,2,2,1000), Pone(5,2,3,1000))
for (i in 1:length(ind_prob)){
  print(paste0("The individual probability for n = 5 using strategy ", i, " is ", ind_prob[i]))
}

Pone(50,2,1,1000)
Pone(50,2,2,1000)
Pone(50,2,3,1000)
Pall(5,1,1000)
Pall(5,2,1000)
Pall(5,3,1000)
Pall(50,1,1000)
Pall(50,2,1000)
Pall(50,3,1000)


# Q5
dloop <- function(n, nreps = 10000)
{
  prob_arr <- c()
  
  for (h in 1:nreps)
  {
    card_array <- c(sample((2*n)))
    uniq_list <- cycle_decompose(card_array) # list to collect the unique cycles
    
    num_match<-c(1:(2*n)) # match the length of cycles to count how many there are
    lengths_tab<-rep(0,(2*n))
    for (i in 1:length(uniq_list))
    {
      addition <-c(match(num_match,length(uniq_list[[i]])))
      addition[is.na(addition)] = 0
      lengths_tab <- lengths_tab +  addition
    }
    
    lengths_tab <- lengths_tab/lengths_tab
    lengths_tab[is.na(lengths_tab)] = 0
    if (h == 1){
      prob_arr <- lengths_tab
    }
    else {
      prob_arr <- prob_arr + lengths_tab
    }
  }
  
  return(prob_arr / nreps)
}
dloop(10,100)

#Q6
prob<-dloop(50,100)
dat <- data.frame(x=c(1:100), y=(prob))
barplot(dat$y, names.arg=dat$x, ylim=c(0,0.6), ylab="probability", xlab="cycle length")
prob2<- sum(prob[51:100])

# prob that loops no longer than 50 
1 - prob2
