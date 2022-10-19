# Group members:
# 1. Jie Xin Ng s1859154
# 2. Qiqi Xiao s2323497
# 3. Imran Toprak s2430699

# Team member contributions: 
# the three of us tried all the questions together and helped each other out when stuck
# we compared our codes and modified them into this final version by picking out bits from each others' codes
# each of us contributed equally towards this assignment 
# each of us contributed 33% towards the assignment

### Overview ####
# This project is set up to find a good strategy for the prisoner problem through simulations.
# The prisoner problem can be described as a problem where there are 2n prisoners, 2n boxes, and 2n cards, 
# each prisoner is assigned a number from 1 to 2n, the same is done for the boxes and cards.
# In each box, a card is randomly allocated.
# The prisoner is given n tries to open a box until card k, the prisoner's own assigned number is found
# This project will simulate 3 strategies to find the card k by the n-th opened box.
# A success would be counted as a simulation whereby the prisoner found card k by the n-th box.
# We compare the 3 strategies by comparing the probabilities of successful simulations (prisoner found card k) 
# for each strategy.
# Startegy 1 is to start at box k, then going to the subsequent box with the card number gotten in box k, 
# this process is repeated until card = k is found
# Strategy 2 is the same as Strategy 1, but starting at a random box instead
# Strategy 3 is opening boxes randomly until card = k is found
# The function Pone simulates the prisoner problem and estimate the probabilities
# From the results found with Pone, we are interested in finding the probabilities of all prisoners successfully 
# finding their own cards with each startegy. Hence, Pall is a function to estimate thoes probabilities.
# We then created the function dloop to explore the probabilities of each loop length from 1 to 2n occurring 
# at least once in a random shuffling of cards to boxes
# dloop is created to find an explanation for why strategy 1 is better than the other two
# The presence of loops in the sequence of cards while being in the loop for starting at box k increases the 
# chances of finding card k if loops with length <= n are more likely to occur. 
# We end the project with visualising the probabilities of each loop length from 1 to 2n occurring 
# at least once in a random shuffling of cards to boxes
#################

create_box <- function(n)
{
  # this function returns a list to contain 2n of box-car pairs where each card is randomly allocated to a box  
  # @return box_with_card = a list of box and card pairs
  # @param n = half of the number of boxes, i.e. put n=5 if you wish to create 10 box-card pairs

  
  box_number <- c(1:(2*n)) # create an array of boxes from 1 to 2n
  card_number <- sample(1:(2*n)) # create an array of cards randomly sorted from 1 to 2n
  box_with_card <- list() # create a list to contain the box-card pair
  
  # loop to pair up the boxes and cards
  for (i in 1:(2*n)) {
    box_with_card <- append(box_with_card, list(c(box_number[i],card_number[i]))) 
  }
  
  return(box_with_card) # this is in the format (box_number,card_number)
}

Pone <- function(n, k, strategy, nreps){
  
  # Pone is used to estimate the probability of finding card = k by the n-th box, there are 3 strategies.
  # Strategy 1 is to start at box k, then going to the subsequent box with the card number gotten in box k,  
  # this process is repeated until card = k is found
  # Strategy 2 is the same as Strategy 1, but starting at a random box instead
  # Strategy 3 is opening boxes randomly until card = k is found
  #
  # This function uses create_box
  # @return the probability of finding card = k by the n-th box given a strategy
  # @param n = number of boxes opened to find k card
  # @param k = prisoner's number
  # @param strategy = strategy chosen to find k card
  # @param nreps = number of repetitions of simulation to estimate probability
  
  if (strategy == 1){
    count = 0 # start a counter to find the number of times prisoner successfully found card k by the n-th opened box 
    
    # repeat the simulation nreps times
    for (rep in 1:nreps){
      box_with_card <- create_box(n) # create a list of 2n random box-card pairs (numbers ranging from 1 to 2n)
      picked_box <- box_with_card[[k]] # The k-th box is the first opened box
      
      # loop n times as the prisoner only has n tries
      for (b in 1:n){
        # if we do not find the prisoner's number in the box
        if (picked_box[2] != k){
          # pick the next box using the card number found in current box, [picked_box[2]]
          picked_box <- box_with_card[[picked_box[2]]]
        }
        # if we found the prisoner's card by the n-th opened box
        else {
          count <- count + 1 # counting the simulation as a success, hence + 1 to count 
          break
        }
      }
    }
  }
  
  else if (strategy == 2)
  {
    count = 0 # start a counter to find the number of times prisoner successfully found card k by the n-th opened box
    # repeat the simulation nreps times
    for (rep in 1:nreps){
      box_with_card <- create_box(n) # create a list of 2n random box-card pairs (numbers ranging from 1 to 2n)
      picked_box <- box_with_card[[sample(1:(2*n),1)]] # start from a randomly selected box
      
      # loop n times as the prisoner only has n tries
      for (b in 1:n){
        # if we do not find the prisoner's number in the box
        if (picked_box[2] != k){
          # pick the next box using the card number found in current box, [picked_box[2]]
          picked_box <- box_with_card[[picked_box[2]]]
        }
        # if we found the prisoner's card by the n-th opened box
        else {
          count <- count + 1 # counting the simulation as a success, hence + 1 to count 
          break
        }
      }
    }
  }
  
  else if (strategy == 3)
  {
    count = 0 # start a counter to find the number of times prisoner successfully found card k by the n-th opened box
    # repeat the simulation nreps times
    for (rep in 1:nreps){
      box_with_card <- create_box(n) # create a list of 2n random box-card pairs (numbers ranging from 1 to 2n)
      picked_box <- box_with_card[[sample(1:(2*n),1)]] # start from a randomly selected box
      box_with_card[[picked_box[1]]] <- NULL # the boxes cannot be selected twice, set the first box to null so that it is not repeated
      
      # loop n times as the prisoner only has n tries
      for (b in 1:n)
      {
        # if we do not find the prisoner's number in the box
        if (picked_box[2] != k){
          picked_index <- sample(1:length(box_with_card),1) # choose a random index to open another box for next try
          picked_box <- box_with_card[[picked_index]] # select the box
          box_with_card[[picked_index]] <- NULL # set the selected box to null so that it is not repeated
        }
        # if we found the prisoner's card by the n-th opened box
        else {
          count <- count + 1 # counting the simulation as a success, hence + 1 to count 
          break
        }
      }
    }
  }
  
  return(count/nreps) # probability = number of successes / total simulations (nreps)
  
}

Pall <- function(n,strategy,nreps){
  count_all_simulations <- 0 #counting up the times of simulations that all prisoners succeed
  if(strategy == 1)
  {
    for (rep in 1:nreps)
    {
      count_one_simulation <- 0 # counting up the times of prisoners succeed in a simulation
      box_with_card <- create_box(n)
      for (k in 1:(2*n))
      {
        picked_box <- box_with_card[[k]] # boxes and cards inside are exactly the same for every prisoner in one simulation
        for (b in 1:n)
        {
          if (picked_box[2] != k)
          {
            picked_box <- box_with_card[[picked_box[2]]]
          }
          else 
          {
            count_one_simulation <- count_one_simulation+1 # counting up the numbers of prisoners who have found the card in one simulation
            break
          }
        }
      }
      if (count_one_simulation == (2*n))
      {
        count_all_simulations <- count_all_simulations+1 # counting up the times that all prisoners succeed
      }
    }
  }
  
  else if (strategy == 2)
  {
    count_all_simulations <- 0
    for (rep in 1:nreps)
    {
      count_one_simulation <- 0
      box_with_card <- create_box(n)
      for (k in 1:(2*n))
      {
        picked_box <- box_with_card[[sample(1:(2*n),1)]]
        for (b in 1:n)
        {
          if (picked_box[2] != k)
          {
            picked_box <- box_with_card[[picked_box[2]]]
          }
          else 
          {
            count_one_simulation <- count_one_simulation+1
            break
          }
        }
      }
      if (count_one_simulation == (2*n))
      {
        count_all_simulations <- count_all_simulations+1
      }
    }
  }
  
  else if (strategy == 3)
  {
    count_all_simulations <- 0
    for (rep in 1:nreps)
    {
      count_one_simulation <- 0
      box_with_card <- create_box(n)
      for (k in 1:(2*n))
      {
        picked_box <- box_with_card[[sample(1:(2*n),1)]]
        remained_index <- c(1:(2*n))
        remained_index <- remained_index[-picked_box[1]] # the boxes cannot be selected twice
        for (b in 1:n)
        {
          if (picked_box[2] != k)
          {
            if (length(remained_index)>=2)
            {
              picked_index <- sample(remained_index,1)
            }
            else if(length(remained_index)==1)
            {
              picked_index <- remained_index
            }
            picked_box <- box_with_card[[picked_index]] 
            remained_index <- remained_index[-picked_box[1]]
          }
          else 
          {
            count_one_simulation <- count_one_simulation+1
            break
          }
        }
      }
      if (count_one_simulation == (2*n))
      {
        count_all_simulations <- count_all_simulations+1
      }
    }
  }
  
  return(count_all_simulations/nreps) 
}

# We compare the probabilities for n=5 and n=50 for individual and joint probabilities
one5 <- c(Pone(5,7,1,10000),Pone(5,7,2,10000),Pone(5,7,3,10000))
one50 <- c(Pone(50,7,1,10000),Pone(50,7,2,10000),Pone(50,7,3,10000))
all5 <- c(Pall(5,1,10000),Pall(5,2,10000),Pall(5,3,10000))
all50 <- c(Pall(50,1,10000),Pall(50,2,10000),Pall(50,3,10000))
# when n=5
for (i in 1:3){
  print(paste0("The individual success probability for n = 5 using strategy ", i, " is ", one5[i]))
  print(paste0("The joint success probability for n = 5 using strategy ", i, " is ", all5[i]))
}
# when n=50
for (i in 1:3){
  print(paste0("The individual success probability for n = 50 using strategy ", i, " is ", one50[i]))
  print(paste0("The joint success probability for n = 50 using strategy ", i, " is ", all50[i]))
}


# Q4




dloop <- function (n,nreps){
  
  # dloop is used to estimate the probabilities of each loop length from 1 to 2n occurring at least once 
  # in a random shuffling of cards to boxes.
  # This is achieved by setting up two variables "start" and "pick", "start" is used to decompose the cycles as we 
  # remove elements found in the "start" cycle such that we do not end up in the same permutation cycle again when
  # picking a new "start" value.
  # "pick" is used to pick the next opened box while adding each element found to the list of elements to be 
  # excluded from "start", if we end up picking a box with the card "start", we end the current loop and and 
  # choose a new "start"
  #
  # @return a 2n array of probabilities for each loop length occurring at least once in a random shuffling of 
  # cards to boxes
  # @param n = half the loop length, i.e. put n=5 if you wish to create 10 box-card pairs
  # @param nreps = number of repetitions of simulation to estimate probability
  
  
  # array to contain the probabilites for all loop length occurring at least once 
  loop_len_for_all <- rep(0,2*n)
  
  # loop the simulation nreps times 
  for (rep in 1:nreps){
    # array to contain the proability for all loop length occurring at least once in a given simulation
    loop_len_for_one <- rep(0,2*n)
    # collect the box numbers that have already been in a loop so these indices will not be chosen when starting other loops
    loop_included <- c() 
    card_num <- sample(1:(2*n)) # simulate the random shuffling of card numbers from 1 to 2n
    start <- 1                  # we start with the first card for the first loop
    
    # while we have not found all the cycles of the cards
    while (length(loop_included) != 2*n) {
      # if we have not gone through all possible loops, i.e. length(loop_included) < 2*n-1
      if (length(loop_included) >= 1 & length(loop_included) < 2*n-1){
        # pick the next start for the next loop among the cards that have not been opened
        start <- sample(c(1:(2*n))[-loop_included], 1)
      }
      
      # else if we have gone through all loops except for one
      else if(length(loop_included) == 2*n-1){
        
        start <- c(1:(2*n))[-loop_included] # set the last card number to be the element left in loop_included
      }
  
      pick <- start     # set the first opened box number to be start
      length_count <- 0 # count up the length of the loop
      
      # while we have not reached the end of the loop, i.e. the selected card is not the starting box
      while (card_num[pick] != start){
        
        loop_included <- append(loop_included, pick)# keep adding the elements in the cycle into loop_included
        pick <- card_num[pick]                      # pick the next box to be the current card number
        length_count <- length_count + 1            # counting up the length of each loop
        
      }
      
      loop_included <- append(loop_included, pick) # the last element in the loop is not counted up
      length_count <- length_count + 1             # count up the number of elements in the loop
      # count the times each loop length shows in one simulation
      loop_len_for_one[length_count] <- loop_len_for_one[length_count] + 1 
      
    }
    
    # loop through each cycle length to update the value for loop_len_for_all
    for (i in 1:(2*n)){
      # if the cycle length occurs at least one
      if (loop_len_for_one[i] != 0){
        # count up each loop length occurring at least once for all simulations
        loop_len_for_all[i] <- loop_len_for_all[i] + 1 
      }
    }
  }
  
  # divide the total number of times each cycle length occurs by nreps (total number of simulations) to get probability
  return(loop_len_for_all / nreps) 
  
}

# Now we explore the probabilities with dloop and visualise them
prob<-dloop(50,10000)
print("Below is the estimate of the probabilities of each loop length from 1 to 2n occurring at least once for n = 50.")
print(prob)
prob2<- sum(prob[51:100])
print(paste0("The probability that there is no loop longer than 50 in a random reshuffling of cards to boxes is ", 1 - prob2))
# plot the probabilities
dat <- data.frame(x=c(1:100), y=(prob))
barplot(dat$y, names.arg=dat$x, ylim=c(0,0.7), ylab="Probability", xlab="Cycle length")


