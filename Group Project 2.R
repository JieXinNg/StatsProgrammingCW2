 create_box <- function(n)
{
  box_number <- c(1:(2*n))
  card_number <- sample(1:(2*n))
  box_with_card <- list()
  for (i in 1:(2*n))
  {
    # create a list of vector and each vector is in the format (box_number,card_number) 
    # so the box number is the index of the corresponding vector in the list
    box_with_card <- append(box_with_card, list(c(box_number[i],card_number[i]))) 
  }
  return(box_with_card)
}


# argument n denotes the max number of boxes one prisoner can select, k denotes the prisoner's number, 
# strategy means the method we choose and it can be 1,2,3 and nreps is the number of replicatesimulations
Pone <- function(n,k,strategy,nreps)
{
  if (strategy == 1)
  {
    count = 0
    for (rep in 1:nreps)
    {
      box_with_card <- create_box(n)
      picked_box <- box_with_card[[k]] # start from the kth box
      # loop n times to ensure that n card numbers can be checked to the maximum
      for (b in 1:n)
      {
        if (picked_box[2] != k)
        {
          picked_box <- box_with_card[[picked_box[2]]]
        }
        else 
        {
          count <- count+1 # counting up the times that the prisoner has found the card 
          break
        }
      }
    }
  }
  
  else if (strategy == 2)
  {
    count = 0
    for (rep in 1:nreps)
    {
      box_with_card <- create_box(n)
      picked_box <- box_with_card[[sample(1:(2*n),1)]] # start from a randomly selected box
      for (b in 1:n)
      {
        if (picked_box[2] != k)
        {
          picked_box <- box_with_card[[picked_box[2]]]
        }
        else 
        {
          count <- count+1
          break
        }
      }
    }
  }
  
  else if (strategy == 3)
  {
    count = 0
    for (rep in 1:nreps)
    {
      box_with_card <- create_box(n)
      picked_box <- box_with_card[[sample(1:(2*n),1)]]
      box_with_card[[picked_box[1]]] <- NULL # the boxes cannot be selected twice
      for (b in 1:n)
      {
        if (picked_box[2] != k)
        {
          picked_index <- sample(1:length(box_with_card),1) # every box is randomly selected
          picked_box <- box_with_card[[picked_index]]
          box_with_card[[picked_index]] <- NULL
        }
        else 
        {
          count <- count+1
          break
        }
      }
    }
  }
  
    return(count/nreps)
  
}


Pall <- function(n,strategy,nreps)
{
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

#cat("When n=5, the individual success probability is: ", Pone(5,7,1,10000),
    #" and the joint success probability is: ", Pall(5,1,10000),"with strategy 1.")
#cat("When n=5, the individual success probability is: ", Pone(5,7,2,10000),
   # " and the joint success probability is: ", Pall(5,2,10000),"with strategy 2.")
#cat("When n=5, the individual success probability is: ", Pone(5,7,3,10000),
    #" and the joint success probability is: ", Pall(5,3,10000),"with strategy 3.")
#cat("When n=50, the individual success probability is: ", Pone(50,7,1,10000),
    #" and the joint success probability is: ", Pall(50,1,10000),"with strategy 1.")
#cat("When n=50, the individual success probability is: ", Pone(50,7,2,10000),
    #" and the joint success probability is: ", Pall(50,2,10000),"with strategy 2.")
#cat("When n=50, the individual success probability is: ", Pone(50,7,3,10000),
    #" and the joint success probability is: ", Pall(50,3,10000),"with strategy 3.")

# The joint success probability is surprisingly high for strategy 1. 
# No matter what other arguments take, the probability is around 31%, which is quite higher than other two strategies.


dloop <- function (n,nreps)
{
  loop_len_for_all <- rep(0,2*n)
  for (rep in 1:nreps)
  {
    loop_len_for_one <- rep(0,2*n)
    # collect the box numbers that have already been in a loop so these indices will not be chosen when starting other loops
    loop_included <- c() 
    card_num <- sample(1:(2*n))
    start <- 1 
    while (length(loop_included)!=2*n) 
    {
      if (length(loop_included)>=1 & length(loop_included)<2*n-1)
      {
        start <- sample(c(1:(2*n))[-loop_included],1)
      }
      else if(length(loop_included)==2*n-1)
      {
        start <- c(1:(2*n))[-loop_included]
      }
      pick <- start
      length_count <- 0
      
      while (card_num[pick]!=start)
      {
        loop_included <- append(loop_included,pick)
        pick <- card_num[pick]
        length_count <- length_count+1 # counting up the length of each loop
      }
      # the last element in the loop is not counted up
      loop_included <- append(loop_included,pick)
      length_count <- length_count+1
      loop_len_for_one[length_count] <- loop_len_for_one[length_count]+1 # collect the times each loop length shows in one simulation
      
    }
    for (i in 1:(2*n))
    {
      if (loop_len_for_one[i]!=0)
      {
        loop_len_for_all[i] <- loop_len_for_all[i]+1 # counting up each loop length occurring at least once in all simulations
      }
    }
    
  }
  
  return(loop_len_for_all/nreps)
}
dloop(50,10000)

