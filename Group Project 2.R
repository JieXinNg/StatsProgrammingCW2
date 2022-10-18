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


# argument n denote the max times that prisoners can select boxes
Pone <- function(n,k,strategy,nreps)
{
  if (strategy == 1)
  {
    count = 0
    for (rep in 1:nreps)
    {
      box_with_card <- create_box(n)
      #start with the kth box
      picked_box <- box_with_card[[k]]
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
  
  else if (strategy == 2)
  {
    count = 0
    for (rep in 1:nreps)
    {
      box_with_card <- create_box(n)
      picked_box <- box_with_card[[sample(1:(2*n),1)]]
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
  
  else 
  {
    count = 0
    for (rep in 1:nreps)
    {
      box_with_card <- create_box(n)
      picked_box <- box_with_card[[sample(1:(2*n),1)]]
      box_with_card[[picked_box[1]]] <- NULL
      for (b in 1:n)
      {
        if (picked_box[2] != k)
        {
          picked_index <- sample(1:length(box_with_card),1)
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
  count_all_simulations <- 0 #count up the number of simulations that all prisoners succeed
  if(strategy == 1)
  {
    for (rep in 1:nreps)
    {
      count_one_simulation <- 0 # count up the number of prisoners succeed in a simulation
      box_with_card <- create_box(n)
      for (k in 1:(2*n))
      {
        picked_box <- box_with_card[[k]]
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
  
  else 
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
        remained_index <- remained_index[-picked_box[1]]
        for (b in 1:n)
        {
          if (picked_box[2] != k)
          {
            picked_index <- sample(remained_index,1)
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
        length_count <- length_count+1
      }
      loop_included <- append(loop_included,pick)
      length_count <- length_count+1
      loop_len_for_one[length_count] <- loop_len_for_one[length_count]+1
      
    }
    for (i in 1:(2*n))
    {
      if (loop_len_for_one[i]!=0)
      {
        loop_len_for_all[i] <- loop_len_for_all[i]+1
      }
    }
    
  }
  
  return(loop_len_for_all/nreps)
}
#dloop(5,10)

