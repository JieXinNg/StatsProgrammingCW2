Pone <- function(n,k,strategy,nreps,return_prob)
{
  if (strategy == 1)
  {
    count = 0
    for (rep in 1:nreps)
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
      box_number <- c(1:(2*n))
      card_number <- sample(1:(2*n))
      box_with_card <- list()
      for (i in 1:(2*n))
      {
        # create a list of vector and each vector is in the format (box_number,card_number) 
        # so the box number is the index of the corresponding vector in the list
        box_with_card <- append(box_with_card, list(c(box_number[i],card_number[i]))) 
      }
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
      box_number <- c(1:(2*n))
      card_number <- sample(1:(2*n))
      box_with_card <- list()
      for (i in 1:(2*n))
      {
        # create a list of vector and each vector is in the format (box_number,card_number) 
        # so the box number is the index of the corresponding vector in the list
        box_with_card <- append(box_with_card, list(c(box_number[i],card_number[i]))) 
      }
      picked_box <- box_with_card[[sample(1:(2*n),1)]]
      box_with_card[[picked_box[1]]] <- NULL
      for (b in 1:n)
      {
        if (picked_box[2] != k)
        {
          picked_index <- sample(1:length(box_with_card[[1]]),1)
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
  if (return_prob == TRUE)
  {
    return(count/nreps)
  }
  else 
  {
    return(count)
  }
  
}
#set.seed(8)
#Pone(100,77,1,10000,return_prob = TRUE)
#set.seed(8)
#Pone(100,77,1,10000,return_prob = FALSE)
#Pone(100,77,3,10000)


Pall <- function(n,strategy,nreps)
{
  count_all_simulations <-0
  for (rep in 1:nreps)
  {
    count_one_simulation <- 0
    for (k in 1:(2*n))
    {
      # Each process is independent
      count_one_simulation <- count_one_simulation+Pone(n,k,strategy,1,return_prob=FALSE)
    }
    if (count_one_simulation == 2*n)
    {
      count_all_simulations <- count_all_simulations+1
    }
  }
  
  return(count_all_simulations/nreps) 
}



