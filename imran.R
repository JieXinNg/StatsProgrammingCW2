#FOO
Pone <- function(n, k, strategy, nreps) { 
  #Initialization
  counter <- rep(0, nreps)   
  
  if (strategy == 1) {
    for (a in 1:nreps) {
      #Card Box Allocation
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()
      open_order <- c(k)
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }
      for (j in 1:n) {
        if (cards_in_boxes[[open_order[j]]][2] == k) {
          counter[a] <- 1
          break}
        else {
          open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])
        }      
      }
    }
  }
  
  if (strategy == 2) {
    for (a in 1:nreps) {
      #Card Box Allocation
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()      
      open_order <- c(sample(1:(2*n), 1))
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }
      for (j in 1:n) {
        if (cards_in_boxes[[open_order[j]]][2] == k) {
          counter[a] <- 1
          break}
        else {
          open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])
        }      
      }
    }
  }

  if (strategy == 3) {
    for (a in 1:nreps) {
      #Card Box Allocation
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()      
      open_order <- c(sample(1:(2*n), n))
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }
      for (j in 1:n) {
        if (cards_in_boxes[[open_order[j]]][2] == k) {
          counter[a] <- 1
          break}
      }
    }
  }
  print(sum(counter)/nreps)
}

Pone(50,3,1,10000)
Pone(50,3,2,10000)
Pone(50,3,3,10000)

############################################################################################################################

Pall <- function(n, strategy, nreps) { 
  #Card-Box Allocation
  all_counter <- rep(0, nreps)
  
  if (strategy == 1) {

    for (a in 1:nreps) {
      prisoner_list <- c(1:(2*n))
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()
      counter <- rep(0, 2*n)
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }
      for (b in 1:(2*n)) {
        #Initialization
        open_order <- c(b)
        #Card opening loop
        for (j in 1:n) {
          if (cards_in_boxes[[open_order[j]]][2] == b) {
            counter[b] <- 1
          break}
          else {open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])}      
        }
      }
      if (sum(counter) == 2*n) {all_counter[a] <- 1}

    }
    
  }
  
  if (strategy == 2) {
    
    for (a in 1:nreps) {
      prisoner_list <- c(1:(2*n))
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()
      counter <- rep(0, 2*n)
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }
      for (b in 1:(2*n)) {
        #Initialization
        open_order <- c(sample(1:(2*n), 1))
        #Card opening loop
        for (j in 1:n) {
          if (cards_in_boxes[[open_order[j]]][2] == b) {
            counter[b] <- 1
            break}
          else {open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])}      
        }
      }
      if (sum(counter) == 2*n) {all_counter[a] <- 1}
      
    }
    
  }
  
  if (strategy == 3) {
    
    for (a in 1:nreps) {
      prisoner_list <- c(1:(2*n))
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()
      counter <- rep(0, 2*n)
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }
      for (b in 1:(2*n)) {
        #Initialization
        open_order <- c(sample(1:(2*n)))
        #Card opening loop
        for (j in 1:n) {
          if (cards_in_boxes[[open_order[j]]][2] == b) {
            counter[b] <- 1
            break}
        }
      }
      if (sum(counter) == 2*n) {all_counter[a] <- 1}
      
    }
    
  }
  print(sum(all_counter)/nreps)
  #print(tail(all_counter, 20))
  #print(cards_in_boxes)
  #print(counter)
  
}

#Q3 

for (i in c(5, 50)) {
  for (j in (1:3)) {
    Pall(i, j, 10000)
  }
}
#Pall(5,1,10000)
#Pall(5,2,10000)
#Pall(5,3,10000)
#Pall(50,1,10000)
#Pall(50,2,10000)
#Pall(50,3,10000)

#Q4

#There is a significant different between the result of the strategy 1 and the others, because of the loops, which will be
#elaborated in the next questions, as well.

#For strategy 3, we do not consider the loops, because the prisoners pick the boxes
#randomly. Their decision do not depends on the outcome of the previous one. So, each prisoner will have 0.5 chance, and the
#success probability of each prisoner is independent from the other. Therefore, joint success probability decreases dramatically 
#with increasing n.

#If prisoners decide on the next box according to the outcome of previous box, a loop will definitely occur, because we will turn
#back to our initial box ultimately. In other words, there will be limited number of loops whose ranges are ranging from 1 to 2n.

#Then, for strategy 2, we always have to possibilites: Either the first randomly selected box and our desired box are members of
#the same loop, or not. If not, we will have infinite loop and it is impossible to reach the box containing our number in n steps.
#That is why the joint probability  of success is dramatically less then the one in strategy 1. According to Pone function, the
#probability of success for each prisoner was around 0.4, and it is independent from the other prisoners again. Which decreases 
#the probability of total success with increasing n, again.

#Unlike strategy 2, strategy 3 works significantly well, because the loop including our first box will definitely include our
#desired box, because we are starting with the box with the our number. Then, the probability of success will depend on the length
#of this first loop. As we try max n times, we can find our number only if the length of loop is less than 51. For a single person,
#the probability of success is still 0.5, but this time joint probability is much higher, because the success of each prisoner
#will not be independent from the others. Because the room is turned to its original state after each turn, and all prisoners 
#start with a unique number. Then, all loops included by 2n box-number combinations will be tried by the prisoners, and if the 
#maximum loop length is less than 51, then all prisoners will be free. 


dloop <- function(n, nreps) { 
  
  loop_array <- list()
  for (a in 1:nreps) {
    #Card Box Allocation
    box_numbers <- c(1:(2*n))
    card_numbers <- c(sample(1:(2*n)))
    cards_in_boxes <- list()
    sub_loop_array <- rep(0, 2*n)
  
    for (i in 1:(2*n)) {
      cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
    }
    for (b in 1:(2*n)) {
      #Initialization
      open_order <- c(b)
      loop_length <- 1
      #Card opening loop
      for (j in 1:(2*n)) {
        if (cards_in_boxes[[open_order[j]]][2] == b) {
          sub_loop_array[b] <- loop_length
          break}
        else {open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])
              loop_length <- loop_length+1}
      }
    }
    #loop_array <- append(loop_array, max(sub_loop_array))
    loop_array <- append(loop_array, unique(sub_loop_array))
  }
  #print(open_order)
  #print(loop_array)
  loop_size <- c(1:(2*n))
  match_index <- match(loop_array, loop_size)
  freq <- tabulate(match_index)
  #freq_weight <- 1:(2*n)
  #weighted_freq <- freq*freq_weight
  #print(weighted_freq)
  return(freq)
}

dloop(10,10000)




####### TRASH #########
  
    for (j in 1:(2*n)) {
      if (!is.null(cards_in_boxes[[open_order[j]]][2]) && cards_in_boxes[[open_order[j]]][2] == open_order[j]) {
        loop_length <- length(open_order)}
      else {
        open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])
      }      
    }


    Pone <- function(n, k, strategy, nreps) { 
  #Initialization
  counter <- rep(0, nreps)   
  
  if (strategy == 1) {
    for (a in 1:nreps) {

      #Initialization
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()
      open_order <- c(k)

      #Card-Box Allocation
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }

      #Card opening loop
      for (j in 1:n) {
        if (cards_in_boxes[[open_order[j]]][2] == k) {
          counter[a] <- 1
          break}
        else {
          open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])
        }      
      }
    }
  }
  
  if (strategy == 2) {
    for (a in 1:nreps) {

      #Initialization
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()      
      open_order <- c(sample(1:(2*n), 1))

      #Card-Box Allocation
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }

      #Card opening loop
      for (j in 1:n) {
        if (cards_in_boxes[[open_order[j]]][2] == k) {
          counter[a] <- 1
          break}
        else {
          open_order <- append(open_order, cards_in_boxes[[open_order[j]]][2])
        }      
      }
    }
  }

  if (strategy == 3) {
    for (a in 1:nreps) {

      #Initialization
      box_numbers <- c(1:(2*n))
      card_numbers <- c(sample(1:(2*n)))
      cards_in_boxes <- list()      
      open_order <- c(sample(1:(2*n), n))

      #Card-Box Allocation
      for (i in 1:(2*n)) {
        cards_in_boxes <- append(cards_in_boxes, list(c(box_numbers[i], card_numbers[i])))
      }

      #Card opening loop
      for (j in 1:n) {
        if (cards_in_boxes[[open_order[j]]][2] == k) {
          counter[a] <- 1
          break}
      }
    }
  }
  print(sum(counter)/nreps)
}

#Pone(5,3,1,10000)
#Pone(5,3,2,10000)
#Pone(5,3,3,10000)
