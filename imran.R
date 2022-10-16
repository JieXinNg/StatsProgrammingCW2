#FOO
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
      open_order <- c(sample(1:(2*n), 5))

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

Pone(5,3,1,10000)
Pone(5,3,2,10000)
Pone(5,3,3,10000)

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
  print(tail(all_counter, 20))
  #print(cards_in_boxes)
  print(counter)
  
}

Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)


  
