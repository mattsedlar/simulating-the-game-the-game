#set.seed(1234)

deck <- c(2:99)

# player 1
player1 <- sample(deck,6,replace=F)
deck <- deck[! deck %in% player1]

# player 2
player2 <- sample(deck,6,replace=F)
deck <- deck[! deck %in% player2]

# player 3
player3 <- sample(deck,6,replace=F)
deck <- deck[! deck %in% player3]

players <- list(player1 = player1,player2 = player2,player3 = player3)

ascend.1 <- 1
ascend.2 <- 1
descend.1 <- 99
descend.2 <- 99

# game mechanics

play.cards <- function(p,x,limit) {
  for(i in x) {
    if(i < descend.1 & i > descend.1 - limit) { 
      descend.1 <<- i
      print(paste(i, "was played on descend.1 by",p))
      x <- x[! x %in% i]
    } else if (i < descend.2 & i > descend.2 - limit) {
      descend.2 <<- i
      print(paste(i, "was played on descend.2 by",p))
      x <- x[! x %in% i]
    } else if (i > ascend.1 & i < ascend.1 + limit) {
      ascend.1 <<- i
      print(paste(i, "was played on ascend.1 by",p))
      x <- x[! x %in% i]
    } else if (i > ascend.2 & i < ascend.2 + limit) {
      ascend.2 <<- i
      print(paste(i, "was played on ascend.2 by",p))
      x <- x[! x %in% i]
    } else if (i == descend.1 + 10) { 
      descend.1 <<- i 
      print(paste(i, "was played on descend.1 by",p))
      x <- x[! x %in% i]
    }
    else if (i == descend.2 + 10) { 
      descend.2 <<- i
      print(paste(i, "was played on descend.2 by",p))
      x <- x[! x %in% i]
    }
    else if (i == ascend.1 - 10) { 
      ascend.1 <<- i
      print(paste(i, "was played on ascend.1 by",p))
      x <- x[! x %in% i]
    }
    else if (i == ascend.2 - 10) { 
      ascend.2 <<- i
      print(paste(i, "was played on ascend.2 by",p))
      x <- x[! x %in% i]
    }
    else if (length(x) < min) { break }
    else { next }
  }
  x
}

min <- 4

round <- function(limit=1) {
  
  if(length(deck) == 0) { min <<- 1 }
  
  # player 1's turn
  while(length(player1) > min) {
    player1 <<- play.cards("player1",player1, limit)
    limit <- limit + 1
    if(limit > 99) { 
      print("player1 has no move")
      break
    }
  }
  
  limit <- 1
  
  ## new cards dealt
  if(length(player1) < 6 & length(deck) > 6-length(player1)) {
    player1.deal <<- sample(deck,6-length(player1),replace=F)
    deck <<- deck[! deck %in% player1.deal]
    player1 <<- c(player1,player1.deal)
  } else if (length(deck) == 6-length(player1)) { 
    player1.deal <<- sample(deck,length(deck),replace=F)
    deck <<- deck[! deck %in% player1.deal]
    player1 <<- c(player1, player1.deal)
  } else { print("There are no more cards in the deck") }
  
  # player 2's turn
  while(length(player2) > min) {
    player2 <<- play.cards("player2",player2, limit)
    limit <- limit + 1
    if(limit > 99) { 
      print("player2 has no move")
      break
    }
  }
  
  limit <- 1
  
  ## new cards dealt
  if(length(player2) < 6 & length(deck) > 6-length(player2)) {
    player2.deal <<- sample(deck,6-length(player2),replace=F)
    deck <<- deck[! deck %in% player2.deal]
    player2 <<- c(player2,player2.deal)
  } else if (length(deck) == 6-length(player2)) { 
    player2.deal <<- sample(deck,length(deck),replace=F)
    deck <<- deck[! deck %in% player2.deal]
    player1 <<- c(player2, player2.deal)
  } else { print("There are no more cards in the deck") }
  
  # player 3's turn
  while(length(player3) > min) {
    player3 <<- play.cards("player3",player3, limit)
    limit <- limit + 1
    if(limit > 99) { 
      print("player3 has no move")
      break
    }
  }
  
  limit <- 1
  
  ## new cards dealt
  if(length(player3) < 6 & length(deck) > 6-length(player3)) {
    player3.deal <<- sample(deck,6-length(player3),replace=F)
    deck <<- deck[! deck %in% player3.deal]
    player3 <<- c(player3,player3.deal)
  } else if (length(deck) == 6-length(player3)) { 
    player3.deal <<- sample(deck,length(deck),replace=F)
    deck <<- deck[! deck %in% player3.deal]
    player3 <<- c(player3, player3.deal)
  } else { print("There are no more cards in the deck") }
}

simulation <- replicate(20,round())

score <- sum(length(deck),length(player1),length(player2),length(player3))
score