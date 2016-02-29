set.seed(1234)
source('raw/dealing.R')

game.results <- data.frame(game="Game 1",score=score)

num <- 1

set.seed(1234)
seeds <- rpois(999,5550)

while (num < 1000) {
  set.seed(seeds[num])
  source('raw/dealing.R')
  game.results <- rbind(game.results,data.frame(game=paste("Game",num+1),score=score))
  num <- num + 1
}

if(!file.exists('data/')) { dir.create("data/")}
write.csv(game.results,file="data/game-results.csv",row.names = F)
