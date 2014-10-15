
testGutterGame <- function() {
  g <- new(Class = 'Game', score=0) # need to initialize the slots
  for (i in 1:20)
    roll(g) <- 0
  checkEquals(score(g), 0)
}
