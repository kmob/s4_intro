
testNoChange <- function() {
  BMI <- 10
  times <- c(1,2,3,4)
  traj <- matrix(BMI,nrow=4,ncol=4)
  testCase <- new(Class = 'Trajectories', times, traj)
  checkEquals(studyLength(testCase),length(times))
  checkEquals(studyObservations(testCase), length(traj))
  checkEquals(prod(traj), BMI^length(traj))
}

testCountMissingData <- function() {
  times <- c(1,3,4,5)
  traj <-rbind (
    c(15,15.1,15.2,15.2),
    c(16,15.9,16,16.4),
    c(15.2,NA,15.3,15.3),
    c(15.7,15.6,15.8,16)
  )
  testCase <- new(Class = 'Trajectories', times, traj)
  checkEquals(countMissing(testCase), 1)
}