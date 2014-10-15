setClass(
  Class = 'TrajectoriesBis',
  slots = c(
    times = 'numeric',
    traj = 'matrix'
  )
)

setMethod(f = 'initialize',
          signature = 'TrajectoriesBis',
          definition = function(.Object, nbWeek, BMIinit){
            traj <- outer(BMIinit, 1:nbWeek, function(init, week) {return(init+0.1*week)})
            rownames(traj) <- paste('I', 1:nrow(traj), sep = '')
            colnames(traj) <- paste('T', 1:nbWeek, sep = '')
            .Object@traj <- traj # assign the slots
            .Object@times <- 1:nbWeek
            return(.Object)
          }
)
