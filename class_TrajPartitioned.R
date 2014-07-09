# new class inherits Trajectories
setClass(Class = 'TrajPartitioned',
         slots = c(listPartitions = 'list'),
         contains = 'Trajectories'
)

# initialize the class automatically naming matrix row and column
setMethod(
  f = 'initialize',
  signature = 'TrajPartitioned',
  definition = function(.Object, times, traj, listPartitions){
    cat('*** TrajPartitioned: initializator *** \n')
    if(!missing(traj)){  # handle empty objects as needed
      .Object@traj <- traj # assign the slots
      .Object@times <- times
      .Object@listPartitions <- listPartitions
      validObject(.Object) # call the inspector to verify instance is OK      
    }
    return(.Object) # return the object
  }
)

# set show method for class TrajPartitioned
setMethod(f = 'show',
          signature = 'TrajPartitioned',
          definition = function(object){
            show(as(object,"Trajectories"))
            lapply(object@listPartitions,show)
          }
)

## setMethod for print uses callNextMethod
## callNextMethod fails to discriminate when class inherits from multiple classes
## specify which parent using as, is, and setIs as shown in work_file.R
#### avoid risks of callNextMethod by using as, is, and setIs
setMethod(
  f = "print",
  signature = "TrajPartitioned",
  definition = function(x,...){
    callNextMethod()
    cat("the object also contains", length(x@listPartitions), "partition")
    cat("\n ***** Fine of print (TrajPartitioned) ***** \n")
    return(invisible()) 
  }
)

