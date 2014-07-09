# define class for Trajectories
setClass(Class = 'Trajectories',
         slots = c(
           times = 'numeric',
           traj = 'matrix'
         ),
         validity = function(object){
           cat('*** Trajectories: inspector ***\n')
           if(length(object@times)!=ncol(object@traj)){
             stop ('[Trajectories: Validation] the number of temporal measurements does not correspond to the number of columns in the matrix')
           }else{}
           return(TRUE)
         }
)

# initialize the class automatically naming matrix row and column
setMethod(
  f = 'initialize',
  signature = 'Trajectories',
  definition = function(.Object, times, traj){
    cat('*** Trajectories: initializator *** \n')
    if(!missing(traj)){  # handle empty objects as needed
      rownames(traj) <- paste('I', 1:nrow(traj), sep = '')
      colnames(traj) <- paste('T', times, sep = '')
      .Object@traj <- traj # assign the slots
      .Object@times <- times
      validObject(.Object) # call the inspector to verify instance is OK      
    }
    return(.Object) # return the object
  }
)

# set show method for class Trajectories
setMethod(f = 'show',
          signature = 'Trajectories',
          function(object){
            cat('*** Class Trajectories, method Show *** \n')
            cat('* Times ='); print(object@times)
            nrowShow <- min(10, nrow(object@traj))
            ncolShow <- min(10, ncol(object@traj))
            cat('* Traj (limited to a matrix 10x10) = \n')
            # if the length of the traj is other than zero
            if(length(object@traj)!=0){
              # print no more than a 10x10 segment of the matrix 
              print(formatC(object@traj[1:nrowShow, 1:ncolShow]), quote = FALSE)              
            } else{}
            cat('******* End Show (Trajectories) ******* \n')
          }
)

# write a function to create a new instance of the class
tr <- trajectories <- function(times, traj){
  cat('*** Trajectories: constructor *** \n')
  new(Class = 'Trajectories', times = times, traj = traj)
}

# write a function to create a new instance of the class based on special arguments
regularTrajectories <- function(nbWeek, BMIinit){
  cat('*** regularTrajectories: constructor *** \n')
  traj <- outer(BMIinit, 1:nbWeek, function(init,week){return(init+0.1*week)})
  times <- 1:nbWeek
  return(new(Class = 'Trajectories', times = times, traj = traj))
}

# set plot method for class Trajectories
setMethod(
  f = 'plot',
  signature = 'Trajectories',
  definition = function(x, y,...){
    matplot(x@times, t(x@traj), xaxt='n', type = 'l', ylab = '', xlab = '', pch = 1)
    axis(1, at =  x@times)
  }
)

# set print method for class Trajectories
setMethod(f = 'print', 
          signature = 'Trajectories',
          function(x,...){
            cat('*** Class Trajectoreis, method Print *** \n')
            cat('* Times ='); print (x@times)
            cat('* Traj = \n'); print (x@traj)
            cat('******* End Print (Trajectories) ******* \n')
          }
)

# create countMissing method for class Trajectories
setGeneric(
  name = 'countMissing',
  def = function(object){
    standardGeneric('countMissing')
  }
)

# set countMissing method for class Trajectories
setMethod(
  f = 'countMissing',
  signature = 'Trajectories',
  definition = function(object){
    return(sum(is.na(object@traj)))
  }
)

# lock countMissing method for class Trajectories
lockBinding('countMissing',.GlobalEnv)

# create method to prevent over-writing a method using setGeneric
setGenericVerif <- function(x,y){
  if(!isGeneric(x)){
    setGeneric(x,y)}
  else{}
}

### Getter for 'times'
setGeneric('getTimes', function(object){standardGeneric('getTimes')})
setMethod(f = 'getTimes',
          signature = 'Trajectories',
          definition = function(object){
            return(object@times)
          }
)

### Getter for 'traj'
setGeneric('getTraj', function(object){standardGeneric('getTraj')})
setMethod(f = 'getTraj',
          signature = 'Trajectories',
          definition = function(object){
            return(object@traj)
          }
)


### Getter for subset of 'traj'
setGeneric('getTrajInclusion', function(object){standardGeneric('getTrajInclusion')})
setMethod(f = 'getTrajInclusion',
          signature = 'Trajectories',
          definition = function(object){
            return(object@traj[,1])
          }
)


##### setter for 'times'
setGeneric('setTimes<-', function(object, value){standardGenaric('setTimes<-')})
setReplaceMethod(
  f = 'setTimes',
  signature = 'Trajectories',
  definition = function(object, value){
    object@times <- value
    validObject(object) # call the inspector to verify instance is OK 
    return(object)
  }
)

# # new class inherits Trajectories, didn't work if in separate .R file
# setClass(Class = 'TrajPartitioned',
#          slots = c(listPartitions = 'list'),
#          contains = 'Trajectories'
# )
# 
# # initialize the class automatically naming matrix row and column
# setMethod(
#   f = 'initialize',
#   signature = 'TrajPartitioned',
#   definition = function(.Object, times, traj, listPartitions){
#     cat('*** TrajPartitioned: initializator *** \n')
#     if(!missing(traj)){  # handle empty objects as needed
#       .Object@traj <- traj # assign the slots
#       .Object@times <- times
#       .Object@listPartitions <- listPartitions
#       validObject(.Object) # call the inspector to verify instance is OK      
#     }
#     return(.Object) # return the object
#   }
# )
# 
# # set show method for class TrajPartitioned
# setMethod(f = 'show',
#           signature = 'TrajPartitioned',
#           definition = function(object){
#             show(as(object,"Trajectories"))
#             lapply(object@listPartitions,show)
#           }
# )
# 
# setMethod(
#   f = "print",
#   signature = "TrajPartitioned",
#   definition = function(x,...){
#     callNextMethod()
#     cat("the object also contains", length(x@listPartitions), "partition")
#     cat("\n ***** Fine of print (TrajPartitioned) ***** \n")
#     return(invisible()) 
#   }
# )
