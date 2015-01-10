# setClass
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


##### initialize for 'Trajectories' ############
# setMethod
# specify generic "initialize" for Trajectories
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

##### show for 'Trajectories' ############
# setMethod
# specify generic "show" for Trajectories
setMethod(
  f = 'show',
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

##### plot for 'Trajectories' ############
# setMethod
# specify generic "plot" for Trajectories
setMethod(
  f = 'plot',
  signature = 'Trajectories',
  definition = function(x, y,...){
    matplot(x@times, t(x@traj), xaxt='n', type = 'l', ylab = '', xlab = '', pch = 1)
    axis(1, at =  x@times)
  }
)

##### print for 'Trajectories' ############
# setMethod
# specify generic "print" for Trajectories
setMethod(f = 'print', 
          signature = 'Trajectories',
          definition = function(x,...){
            cat('*** Class Trajectoreis, method Print *** \n')
            cat('* Times ='); print (x@times)
            cat('* Traj = \n'); print (x@traj)
            cat('******* End Print (Trajectories) ******* \n')
          }
)

##### countMissing for 'Trajectories' ############
# setGeneric
# create countMissing method
setGeneric(
  name = 'countMissing',
  def = function(object){
    standardGeneric('countMissing')
  }
)
# setMethod
# specify "countMissing" for Trajectories
setMethod(
  f = 'countMissing',
  signature = 'Trajectories',
  definition = function(object){
    return(sum(is.na(object@traj)))
  }
)

# lock countMissing method for class Trajectories
lockBinding('countMissing',.GlobalEnv)

##### prevent over-writing methods ############
# setGeneric
# create method to prevent over-writing a method
setGenericVerif <- function(x,y){
  if(!isGeneric(x)){
    setGeneric(x,y)}
  else{}
}

##### getTimes for 'Trajectories' ############
# setGeneric
# create getTimes method
setGeneric('getTimes', function(object){standardGeneric('getTimes')})
# setMethod
# specify "getTimes" for Trajectories
setMethod(f = 'getTimes',
          signature = 'Trajectories',
          definition = function(object){
            return(object@times)
          }
)

##### studyLength for 'Trajectories' ############
# setGeneric
# create studyLength method
setGeneric('studyLength', function(object){standardGeneric('studyLength')})
# setMethod
# specify "studyLength" for Trajectories
setMethod(f = 'studyLength',
          signature = 'Trajectories',
          definition = function(object){
            return(length(object@times))
          }
)

##### getTraj for 'Trajectories' ############
# setGeneric
# create getTraj method
setGeneric('getTraj', function(object){standardGeneric('getTraj')})
# setMethod
# specify "getTraj" for Trajectories
setMethod(f = 'getTraj',
          signature = 'Trajectories',
          definition = function(object){
            return(object@traj)
          }
)

##### studyObservations for 'Trajectories' ############
# setGeneric
# create studyObservations method
setGeneric('studyObservations', function(object){standardGeneric('studyObservations')})
# setMethod
# specify "studyObservations" for Trajectories
setMethod(f = 'studyObservations',
          signature = 'Trajectories',
          definition = function(object){
            return(length(object@traj))
          }
)

##### getTrajInclusion for 'Trajectories' ############
# setGeneric
# create getTrajInclusion method
setGeneric('getTrajInclusion', function(object){standardGeneric('getTrajInclusion')})
# setMethod
# specify "getTrajInclusion" for Trajectories
setMethod(f = 'getTrajInclusion',
          signature = 'Trajectories',
          definition = function(object){
            return(object@traj[,1])
          }
)

##### setTimes for 'Trajectories' ############
# setGeneric
# create setTimes method
setGeneric('setTimes<-', function(object, value){standardGeneric('setTimes<-')})
# setReplaceMethod
# specify "setTimes" for Trajectories
setReplaceMethod(
  f = 'setTimes',
  signature = 'Trajectories',
  definition = function(object, value){
    object@times <- value
    validObject(object) # call the inspector to verify instance is OK 
    return(object)
  }
)

##### impute for 'Trajectories' ############
meanWithoutNa <- function (x){mean(x,na.rm=TRUE)}
# setGeneric
# create impute method
setGeneric("impute",function (.Object){standardGeneric("impute")})
# setMethod
# specify "impute" for Trajectories
setMethod(f = "impute",
          signature = "Trajectories",
          def = function(.Object){
            average <- apply(.Object@traj,2,meanWithoutNa)
            for (iCol in 1:ncol(.Object@traj)){
              .Object@traj[is.na(.Object@traj[,iCol]),iCol] <- average[iCol]      
            }
            return(.Object) 
          }
)
