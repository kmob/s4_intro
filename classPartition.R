# We will suppose that part is always composed of capital letters 
# going from A to LETTERS[nbGroups] 
# (it will be necessary to specify in the documentation of this 
# class that the number of groups must be lower to 26). We can allow 
# us such an assumption by programming initialize and part<- to always check 
# that it is the case.
# length of part should be evenly divisible by nbGroups

setClass(Class = 'Partition',
         slots = c(
           nbGroups = 'numeric', 
           part = 'factor'
         ),
         validity = function(object){
           cat('*** Partition: inspector ***\n')
           if(object@nbGroups > 26){
             stop ('[Partition: Validation] the number of groups exceeds 26')
           }else{}
           return(TRUE)
         }
)

# initialize the class automatically naming matrix row and column
setMethod(
  f = 'initialize',
  signature = 'Partition',
  definition = function(.Object, nbGroups, part){
    cat('*** Partition: initializator *** \n')
    if(!missing(part)){  # handle empty objects as needed
      .Object@nbGroups <- nbGroups # assign the slots
      .Object@part <- part
      validObject(.Object) # call the inspector to verify instance is OK      
    }
    return(.Object) # return the object
  }
)

# set show method for class Partition
setMethod(f = 'show',
          signature = 'Partition',
          function(object){
            cat('*** Class Partition, method Show *** \n')
            cat('* nbGroups ='); print(object@nbGroups)
            cat('* Part ='); print(object@part)
            cat('******* End Show (Partition) ******* \n')
          }
)

# write a function to create a new instance of the class
part <- partitions <- function(nbGroups, part){
  cat('*** Partition: constructor *** \n')
  new(Class = 'Partition', nbGroups = nbGroups, part = part)
}

# create getNbGroups method for class Partition
setGeneric(
  name = 'getNbGroups',
  def = function(object){
    standardGeneric('getNbGroups')
  }
)

# set getNbGroups method for class Partition
setMethod(
  f = 'getNbGroups',
  signature = 'Partition',
  definition = function(object){
    return(object@nbGroups)
  }
)

# create getPart method for class Partition
setGeneric(
  name = 'getPart',
  def = function(object){
    standardGeneric('getPart')
  }
)

# set getPart method for class Partition
setMethod(
  f = 'getPart',
  signature = 'Partition',
  definition = function(object){
    return(object@part)
  }
)

setMethod(
  f="plot",
  signature = c(x = "Trajectories", y = "Partition"),
  definition = function(x,y,...){
    matplot(x@times, t(x@traj[y@part == "A",]), 
            ylim = range(x@traj,na.rm = TRUE), 
            xaxt = "n", type = "l", ylab = "", xlab = "", col = 2)
    for(i in 2:y@nbGroups){
      matlines(x@times, t(x@traj[y@part == LETTERS[i],]),
               xaxt = "n",type = "l",col = i+1) 
    }
    axis(1,at=x@times)
  }
)

