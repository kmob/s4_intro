setClass(
  Class="PartitionFather",
  representation=representation(nbGroups="numeric","VIRTUAL") 
)

### multiplier method for values in the class
setGeneric("nbMultTwo",function(object){standardGeneric("nbMultTwo")})
setMethod(f = "nbMultTwo",
          signature = "PartitionFather",
          definition = function(object){
            object@nbGroups <- object@nbGroups*2
            return (object) 
          }
)

