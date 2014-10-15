source('classTrajectories.R')
source('class_Partition.R')
source('class_TrajectoriesBis.R')
source('class_TrajPartitioned.R')
source('class_PartitionFather.R')
source('class_PartitionSimple.R')
source('class_PartitionEval.R')



######### CLASS DECLARATION #########

new(Class = 'Trajectories', times=c(1,3,4))

new(Class = 'Trajectories', times=c(1,3),traj=matrix(1:4,ncol=2))

trajPitie <- new(Class='Trajectories')

trajCochin <- new(Class = 'Trajectories',
                  times =c(1,3,4,5),
                  traj=rbind (
                    c(15,15.1,15.2,15.2),
                    c(16,15.9,16,16.4),
                    c(15.2,NA,15.3,15.3),
                    c(15.7,15.6,15.8,16)
                  )
)

trajStAnne <- new(Class='Trajectories',
                  times=c(1:10,(6:16)*2),
                  traj=rbind(
                    matrix(seq(16,19,length=21),ncol=21,nrow=50,byrow=TRUE),
                    matrix(seq(15.8,18,length=21),ncol=21,nrow=30,byrow=TRUE)
                  )+rnorm(21*80,0,0.2)
)

trajCochin@times
trajCochin@times<-c(1,2,4,5) #using @ re-writes the values of the instance. DON'T use @!
trajCochin

test <- new('Trajectories')
test
length(test)

slotNames('Trajectories')
getSlots('Trajectories')
getClass('Trajectories')

####### METHODS ###########

# try plot method set for class Trajectories
par(mfrow = c(1,2))
plot(trajCochin)
plot(trajStAnne)

# try print method set for class Trajectories
print(trajCochin)

# try show method set for class Trajectories
trajStAnne

# what happens with new instance with no arguments?
new('Trajectories')

# try countMissing method for class Trajectories
countMissing(trajCochin)

# see the methods for a class
showMethods(class = 'Trajectories')

# see the details for a method
getMethod(f ='plot',
          signature = 'Trajectories')

# verify the method is defined for the class
existsMethod(f = 'plot', signature = 'Trajectories')

#### CONSTRUCTION ##########

# see results of inspector check on instance validity
new(Class = 'Trajectories', times = 1:2, traj = matrix(1:2,ncol=2))
new(Class = 'Trajectories', times = 1:3, traj = matrix(1:2,ncol=2))

## reminder that accessing values by @ is a bad idea
trajStLouis <- new(Class = 'Trajectories', times = c(1), traj = matrix(1))
## using @ bypasses checking done on initialization, so the instance can now be broken
(trajStLouis@times <- c(1,2,3))

# see the results of initialize on labeling and inspecting
new(Class = 'Trajectories', times = c(1,2,4,8), traj = matrix(1:8, nrow = 2))
new(Class = 'Trajectories', times = c(1,2,48), traj = matrix(1:8, nrow = 2))

# see the result of using initialize to define the args for setting a new instance
# args in 'new' don't have to match the slots in the class
new(Class = 'TrajectoriesBis', nbWeek = 4, BMIinit = c(16,17,15.6))

# see the results of the constructor funtion for general users that hides 'new'
trajectories(time = c(1,2,4), traj = matrix(1:6, ncol = 3))
# see the results of the constructor function that takes special arguments
# shows that a 'global' initializor can deal with multiple cases
regularTrajectories(nbWeek = 3, BMIinit = c(14,15,16))

##### GET ACCESSOR ########
# times for a Trajectory
getTimes(trajCochin)
# traj for a Trajectory
getTraj(trajCochin)
# first column of traj for a Trajectory
getTrajInclusion(trajCochin)


##### SET ACCESSOR ########
setTimes(trajCochin) <- c(1,2,4,6)
setTimes(trajCochin) <- 1:4

###### Section 8 #######
### new Partition instances ####
partCochin <- new(Class="Partition",nbGroups=2,part=factor(c("A","B","A","B")))
partStAnne <- new(Class="Partition",nbGroups=2,part=factor(rep(c("A","B"),c(50,30))))

partTest <- new(Class="Partition",nbGroups=27,part=factor(rep(c("A","B"),c(50,30))))

partCochin
partStAnne

par(mfrow=c(2,2))
### Plot for "Trajectory"
plot(trajCochin)
plot(trajStAnne)
### Plot for "Trajectory" plus "Partition" 
plot(trajCochin,partCochin)
plot(trajStAnne,partStAnne)

###### Section 9 ########
### new TrajPartitioned instances ###
tdPitie <- new('TrajPartitioned')

unclass(tdPitie)

partCochin2 <- new(Class = "Partition",
                   nbGroups = 3,
                   part = factor(c("A","C","C","B")) 
                   )


getMethod("initialize","TrajPartitioned") #looks only at TrajPartitioned
existsMethod("initialize","TrajPartitioned") #looks only at TrajPartitioned 
hasMethod("initialize","TrajPartitioned") #looks up the inheritance for TrajPartitioned
selectMethod ("initialize", "TrajPartitioned") #tells which method up the inheritance chain is executing


tdCochin <- new(Class = "TrajPartitioned",
                traj = trajCochin@traj,
                times = c(1,3,4,5),
                listPartitions = list(partCochin,partCochin2) 
)

tdCochin
print(tdCochin)

### section 9.6 ###
# avoid callNextMethod issues with 'as'
print(as(tdPitie,"Trajectories"))

is(trajCochin,"TrajPartitioned")
is(tdCochin,"TrajPartitioned")
is(tdCochin,"Trajectories")

### Creation of empty TrajPartitioned
tdStAnne <- new("TrajPartitioned")

### Assignment of Trajectories to the attributes of TrajPartitioned 
##### as(objectSon,"ClassFather") <- objectFather ####
as(tdStAnne,"Trajectories") <- trajStAnne
tdStAnne

### see object of class TrajPartitioned as a Partition ###
## specific instance of a class is called so doesn't belong in the class file
## can add a fifth argument, the function test: 
## it subordinates the transformation of class1 into class2 following a condition
## this setIs replaces the partition having the smallest number of groups by a new one
setIs(
  class1="TrajPartitioned",
  class2="Partition",
  coerce=function(from,to){
    numberGroups <- sapply(tdCochin@listPartitions,getNbGroups)
    Smallest <- which.min(-numberGroups)
    to<-new("Partition")
    to@nbGroups <- getNbGroups(from@listPartitions[[Smallest]])
    to@part <- getPart(from@listPartitions[[Smallest]])
    return(to) 
  },
  replace=function(from,value){
    numberGroups <- sapply(tdCochin@listPartitions,getNbGroups)
    smallest <- which.min(numberGroups)
    from@listPartitions[[smallest]] <- value
    return(from) 
  }
)

as(tdCochin,"Partition")
as(tdCochin,"Partition") <- partCochin2

### section 9.8
### Virtual classes allow one method written for two classes
### since the method applies to the parent,
### both children run the same method (method looks up the inheritence chain)
a <- new("PartitionSimple",nbGroups=3,part=factor(LETTERS[c(1,2,3,2,2,1)]))
nbMultTwo(a)

b <- new("PartitionEval",nbGroups=5,part=ordered(LETTERS[c(1,5,3,4,2,4)])) 
nbMultTwo(b)

### section 10.2 
### impute method creates a local object .Object, 
### it modifies the local trajectories (impute by average) and returns an object. 
### Impute(trajCochin) did not modify trajCochin.
impute(trajCochin) # local replacement of NA with the average
trajCochin # globally, the data in trajCochin is unchanged