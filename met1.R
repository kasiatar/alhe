#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################




#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
  selectedPoints<-c(historyPop(history, 99), list(model$bestPoint))
  selectedPoints<-tournamentSelection(selectedPoints)
  return(selectedPoints)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel)
{
  newModel<-oldModel
  for(i in 1:length(selectedPoints)){
    if(selectedPoints[[i]]$quality<newModel$bestPoint$quality){
      newModel$bestPoint<-selectedPoints[[i]]
    }
  }
  return (newModel)
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
  newPoints<-array(list(NULL), length(selectedPoints))
  pc<-0.7
  for(i in 1:length(newPoints)){
    if(runif(1) < pc){
      newPoints[[i]]<-pmxCrossoverTable(selectedPoints)
    }
    else{
      newPoints[[i]]<-selectedPoints[[i]]
    }
    newPoints[[i]]<-switchMutationTable(newPoints[[i]])
  }
  newPoints<-c(list(model$bestPoint), newPoints)
  return (newPoints)
}

#initializes model based on history
# returns model
initModel<-function(history)
{
  model<-list(bestPoint=history[[1]])
  for(i in 2:length(history)){
    if(history[[i]]$quality<model$bestPoint$quality){
      model$bestPoint<-history[[i]]
    }
  }
  return (model)
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{
  
  selectedPoints<-selection(history, oldModel)
  newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, newModel)
  return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
  history<-evaluateList(history, evaluation)
  model<-initModel(history)
  i<-1
  print(c(i, model$bestPoint$quality))
  while (!termination(history,model))
  {
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
    i<-i+1
    print(c(i, model$bestPoint$quality))
  }
  return(history)
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints)
  return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
  stop=length(history)
  start=max(stop-number+1,1)
  return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points)){
    points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  }
  return (points) 
}


####  THAT'S ALL FOLKS
