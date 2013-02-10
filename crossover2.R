#funkcja krzyzująca 
# param p - populacja po selekcji
# return o - child
#wybiera losowo dwa osobniki populacji
#parent 1 donates a swath genetic material and the corresponding swath 
#from the other parent is sprinkled about in the child. Once that is done, 
#the remaining alleles are copied direct from parent 2.


pmxCrossoverTable2<-function(p, parentIndex)
{
  
  parent1<-p[[parentIndex]]
  parent2<-p[[sample.int(length(p),1)]]
  tableLength<-length(parent1$coordinates)
  min<-sample.int(tableLength,1)
  max<-0
  
  if(min<=floor(tableLength/2)){
    max<-min+ceiling(tableLength/2)
  } else {
    max<-min
    min<-max-floor(tableLength/2)
  }
  
  child<-list(coordinates=array(0, tableLength), quality=NA)
  
  for(i in min:max){
    child$coordinates[[i]]<-parent1$coordinates[[i]]
  }

  tempAr<-list(NULL)
  indAr<-list(NULL)
  count=1
  
  for(i in min:max){
    found=FALSE
    currVal<-parent2$coordinates[[i]]
    for(j in min:max){
      if(parent1$coordinates[[j]]==currVal){
        found<-TRUE
        break
      }
      j<-j+1
    }
    if(!found){
      tempAr[[count]] <-currVal
      indAr[[count]]<-i
      count<-count+1}
    i<-i+1
    
  }
  
  if (count != 1) {
    for(i in 1:length(tempAr)){
      nextV <- repeatAction2(tempAr[[i]], tempAr, indAr, parent1, parent2, child,min, max)
      cnt = 0
      
      while((nextV > 0) && (cnt < length(tempAr))){
        nextV<-repeatAction2(nextV, tempAr, indAr, parent1, parent2, child, min, max)
        cnt <- cnt+1
      }

      ind=0
      for(c in 1:tableLength){
        if(parent2$coordinates[[c]]==-nextV){
          found<-TRUE
          ind <- c
          break
        }
        c<-c+1
      }

      if(ind != 0) {
        
        v2<-parent1$coordinates[[ind]]

        for(j in 1:tableLength){
          if(parent2$coordinates[[j]]==v2){
            ind2<-j
            break
          }
          j<-j+1
        }
        
        child$coordinates[[ind2]]<- tempAr[[i]]       
        
        i<-i+1
      }
    }
  } 

  for(k in 1:tableLength){
    if(child$coordinates[[k]]==0){
      child$coordinates[[k]] <- parent2$coordinates[[k]]
    }
    k<-k+1
  }
  
  for(a in 1:(tableLength-1)){
    for(b in (a+1):tableLength){
      if(child$coordinates[[a]]==child$coordinates[[b]]){

        for(e in 1:tableLength){
          ind3 = 0
          for(d in 1:tableLength){
            if(child$coordinates[[d]]==e){
              ind3 = d}
          }
          
          if(ind3 == 0){
            child$coordinates[[a]] <- e
          }
          
        }
      }
    }
  }
  
  return (child)
  
}


repeatAction2 <- function (v, tempAr, indAr, parent1, parent2, child,min, max){
  goOn=NULL
  ind4=NULL
  found=FALSE
  tableLength<-length(parent1$coordinates)

  for(i in 1:tableLength){
    if(parent2$coordinates[[i]]==v){
      found<-TRUE
      ind4 <- i
      break
    }
    i<-i+1
  }

  v2<-parent1$coordinates[[ind4]]

  for(j in 1:tableLength){
    if(parent2$coordinates[[j]]==v2){
      ind2<-j
      break
    }
    j<-j+1
  }

  if((ind2>=min)&&(ind2<=max)){
    goOn <- v2

  }
  else{
    goOn <- -v
  }
  
  return(goOn)
}