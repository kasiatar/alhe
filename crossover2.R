# funkcja krzyzująca 
# param p - populacja po selekcji
# return o - child
# wybiera losowo dwa osobniki populacji
# parent 1 donates a swath genetic material and the corresponding swath 
# from the other parent is sprinkled about in the child. Once that is done, 
# the remaining alleles are copied direct from parent 2.


pmxCrossoverTable2<-function(p)
{
  #randomly pick parent 1
  #print(length(p))
  parent1<-p[[sample.int(length(p),1)]]
  #print("parent 1")
  #print(parent1)
  #randomly pick parent 2
  parent2<-p[[sample.int(length(p),1)]]
  #print("parent2")
  #print(parent2)
  # randomly pick a swath of 50 alleses from parent 1
  # choose the first position of a swath
  first<-sample.int(250,1)
  #calculate the last position of a swath
  if(first<126){
    last = first+124
  }
  else last=first+124-249
  #print(first)
  #print(last)
  
  #initialize child
  child<-list(coordinates=array(0, 250), quality=NA)
  #print(child)
  
  if(first>last){
    min<-last
    max<-first}
  else
  {
    min<-first
    max<-last}
  
  #print(min)
  #print(max)
  
  #copy directly into child
  for(i in min:max){
    child$coordinates[[i]]<-parent1$coordinates[[i]]
  }
  
  #print(child)
  
  
  #looking into the same segment position in parent2 look for values 
  # that were not copied from parent1
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
    }
    if(!found){
      tempAr[[count]] <-parent2$coordinates[[i]]
      indAr[[count]]<-i
      count<-count+1
    }
  }
  #print("tempAr")
  #print(tempAr)
  
  if (count != 1) {
    #for each of these values
    for(i in 1:length(tempAr)){
      nextV <- repeatAction2(tempAr[[i]], tempAr, indAr, parent1, parent2, child,min, max)
      cnt = 0
      # if the value is a part of the swath
      while((nextV > 0) && (cnt < length(tempAr))){
        nextV<-repeatAction2(nextV, tempAr, indAr, parent1, parent2, child, min, max)
        cnt <- cnt+1
      }
      
      # note the index of the value in parent2
      ind=0
      for(c in 1:250){
        if(parent2$coordinates[[c]]==-nextV){
          found<-TRUE
          ind <- c
          break
        }
        c<-c+1
      }
      #print("MAIN the index of the value in parent2")
      #print(ind)
      #locate the value2 from parent1 in this same position
      if(ind != 0) {
        v2<-parent1$coordinates[[ind]]
        #print("MAIN located the value2 from parent1 in this same position")
        #print(v2)
        #locate the index of the value2 element in parent2; 
        for(j in 1:250){
          if(parent2$coordinates[[j]]==v2){
            ind2<-j
            break
          }
        }
        #print("MAIN the index of the value2 element in parent2")
        #print(ind2) 
        
        # if it is not a part of a swath, copy directly into child
        child$coordinates[[ind2]]<- tempAr[[i]]
        #print("MAIN copied value")
        #print(tempAr[[i]])
        #print("at index")
        #print(ind2) 
      }
    }
    #print(child)
  } 
  # copy any remaining positions from parent 2 to child
  for(k in 1:250){
    if(child$coordinates[[k]]==0){
      child$coordinates[[k]] <- parent2$coordinates[[k]]
    }
    k<-k+1
  }
  
  #print(child)
  #additional check
  for(a in 1:249){
    for(b in (a+1):250){
      if(child$coordinates[[a]]==child$coordinates[[b]]){
        #print(child)
        #search for missing value
        for(e in 1:250){
          ind3 = 0
          for(d in 1:250){
            if(child$coordinates[[d]]==e){
              ind3 = d}
          }
          
          if(ind3 == 0){
            # print(e)
            child$coordinates[[a]] <- e
          }
          
        }
        
        # print("NOT GOOD ONE")
        
      }
    }
  }
  
  return (child)
  
}


repeatAction2 <- function (v, tempAr, indAr, parent1, parent2, child,min, max){
  # print("repeatAction for value")
  #print(v)
  goOn=NULL
  ind4=NULL
  found=FALSE
  # note the index of the value in parent2
  for(i in 1:250){
    if(parent2$coordinates[[i]]==v){
      found<-TRUE
      ind4 <- i
      break
    }
    i<-i+1
  }
  #print("the index of the value in parent2")
  #print(ind)
  #locate the value2 from parent1 in this same position
  v2<-parent1$coordinates[[ind4]]
  #print("located the value2 from parent1 in this same position")
  #print(v2)
  #locate the index of the value2 element in parent2; 
  for(j in 1:250){
    if(parent2$coordinates[[j]]==v2){
      ind2<-j
      break
    }
    j<-j+1
  }
  #print("the index of the value2 element in parent2")
  #print(ind2)
  #if the index of this value in parent 2 is part of original swath 
  #call repeat using this value
  if((ind2>=min)&&(ind2<=max)){
    goOn <- v2
    #print("is part of original swath")
    #repeat(index2, tempAr, indAr, parent1, parent2, min, max)
  }
  #if it is not a part of the original swath, insert step A's value
  #into the child in this position
  else{
    #print("is NOT part of original swath")
    goOn <- -v
  }
  
  return(goOn)
}