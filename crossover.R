#funkcja krzyzująca 
# param p - populacja po selekcji
# return o - child
#wybiera losowo dwa osobniki populacji
#parent 1 donates a swath genetic material and the corresponding swath 
#from the other parent is sprinkled about in the child. Once that is done, 
#the remaining alleles are copied direct from parent 2.


pmxCrossoverTable<-function(p)
{
  #randomly pick parent 1
  parent1<-p[[sample.int(length(p),1)]]
  #print("parent 1")
  #print(parent1)
  #randomly pick parent 2
  parent2<-p[[sample.int(length(p),1)]]
  #print("parent2")
  #print(parent2)
  # randomly pick a swath of 50 alleses from parent 1
  # choose the first position of a swath
  first<-sample.int(100,1)
  #calculate the last position of a swath
  if(first<51){
    last = first+50
  }
  else last=first+50-100
  #print(first)
  #print(last)
  
  #initialize child
  #print("child")
  child<-list(coordinates=array(0, 100), quality=NA)
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
      j<-j+1
      }
      if(!found){
      tempAr[[count]] <-parent2$coordinates[[i]]
      indAr[[count]]<-i
      count<-count+1}
    i<-i+1
    
  }
  #print("tempAr")
  #print(tempAr)
  
 
  if (length(tempAr) > 1){
  #for each of these values
  for(i in 1:length(tempAr)){
    nextV <- repeatAction(tempAr[[i]], tempAr, indAr, parent1, parent2, child,min, max)
    cnt = 0
    # if the value is a part of the swath
    
    while((nextV > 0) && (cnt < length(tempAr))){
      nextV<-repeatAction(nextV, tempAr, indAr, parent1, parent2, child, min, max)
      cnt <- cnt+1
    }
    # if it is not a part of a swath, copy directly into child
    #index<-indAr[[i]]
    #v<-parent1$coordinates[[index]]
    #for(j in 1:100){
    #if(parent2$coordinates[[j]]==-nextV){
    # print("back to main: value to copy")
    # print(-nextV)
    # print("at index")
    # index2<-j
    # print(index2)
    #  break
    # }
    # j<-j+1
    #  }
    # note the index of the value in parent2
    for(c in 1:100){
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
    v2<-parent1$coordinates[[ind]]
    #print("MAIN located the value2 from parent1 in this same position")
    #print(v2)
    #locate the index of the value2 element in parent2; 
    for(j in 1:100){
      if(parent2$coordinates[[j]]==v2){
        ind2<-j
        break
      }
      j<-j+1
    }
    #print("MAIN the index of the value2 element in parent2")
    #print(ind2) 
    
    child$coordinates[[ind2]]<- tempAr[[i]]
    #print("MAIN copied value")
    #print(tempAr[[i]])
    #print("at index")
    #print(ind2) 
    
    
    i<-i+1
  }
  #print(child)
  } 
  # copy any remaining positions from parent to child
  for(k in 1:100){
    if(child$coordinates[[k]]==0){
      child$coordinates[[k]] <- parent2$coordinates[[k]]
    }
    k<-k+1
  }
  
  #print(child)
  for(a in 1:99){
    for(b in (a+1):100){
    if(child$coordinates[[a]]==child$coordinates[[b]]){
      print("NOT GOOD ONE")
    }
    b<-b+1
    }
    a<-a+1
  }
  
  return (child)

}
  

repeatAction <- function (v, tempAr, indAr, parent1, parent2, child,min, max){
  # print("repeatAction for value")
  #print(v)
  goOn=NULL
  ind=NULL
  found=FALSE
  # note the index of the value in parent2
  for(i in 1:100){
    if(parent2$coordinates[[i]]==v){
      found<-TRUE
      ind <- i
      break
    }
    i<-i+1
  }
  #print("the index of the value in parent2")
  #print(ind)
  #locate the value2 from parent1 in this same position
  v2<-parent1$coordinates[[ind]]
  #print("located the value2 from parent1 in this same position")
  #print(v2)
  #locate the index of the value2 element in parent2; 
  for(j in 1:100){
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