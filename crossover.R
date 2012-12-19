#funkcja krzyzująca 
# param p - populacja wejściowa
# return o - populacja po krzyżowaniu
#wybiera losowo dwa osobniki populacji
#parent 1 donates a swath genetic material and the corresponding swath 
#from the other parent is sprinkled about in the child. Once that is done, 
#the remaining alleles are copied direct from parent 2.


pmxCrossoverTable<-function(p)
{
  #randomly pick parent 1
  parent1<-p[[sample.int(100,1)]]
  print("parent 1")
  print(parent1)
  #randomly pick parent 2
  parent2<-p[[sample.int(100,1)]]
  print("parent2")
  print(parent2)
  # randomly pick a swath of 50 alleses from parent 1
  # choose the first position of a swath
  first<-sample.int(100,1)
  #calculate the last position of a swath
  if(first<50){
    last = first+49
  }
  else last=first+49-100
  print(first)
  print(last)
  
  #initialize child
  print("child")
  child<-list(coordinates=array(0, 100), quality=NA)
  print(child)
  
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
  
  print(child)
  
  
  #looking into the same segment position in parent2 look for values 
  # that were not in parent1
  tempAr<-list(NULL)
  indAr<-list(NULL)
  count=1
  found=FALSE
  for(i in min:max){
    currVal<-parent2$coordinates[[i]]
    for(j in min:max){
    if(parent2$coordinates[[i]]==parent1$coordinates[[j]]){
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
  print("tempAr")
  print(tempAr)
  
  #for each of these values
  for(i in 1:length(tempAr)){
    # find the element at the same index in parent1
    index<-indAr[[i]]
    v<-parent1$coordinates[[index]]
   print(v)
    #locate the index of the element in parent2; 
    for(j in 1:100){
      if(parent2$coordinates[[j]]==v){
        index2<-j
        break
      }
    j<-j+1
    }
    # if it is not a part of a swath
    if((index2<min)||(index2>max)){
      #insert into child at that position
      print("!!!")
      child$coordinates[[index2]]<-parent2$coordinates[[index2]]
    }
    
    i<-i+1
  }
  print(child)
  
  
  
  return (p)
}