pmxCrossoverTable2<-function(p)
{
    parent1<-p[[sample.int(length(p),1)]]
    parent2<-p[[sample.int(length(p),1)]]
    tableLength<-length(parent1$coordinates)
     
    min<-sample.int(tableLength,1)
    max<-min
    
    if(min<=floor(tableLength/2)){
      max<-min+ceiling(tableLength/2)
    }
    else {
      min<-max-floor(tableLength/2)
    }
  
     child<-list(coordinates=array(0, tableLength), quality=NA)
     
     sizeMaxMin<-max-min
     
     tempTable1<-array(0, sizeMaxMin)
     tempTable2<-array(0, sizeMaxMin)
     
     for(i in 1:sizeMaxMin)
     {
       tempTable1[[i]]<-parent1$coordinates[[min+i-1]]
       tempTable2[[i]]<-parent2$coordinates[[min+i-1]]
     }
     tempTable1<-sort(tempTable1)
     tempTable2<-sort(tempTable2)
     
     tempTableNoI1<-array(NA, 0)
     tempTableNoI2<-array(NA, 0)
     tempTableI<-array(NA, 0)
     
     ct1<-1
     ct2<-1
     while((ct1<=sizeMaxMin) && (ct2<=sizeMaxMin))
     {
       if(tempTable1[[ct1]]<tempTable2[[ct2]]){
         tempTableNoI1[[(length(tempTableNoI1)+1)]]<-tempTable1[[ct1]]
         ct1<-ct1+1
       } else if(tempTable1[[ct1]]>tempTable2[[ct2]]){
         tempTableNoI2[[(length(tempTableNoI2)+1)]]<-tempTable2[[ct2]]
         ct2<-ct2+1
       } else{
         tempTableI[[(length(tempTableI)+1)]]<-tempTable1[[ct1]]
         ct1<-ct1+1
         ct2<-ct2+1
       }
     }
     while(ct1<=sizeMaxMin){
       tempTableNoI1[[(length(tempTableNoI1)+1)]]<-tempTable1[[ct1]]
       ct1<-ct1+1
     }
     while(ct2<=sizeMaxMin){
       tempTableNoI2[[(length(tempTableNoI2)+1)]]<-tempTable2[[ct2]]
       ct2<-ct2+1
     }
     tempIndex<-1
     for(i in 1:tableLength){
       if(i>=min && i<max){
         child$coordinates[[i]]<-parent2$coordinates[[i]]
       }
       else{
         found<-FALSE
         if(length(tempTableNoI2)>0){
           for(j in 1:length(tempTableNoI2)){
             if(tempTableNoI2[[j]]==parent1$coordinates[[i]]){
               child$coordinates[[i]]<-tempTableNoI1[[tempIndex]]
               tempIndex<-tempIndex+1
               found<-TRUE
               break
             }
           }
           if(!found){
             child$coordinates[[i]]<-parent1$coordinates[[i]]
           }
         }
         else{
           child$coordinates[[i]]<-parent1$coordinates[[i]]
         }
       }
     }
     
     return (child)  
}