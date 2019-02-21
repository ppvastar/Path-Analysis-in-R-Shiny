library(igraph)
library(reshape)
library(expm)
library(data.table)


data<-read.csv("/Users/pengzhang/OneDrive - Skyscanner Ltd/project/APP_analysis/markov_analysis/simple_visualization/KR_3M_20000_sample.csv")

#data<-values$Data

valid_node_list<-c(NA,as.vector(unique(data[which(data$weight>=0),'from'])))

data<-data[which(data$from %in% valid_node_list & data$to %in% valid_node_list),]

nodes<-unique(data[,c('from','weight','group')])
colnames(nodes)<-c('id','weight','group')

nodes<-nodes[order(-nodes$weight),]
edges <- data[,c('from','to','transition_probability')]
colnames(edges)<-c("from","to","weight")
edges<-edges[which(edges$weight>0),]


printAllPathsUtil<-function(u, d, visited, path){ 
  
  if(!u %in% nodes_list){
  # Mark the current node as visited and store in path 
  visited[[u]]<-TRUE
# path<-c(path,u) 
# If current vertex is same as destination, then print 
# current path[] 
  if(u==d){ 
    nodes_list<<-unique(c(nodes_list,path))
    nodes_list<<-nodes_list[nodes_list!=source]
    path<-c(path,u) 
    print(paste(path,collapse=">"))
  }
  else{ 
    path<-c(path,u) 
  # If current vertex is not destination 
  #Recur for all the vertices adjacent to this vertex 
    for(i in as.vector(data[which(data$from==u & data$transition_probability>0),'to'])){
      if(visited[[i]]==FALSE){ 
        printAllPathsUtil(i, d, visited, path) 
      }
    }
  }
# Remove current vertex from path[] and mark it as unvisited 
  path<-path[-length(path)]
  visited[[u]]<-FALSE

  }  
}


# Prints all paths from 's' to 'd' 
printAllPaths<-function(){
  
  # Mark all the vertices as not visited 
  visited <-rep(FALSE,length(nodes$id))
  names(visited)<-nodes$id

# Create an array to store paths
  path <- c() 

# Call the recursive helper function to print all paths 
  printAllPathsUtil(source, target,visited, path) 
}

nodes_list=c()
source='AppStart'
target='FlightsBook'
printAllPaths()
print(nodes_list)


data<-data[which(data$from %in% valid_node_list & data$to %in% valid_node_list),]

from_nodes<-data[,c('from','weight','group')]
to_nodes<-data[,c('to','weight','group')]
colnames(from_nodes)<-c('id','weight','group')
colnames(to_nodes)<-c('id','weight','group')
nodes<-unique(rbind(from_nodes,to_nodes))

edges <- data[,c('from','to','transition_probability')]
colnames(edges)<-c("from","to","weight")
edges<-edges[which(edges$weight>input$transition_threshold),]



attribution<-function(source,target,edges){
  
  matrix_power<-function(mt,n){
    if(n%%2==0){
      if(n>2){
        return(matrix_power(mt,n/2)%^%2)
      }
      else{
        return(mt%^%2)
      }
    }
    else{
      if(n>1){
        return(matrix_power(mt,(n-1))%*%mt)
      }
      else{
        return(mt)
      }
    }
    
  }

  
  nodes<-unique(c(as.vector(edges$from),as.vector(edges$to)))
  nodes<-nodes[!is.na(nodes)]
    
  tm<-matrix(0,nrow=length(nodes),ncol=length(nodes))
  dimnames(tm)<-list(nodes,nodes)
  
  absorb_node<-NA
  
  for (i in nodes){
    for (j in nodes){
      v<-edges[which(edges$from==i & edges$to==j),'weight']
      if(length(v)>0){
        tm[i,j]<-v
      }
    }
    
    if(abs(sum(tm[i,]))<1.e-9 | i==target){
      if(i!=target){
        absorb_node<-i
      }
      tm[i,]<-0
      tm[i,i]<-1
    }
    
    if(abs(sum(tm[i,])-1)>1.e-9){
      print("error")
    }
  }
  
  if(is.na(absorb_node)){
    tm<-rbind(tm,"dummy_null"=rep(0,length(nodes)))
    tm<-cbind(tm,"dummy_null"=rep(0,length(nodes)+1))
    tm["dummy_null","dummy_null"]<-1
    absorb_node<-"dummy_null"
  }
  
  
  origin_cvr<-matrix_power(tm,500)[source,target]
  to_target_probability<-matrix_power(tm,500)[,target]
  to_target_probability <- as.data.frame(cbind(from = names(to_target_probability), CVR=as.vector(to_target_probability)),stringsAsFactors=FALSE)
  to_target_probability$CVR<-as.numeric(to_target_probability$CVR)
  to_target_probability<-to_target_probability[order(-to_target_probability$CVR),]

  inter_states<-nodes[! nodes %in% c(source,target,absorb_node)]
  
  removal_effect<-data.frame()
  
  for(state in inter_states){
    
    tmp_trans<-tm
    
    tmp_trans[,absorb_node]<-tmp_trans[,absorb_node]+tmp_trans[,state]
    
    tmp_trans<-tmp_trans[!rownames(tmp_trans) %in% c(state),!colnames(tmp_trans) %in% c(state)]
    
    new_cvr<-matrix_power(tmp_trans,500)[source,target]
    
    removal_effect<-rbind(removal_effect,data.frame(removal_state=state,CVR_change_rate=1-new_cvr/origin_cvr))
  }

  removal_effect<-removal_effect[order(-removal_effect$CVR_change_rate),]
  rownames(removal_effect)<-c()

  
  return(list(to_target_probability=to_target_probability,removal_effect=removal_effect))
  
}



source="AppStart"
target="FlightsBook"

res<-attribution(source,target,edges)
