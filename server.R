library(visNetwork) 
library(shiny)
library(shinydashboard)
library(expm)

#library(shinyjqui)
#library(plotly)
library(V8) 
library(shinyjs)

# Global variables can go here


vis_network<-function(nodes,edges,edge_smooth){
  
  nodes$shape <- "dot"
  nodes$shadow <- TRUE # Nodes will drop shadow
  nodes$title <- paste(paste(nodes$id,paste("(weight:", nodes$weight)),")",sep="") # Text on click
  nodes$label <- nodes$id # Node label
  nodes$size <- nodes$weight*40+10 # Node size
  nodes$borderWidth <- 2 # Node border width

  nodes$font.size<-18

  edges$width <- 4*edges$weight # line width
  edges$arrows.to<-40*edges$weight
  edges$smooth <- as.logical(edge_smooth)  # should the edges be curved?
  edges$shadow <- FALSE    # edge shadow
  edges$title=paste(paste(edges$from,edges$to,sep='->'),round(edges$weight,2),sep=':')
  edges$label<-NULL

  visNetwork(nodes,edges) %>%
  visIgraphLayout()%>%
  visPhysics(solver = "forceAtlas2Based",
             #solver="hierarchicalRepulsion",
             forceAtlas2Based = list(gravitationalConstant = -500),
             maxVelocity=200,
             stabilization =list(enabled=TRUE,iterations=10,updateInterval=20)
             #stabilization =FALSE
  ) %>%
  #adaptiveTimestep=TRUE %>%
  #visEdges(arrows ="to") %>%
  #visEdges(smooth = TRUE) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
  visInteraction(navigationButtons = TRUE)
}




get_pass_nodes_list<-function(nodes,edges,source,target){
  
  printAllPathsUtil<-function(u, d, visited, path){ 
    
    if(!u %in% local_nodes_list){
      # Mark the current node as visited and store in path 
      visited[[u]]<-TRUE
      # path<-c(path,u) 
      # If current vertex is same as destination, then print 
      # current path[] 
      if(u==d){ 
        local_nodes_list<<-unique(c(local_nodes_list,path))
        local_nodes_list<<-local_nodes_list[local_nodes_list!=source]
        path<-c(path,u) 
        #print(paste(path,collapse=">"))
      }
      else{ 
        path<-c(path,u) 
        # If current vertex is not destination 
        #Recur for all the vertices adjacent to this vertex 
        for(i in as.vector(edges[which(edges$from==u & edges$weight>0),'to'])){
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
  printAllPaths<-function(source,target){
    
    # Mark all the vertices as not visited 
    visited <-rep(FALSE,length(nodes$id))
    names(visited)<-nodes$id
    
    # Create an array to store paths
    path <- c() 
    
    # Call the recursive helper function to print all paths 
    printAllPathsUtil(source, target,visited, path) 
  }
  
  local_nodes_list<-c()
  printAllPaths(source,target)
  return(c(c(source,target),local_nodes_list))
}


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
    
    if(sum(tm[i,])<1.e-9 | i==target){
      if(i!=target){
        absorb_node<-i
      }
      tm[i,]<-0
      tm[i,i]<-1
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
  
  to_target_probability <-to_target_probability[order(-to_target_probability$CVR),]
  
  inter_states<-nodes[! nodes %in% c(source,target,absorb_node)]
  
  removal_effect<-data.frame()
  
  for(state in inter_states){
    
    tmp_trans<-tm
    
    tmp_trans[,absorb_node]<-tmp_trans[,absorb_node]+tmp_trans[,state]
    
    tmp_trans<-tmp_trans[!rownames(tmp_trans) %in% c(state),!colnames(tmp_trans) %in% c(state)]
    
    new_cvr<-matrix_power(tmp_trans,500)[source,target]
    
    removal_effect<-rbind(removal_effect,data.frame(removed_node=state,CVR_change_rate=1-new_cvr/origin_cvr))
  }
  
  removal_effect<-removal_effect[order(-removal_effect$CVR_change_rate),]
  removal_effect<-cbind('original_CVR'=rep(origin_cvr,nrow(removal_effect)),removal_effect)
  rownames(removal_effect)<-c()
  
  
  return(list(to_target_probability=to_target_probability,removal_effect=removal_effect))
  
  
}



# Define the server code
server <- function(input, output,session) {
  
  values<-reactiveValues()
  values$data<-data.frame()
  values$nodes<-data.frame()
  values$edges<-data.frame()
  values$data_feed<-0
  values$platform_list<-c()
  values$tm_data<-read.csv("data/tm_data_dummy.csv")
  
  market_list<-reactive({
    unique(values$tm_data$market)
  })
  
  observeEvent(input$market,{
    values$platform_list<-unique(values$tm_data[which(values$tm_data$market==input$market),'platform'])
  })
  
  output$choose_market <-renderUI({
  selectInput(inputId="market", label="Market", 
              choices=market_list(),selected=market_list()[1],multiple = FALSE,width=200)
  })
  
  output$choose_platform <-renderUI({
  selectInput(inputId="platform", label="Platform", 
              choices=values$platform_list,selected=values$platform_lis[1],multiple = FALSE,width=200)
  })
  
  observeEvent(input$file,{
    values$data<<-read.table(input$file$datapath, header=TRUE, sep=',',stringsAsFactors=FALSE)
    values$data_feed<<-values$data_feed+1
    if(values$data_feed==3){values$data_feed<-1}
  })
  
  
  observeEvent(input$loaddata,{
    tmp_data<-values$tm_data
    values$data<<-tmp_data[which(tmp_data$market==input$market & tmp_data$platform==input$platform),]
    values$data_feed<<-values$data_feed+1
    if(values$data_feed==3){values$data_feed<-1}
    })
  
  observeEvent(values$data_feed,{
      
  if(values$data_feed!=0){
      
    data<-values$data
    #values$Data<-read.csv("/Users/pengzhang/OneDrive - Skyscanner Ltd/project/APP_analysis/markov_analysis/simple_visualization/KR_3M_20000_sample.csv")
    
    nodes<-unique(data[,c('from','weight','group')])
    colnames(nodes)<-c('id','weight','group')
    nodes<-nodes[order(-nodes$weight),]
    
    edges <- data[,c('from','to','transition_probability')]
    colnames(edges)<-c("from","to","weight")
    
    values$nodes<<-nodes
    values$edges<<-edges
    
    output$full_plot <- renderVisNetwork({
    
      nodes<-values$nodes
      edges<-values$edges
      
      valid_nodes_list<-nodes[which(nodes$weight>=10**(input$weight_threshold)),'id']
      sub_nodes<-nodes[which(nodes$id %in% valid_nodes_list),]
      sub_edges<-edges[which(edges$from %in% valid_nodes_list & edges$to %in% valid_nodes_list &  edges$weight>input$transition_threshold),]
      
      vis_network(sub_nodes,sub_edges,input$edge_smooth)
      
    })
    
    output$custom_from<-renderUI({
      selectInput(inputId="custom_from", label="Choose path start", 
                   choices=values$nodes$id,selected=values$nodes[1,'id'],multiple = FALSE)
    })
    
    output$custom_to <-renderUI({
      selectInput(inputId="custom_to", label="Choose path end", 
                  choices=values$nodes$id,selected=values$nodes[2,'id'],multiple = FALSE)
    })
    
    output$path_analysis<-renderUI({
      actionButton(inputId="path_analysis", label="Visualize")
    })
  
    
    output$conversion_start<-renderUI({
      selectInput(inputId="conversion_start", label="Choose conversion session start", 
                  choices=values$nodes$id,selected=values$nodes[1,'id'],multiple = FALSE)
    })
    
    output$conversion_stop<-renderUI({
      selectInput(inputId="conversion_stop", label="Choose conversion session stop", 
                  choices=values$nodes$id,selected=values$nodes[2,'id'],multiple = FALSE)
      
    })
    
    
    output$attribution_analysis<-renderUI({
      actionButton(inputId="attribution_analysis", label="Analyze")
    })
    

    output$raw_data <- renderDataTable({
        return(values$data)
    },options = list(lengthMenu = c(10, 25, 50, 100), pageLength = 30))   
  }  
})
  
  
  
  
  observeEvent(input$path_analysis,{

    output$custom_plot <- renderVisNetwork({
      
      nodes<-values$nodes
      edges<-values$edges
      source<-input$custom_from
      target<-input$custom_to
      
      valid_nodes_list<-get_pass_nodes_list(nodes,edges,source,target)
      valid_nodes_list<-nodes[which(nodes$weight>=10**(input$weight_threshold) & nodes$id %in% valid_nodes_list),'id']
      
      sub_nodes<-nodes[which(nodes$id %in% valid_nodes_list),]
      sub_edges<-edges[which(edges$from %in% valid_nodes_list & edges$to %in% valid_nodes_list &  edges$weight>input$transition_threshold),]
      
      
      vis_network(sub_nodes,sub_edges,input$edge_smooth)
      
    })
    
  })  
  
  
  observeEvent(input$attribution_analysis,{
    res<-attribution(input$conversion_start,input$conversion_stop,values$edges)
    
    output$attribution_table <- renderDataTable({
      return(res$removal_effect)
    },options = list(pageLength = nrow(res$removal_effect)))
    
    output$to_target_cvr_table <- renderDataTable({
      return(res$to_target_probability)
    },options = list(pageLength = nrow(res$to_target_probability)))
    
  })
} 


