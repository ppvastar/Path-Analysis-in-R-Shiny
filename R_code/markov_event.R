  library("RMixpanel")
  library("sqldf")
  library("readr")
  library("parallel")
  library("plyr")
  library("party")
  library("rjson")
  library("rpart")
  library("rpart.plot")	
  library("ChannelAttribution")
  library("reshape2")
  library("ggplot2") 
  library("expm")
  library("stringr")
  library("expm")
  library("visNetwork")
  
  setwd("/Users/pengzhang/OneDrive - Skyscanner Ltd/project/APP_analysis/markov_analysis/")
  
  platform="IOS APP"
  account = mixpanelCreateAccount("Go iOS",
                                  token="6983e0842c92d9fe21ab1fcac2a7fade",
                                  secret="43dcb43120fe556966b4f75115bd2104", 
                                  key="8f14d7ce0ac11bac4a0d9d54e2dffbab")
  
  
  get_mixpanel_data<-function(date_var){
    
    jql_script<-gsub("date_par",date_var,tmp_script)
    
    error<-TRUE
    while(error){
      tryCatch(
        {data<-mixpanelJQLQuery(account, jql_script)
        error<-FALSE
        }
        ,error=function(e){
          cat(conditionMessage(e),"\n\n")
          Sys.sleep(5)
        }
      )
    }
    return(data)
    
  }
  
  events_list='["AppClose",
  "AppStart",
  "CarHireBookingDetails_CarHireBook",
  "FlightsBookingDetails_ContinueBookingButton_Tapped",
  "FlightsBookingDetails_FlightsBook",
  "FlightsDayView_DayViewItem_Tapped",
  "FlightsDayView_Search",
  "HotelDetails_ImageGallery_Selected",
  "HotelDetails_ImageGallery_Swipe",
  "HotelsBookingDetails_HotelsBook",
  "HotelsBook",
  "HotelsPerformance",
  "InspirationFeed_Country_Tapped",
  "InspirationFeed_InspirationFeedCell_Selected",
  "InspirationFeed_Search",
  "MapView_HotelsMap_MapAnnotationSelected",
  "NavigationTabBar_NavigationTabBar_TabChanged",
  "ProfileHome_ProfileHome_ItemSelected",
  "ScreenshotHelperView_ScreenshotUploadFinished",
  "SearchHome_CarHireSearchButton_Tapped",
  "SearchHome_FlightSearchButton_Tapped",
  "SearchHome_HotelSearchButton_Tapped",
  "SearchHome_ExploreAll_Tapped",
  "SearchHome_ExploreCarousel_ItemSelected",
  "SearchHome_RecentlyViewedItems_Item_Tapped",
  "SearchHome_RecentSearches_Item_Tapped",
  "TopDeals_Search"
  ]'  
  
  market_par="KR"
  
  
tmp_script<-read_file("markov_event.jql")
tmp_script<-gsub("events_list",events_list,tmp_script)
tmp_script<-gsub("market_par",market_par,tmp_script)

data_body1<-strsplit(strsplit(tmp_script, "/\\*body_end\\*/")[[1]][1],"/\\*body_begin\\*/")[[1]][2]
data_body2<-as.vector(strsplit(gsub(" ","",gsub("\n","",data_body1)),",")[[1]])
colname_list<-as.character(as.data.frame(t(sapply(1:length(data_body2), function(x) strsplit(data_body2, ":")[[x]])))$V1)

#begin_date<-as.Date('2018-01-01')
#end_date<-as.Date('2018-01-10')

end_date<-as.Date(Sys.time())
begin_date<-end_date-90

cl<-makeCluster(detectCores())
clusterEvalQ(cl,library("RMixpanel"))
clusterExport(cl,c("account","tmp_script"))
par_list<-seq(from=begin_date,to=end_date,by=1)
node_result<-parLapply(cl,par_list,get_mixpanel_data)
result<-do.call('rbind',node_result) 
stopCluster(cl)

#result<-unique(result)

colnames(result)<-colname_list

events<-result

events[which(!is.na(events$nexttab)),'event']=events[which(!is.na(events$nexttab)),'nexttab']
events[which(events$event=='SearchHome_FlightSearchButton_Tapped'),'event']='0_SearchHome_FlightSearchButton_Tapped'
events[which(events$event=='SearchHome_ExploreAll_Tapped'),'event']='0_SearchHome_ExploreAll_Tapped'

events<-events[,c("event","time","user_id")]
#events$utime<-as.POSIXct(as.numeric(events$time)/1000, origin="1970-01-01")



sample_users<-sample(unique(events$user_id),20000)

events_sample<-events[which(events$user_id %in% sample_users),]

events_sample_order<-unique(sqldf("select user_id,time,
                                  case 
                                  when event='HotelsBookingDetails_HotelsBook' then 'HotelsBook' 
                                  when event='FlightsBookingDetails_FlightsBook' then 'FlightsBook' 
                                  when event='CarHireBookingDetails_CarHireBook' then 'CarHireBook' 
                                  else event end as path
                                  from events_sample 
                                  order by user_id,time,path",drv="SQLite"))

events_sample_order<-events_sample_order[with(events_sample_order, c(path[-1]!= path[-nrow(events_sample_order)], TRUE)),]


events_sample_order[which(events_sample_order$path=='0_SearchHome_FlightSearchButton_Tapped'),'path']='SearchHome_FlightSearchButton_Tapped'
events_sample_order[which(events_sample_order$path=='0_SearchHome_ExploreAll_Tapped'),'path']='SearchHome_ExploreAll_Tapped'


path_data<-aggregate(path ~ user_id, data=events_sample_order, paste, collapse = ">")
clean_path_data<-path_data
clean_path_data$path <- gsub('AppClose>', 'AppClose;', clean_path_data$path)
split_path <- strsplit(clean_path_data$path, split = ";")
clean_path_data<-data.frame(user_id = rep(clean_path_data$user_id, sapply(split_path, length)), path = unlist(split_path))

clean_path_data<-clean_path_data[which(grepl('^AppStart',clean_path_data$path)),]
clean_path_data<-clean_path_data[which(grepl('AppClose$',clean_path_data$path)),]
clean_path_data<-clean_path_data[which(str_count(clean_path_data$path,'AppStart')==1),]
clean_path_data<-clean_path_data[which(str_count(clean_path_data$path,'AppClose')==1),]

channels=unique(events_sample_order$path)

df_trans=data.frame(channel_from=character(),channel_to=character(),transition_probability=numeric(),stringsAsFactors = FALSE)
channel_inflow<-data.frame(channel=character(),inflow=numeric(),stringsAsFactors = FALSE)

for(channel1 in channels){
  inflow<-sum(str_count(clean_path_data$path,channel1))
  channel_inflow<-rbind(channel_inflow,data.frame(channel=channel1,inflow=inflow))
}


path_data_group<-sqldf("select path,count(*) as total_null, 0 as total_conversions,0 as total_conversion_value from clean_path_data group by path order by total_null desc",drv="SQLite")


M <- markov_model(path_data_group,
                  var_path = 'path',
                  var_conv = 'total_conversions',
                  var_null = 'total_null',
                  var_value = 'total_conversion_value',
                  order=1,
                  out_more = TRUE)

df_trans<-M$transition_matrix
df_trans<-df_trans[which(df_trans$channel_from!='(start)'),]
df_trans<-df_trans[which(df_trans$channel_to!='(null)'),]


colnames(df_trans)<-c("from","to","transition_probability")

edges<-df_trans
colnames(edges)<-c("from","to","weight")
transition_threshold<-0
edges<-edges[which(edges$weight>transition_threshold),]

nodes<-channel_inflow

colnames(nodes)<-c('id','weight')
nodes$weight<-nodes$weight/max(nodes$weight)

grp<-c()
for (tmp_id in nodes$id){
  grp_id<-tmp_id

  if(tmp_id %in% c("SearchTab",
                   "CarHireBookingDetails_CarHireBook",
                   "FlightsBookingDetails_ContinueBookingButton_Tapped",
                   "FlightsDayView_DayViewItem_Tapped",
                   "FlightsDayView_Search",
                   "HotelDetails_ImageGallery_Selected",
                   "HotelDetails_ImageGallery_Swipe",
                   "HotelsBookingDetails_HotelsBook",
                   "HotelsPerformance",
                   "MapView_HotelsMap_MapAnnotationSelected",
                   "NearbyMap_Loaded",
                   "ScreenshotHelperView_ScreenshotUploadFinished",
                   "SearchHome_CarHireSearchButton_Tapped",
                   "SearchHome_ExploreAll_Tapped",
                   "SearchHome_ExploreCarousel_ItemSelected",
                   "SearchHome_ExploreCarousel_Scrolled",
                   "SearchHome_FlightSearchButton_Tapped",
                   "SearchHome_HotelSearchButton_Tapped",
                   "SearchHome_RecentlyViewedItems_Item_Tapped",
                   "SearchHome_RecentSearches_Item_Tapped"
                   )){
    grp_id<-"SearchHome"
  }
  
  if(tmp_id %in% c("ExploreTab",
                   "InspirationFeed_Country_Tapped",
                   "InspirationFeed_InspirationFeedCell_Selected",
                   "InspirationFeed_Search",
                   "TopDeals_Search"
   )){
    grp_id<-"ExploreHome"
  }
  
  if(tmp_id %in% c("MyTravelTab",
                   "MyTravelAddFlight_ActionButton_Tapped"
                   )){
    grp_id<-"MyTravelHome"
  }
  
  if(tmp_id %in% c("ProfileTab",
                   "ProfileHome_ProfileHome_ItemSelected")){
    grp_id<-"ProfileHome"
  }
  
  if(tmp_id %in% c("AppStart",
                   "AppClose")){
    grp_id<-"AppStart_Close"
  }
  
  if(tmp_id %in% c("FlightsBook",
                   "HotelsBook",
                   "CarHireBook")){
    grp_id<-"Book"
  }
  
  grp<-c(grp,grp_id)
  
}


nodes$group<-grp

output_data<-merge(x = df_trans, y = nodes, by.x = "from",by.y="id", all.x = TRUE)
output_data<-rbind(output_data,data.frame(from='AppClose',to=NA,transition_probability=NA,weight=nodes[which(nodes$id=='AppClose'),'weight'],group=nodes[which(nodes$id=='AppClose'),'group']))

output_data<-cbind(data.frame(market=market_par,platform=platform),output_data)

write.csv(output_data,paste0(paste(market_par,platform,sep='_'),'.csv'))





