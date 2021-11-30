############## LOADING DATA SET ##########################
df <- read.csv("/Users/aedhalwadaeen/Desktop/data mining/Finalproject/kc_house_data.csv",head=TRUE)

################### Removal of unnecessary attributes #############
df1<-df[,-1]
df1<-df1[,-1]
df1<-df1[,-19]
df1<-df1[,-18]
df1<-df1[,-17]
df1<-df1[,-16]
df1<-df1[,-15]
df1<-df1[,-14]
df2<-df1[,-12]

### Removing outliers from the data set using this function by replacing each outlier value in the data set with na and then deleting the entire rows which have outliers #####
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

y <- remove_outliers(df2$price)
df2$price<-y
y <- remove_outliers(df2$sqft_lot)
df2$sqft_lot<-y
y <- remove_outliers(df2$sqft_living)
df2$sqft_living<-y
y <- remove_outliers(df2$sqft_above)
df2$sqft_above<-y
y <- remove_outliers(df2$yr_built)
df2$yr_built<-y

nooutliers<-df2[complete.cases(df2), ]
Tmissingdata=sum(is.na(nooutliers))
Tmissingdata


############   Random forest implementation from scratch ################
######### This function calculates and returns the value at which a continuous attribute should be split ####
split_value<-function(df,attr)
  
{ 
  if (attr==7) return (1);
  if (attr==2|attr==3|attr==6|attr==8|attr==9|attr==10) unq_val<-unique(df[,attr]);
  if (attr==4|attr==5|attr==11|attr==12)
  {
    unq<-unique(df[,attr])
    unq<-sort(unq)
    unq_val<-array(0:0,dim=c(1,11))
    unq_val[1]<-unq[1]
    unq_val[11]<-unq[length(unq)]
    for (p in 1:9) unq_val[1+p]<-unq[1+(p*length(unq)/10)]
  }
  
  rss_val<-array(0:0,dim=c(1,length(unq_val)))
  
  for (i in 1:length(unq_val))
  { 
    df1<-df[1,]
    df1<-df1[-1,]
    df2<-df[1,]
    df2<-df2[-1,]
    
    for(j in 1:nrow(df))
    {
      if (df[j,attr]<unq_val[i])
      {
        k=nrow(df1)
        df1[(k+1),]<-df[j,]
      }
      else
      {
        k=nrow(df2)
        df2[(k+1),]<-df[j,]
      }
    }
    
    rssl<-0
    if(nrow(df1)>0) {for(l in 1:nrow(df1)) rssl=rssl+(df1[l,1]-mean(df1[,1]))^2}
    rssr<-0
    if(nrow(df2)>0) {for(l in 1:nrow(df2)) rssr=rssr+(df2[l,1]-mean(df2[,1]))^2}
    
    rss_val[i]=rssl+rssr;
  }  
  
  for (i in 1:length(unq_val)) if(rss_val[i]==min(rss_val)) break;
  return(unq_val[i]);
}


############# Split Function: Selects three random attributes and returns the best split #########  
split_df<-function(df)
{ 
  attr_list<-sample(2:11,3,replace=F)
  
  values<-array(0:0,dim=c(1,3))
  for (i in 1:3) values[i]=split_value(df,attr_list[i])
  
  rss<-array(0:0,dim=c(1,3))
  dfs<-list()
  for (a in 1:3)
  { 
    dfs1<-df[1,]
    dfs1<-dfs1[-1,]
    dfs2<-df[1,]
    dfs2<-dfs2[-1,]
    for(i in 1:nrow(df))
    {
      if (df[i,attr_list[a]]<values[a])
      {
        j=nrow(dfs1)
        dfs1[(j+1),]<-df[i,]
      }
      else
      {
        j=nrow(dfs2)
        dfs2[(j+1),]<-df[i,]
      }
    }
    dfs[[(2*a)-1]]=dfs1
    dfs[[(2*a)]]=dfs2
    
    rssl<-0
    if(nrow(dfs1)>0) {for(i in 1:nrow(dfs1)) rssl=rssl+(dfs1[i,1]-mean(dfs1[,1]))^2}
    rssr<-0
    if(nrow(dfs2)>0) {for(i in 1:nrow(dfs2)) rssr=rssr+(dfs2[i,1]-mean(dfs2[,1]))^2}
    
    rss[a]=rssl+rssr;
  }  
  
  for (a in 1:3) if(rss[a]==min(rss)) break;
  split<-list(dfs[[(2*a)-1]],dfs[[(2*a)]],attr_list[a],values[a])
  
  return(split)
}



###################### Main program ############  

########### Array to store the split decisions at each node for all the trees #######
decision<-array(0:0,dim=c(20,1022))

rf<-array(list(NULL), dim=c(1,20))
########### No. of Trees is 20 ##############
for (h in 1:20)   
{ 
  data_sample<-sample (1:18123,18123,replace = TRUE)
  tree_data<- nooutliers[data_sample,]
  
  tree_rf<-list()
  tree_rf[[1]]<-tree_data
  
  edf<-tree_data[1,]
  edf<-edf[-1,]
  for (u in 2:1023) tree_rf[[u]]<-edf;       
  

############ Splitting each tree upto level 9 ##############
  for (u in 1:511)  
  {
    ############ Splitting a node only if it has more than 50 instances ########### 
    if (nrow(tree_rf[[u]])>50) 
    {
      t<-split_df(as.data.frame(tree_rf[u]))
      tree_rf[[2*u]]<-t[[1]]
      tree_rf[[(2*u)+1]]<-t[[2]]
      decision[h,(2*u)-1]=t[[3]]
      decision[h,(2*u)]=t[[4]]
    }
  }
  
  rf[[h]]=tree_rf;
}


################## Function to predict price from a single tree #############
predict_price<-function(tree,dec,ipt,n)
{
  if (dec[1,(2*n)-1]==0) return(mean(tree[[n]][,1]));
  
  if (ipt[1,dec[1,(2*n)-1]] < dec[1,(2*n)])
    
  { if(nrow(tree[[2*n]])==0)
    
    return (mean(tree[[n]][,1]))
    else predict_price (tree,dec,ipt,(2*n))
  }
  
  else 
  { if (nrow(tree[[(2*n)+1]])==0)
    return (mean(tree[[n]][,1]))
    else predict_price (tree,dec,ipt,((2*n)+1))
  }
}

################ Prediction of Price from the set of trees for an existing instance ######################
ipt1<-tree_data[9900,]
price=0
for (t in 1:20) {p=predict_price(rf[[t]],decision,ipt1,1);   price=price+p; } 
price=price/20;
price
ipt1


############### Code for GUI of the application ##################
library(shiny)
library(sp)
library(spacetime)
library(knitr)
library(png)
library(leaflet)

c1 <- 0
c2 <- 0
c3 <- 0
c4 <- 0
c5 <- 0
c6 <- 0
c7 <- 0
c8 <- 0
c9 <- 0
c10 <- 0
c11 <- 0

house_price_prediction <- function(input) {
  c1 <- input$bedrooms
  c2 <- input$bathrooms
  c3 <- input$sqft_living
  c4 <- input$sqft_lot
  c5 <- input$floors
  c6 <- input$waterfront
  c7 <- input$view
  c8 <- input$condition
  c9 <- input$grade
  c10 <- input$sqft_above
  c11 <- input$yr_built
  ipt1<-data.frame(0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11) 
  price<-0
  for (t in 1:20) {p=predict_price(rf[[t]],decision,ipt1,1);   price = price+p; }   
  price=price/20  
  return (price)
}

if (interactive()) {
  
  ui <- fluidPage(    
    includeCSS("style.css"),
    
    headerPanel("House Price Prediction"),
    sidebarLayout(       
      sidebarPanel (  
        numericInput("bedrooms", "No. of Bedrooms", ""),
        numericInput("bathrooms", "No. of Bathrooms", ""),
        numericInput("sqft_living", "Living Area (in sqft)", ""),
        numericInput("sqft_lot", "Lot Area (in sqft)", ""),
        numericInput("floors", "No. of Floors", ""),
        numericInput("waterfront", "Waterfront", ""),
        numericInput("view", "View", ""),
        numericInput("condition", "Condition", ""),
        numericInput("grade", "Grade", ""),
        numericInput("sqft_above", "Sqft Above", ""),
        numericInput("yr_built", "Year Built", ""),
        verbatimTextOutput("value")
      ),
      
      mainPanel (
        
      )
    )
  )
  server <- function(input, output) {
    
    output$value <- renderText({
      
      house_price_prediction (input)
      
    })
    
  }
  shinyApp(ui, server)
}
