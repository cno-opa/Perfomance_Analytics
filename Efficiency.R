### This script analyzes efficiency measures, as expressed by 
### performance as a function of budgeted expenditures from 2011-Present

### Load packages
library(dplyr)
library(gdata)
library(ggplot2)
library(reshape2)

### Function for reading R files directly from github.com
source_https <- function(u, unlink.tmp.certs = FALSE) {
  require(RCurl)
  
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  eval(parse(text = script), envir= .GlobalEnv)
}

source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/lm_equation.R") ##### Load script for displaying Linear regression equation and R-squared on lm plots
source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/Multiplot%20function.R")
source_https("https://raw.githubusercontent.com/cno-opa/Performance_Analytics/master/Read_clean.R")


#### Calculate the average performance status for each department
for(i in 1:length(unique(df$Org))){
  
  df$Composite_2015[i]<-mean(df$Status_2015[i])
  }

for(i in 1:length(unique(df$Org))){
  
  df$Composite_2014[i]<-mean(df$Status_2014[i])
}

for(i in 1:length(unique(df$Org))){
  
  df$Composite_2013[i]<-mean(df$Status_2013[i])
}

for(i in 1:length(unique(df$Org))){
  
  df$Composite_2012[i]<-mean(df$Status_2012[i])
}

for(i in 1:length(unique(df$Org))){
  
  df$Composite_2011[i]<-mean(df$Status_2011[i])
}

#### Calculate budget growth
df$Growth12<-100*(df$FY2012-df$FY2011)/df$FY2011
df$Growth13<-100*(df$FY2013-df$FY2012)/df$FY2012
df$Growth14<-100*(df$FY2014-df$FY2013)/df$FY2013
df$Growth15<-100*(df$FY2015-df$FY2014)/df$FY2014

#### Calculate overall performance improvement
df$Net_perf12<-df$Composite_2012-df$Composite_2011
df$Net_perf13<-df$Composite_2013-df$Composite_2012
df$Net_perf14<-df$Composite_2014-df$Composite_2013
df$Net_perf15<-df$Composite_2015-df$Composite_2014


### Efficiency Measure Calculations

#### Calculate "expected efficiency," as represented by target divided by $100,000 budget
df$Expected_2011<-round(df$Target_2011/(df$FY2011/100000),2)
df$Expected_2012<-round(df$Target_2012/(df$FY2012/100000),2)
df$Expected_2013<-round(df$Target_2013/(df$FY2013/100000),2)
df$Expected_2014<-round(df$Target_2014/(df$FY2014/100000),2)
df$Expected_2015<-round(df$Target_2015/(df$FY2015/100000),2)

#### Calculate "efficiency," as represented by actual divided by $100,000 budget
df$Efficiency_2011<-round(df$Actual_2011/(df$FY2011/100000),2)
df$Efficiency_2012<-round(df$Actual_2012/(df$FY2012/100000),2)
df$Efficiency_2013<-round(df$Actual_2013/(df$FY2013/100000),2)
df$Efficiency_2014<-round(df$Actual_2014/(df$FY2014/100000),2)
df$Efficiency_2015<-round(df$YTDActual_2015/(df$FY2015/100000),2)

#### Subset for DPW (will be scaled to all orgs later)
DPW_Ef<-df[df$Type=="Count" & df$Org=="Public Works",]
DPW_Ef<-cbind(melt(select(DPW_Ef,Measure,Efficiency_2011:Efficiency_2015),
             id.vars=c("Measure"),
             measure.vars=c("Efficiency_2011","Efficiency_2012","Efficiency_2013","Efficiency_2014","Efficiency_2015"),
             variable.name="Year",
             value.name="Efficiency"),
         select(melt(select(DPW_Ef,Measure,Expected_2011:Expected_2015),
                     id.vars=c("Measure"),
                     measure.vars=c("Expected_2011","Expected_2012","Expected_2013","Expected_2014","Expected_2015"),
                     variable.name="Year",
                     value.name="Expected"),Expected))
             

#### Code a new year variable, "Year2" based on efficiency
for (i in 1:nrow(DPW_Ef)){
  
  if(grepl("11",DPW_Ef$Year[i])){
    
    DPW_Ef$Year2[i]<-"2011"
    
  } else if(grepl("12",DPW_Ef$Year[i])){
    
    DPW_Ef$Year2[i]<-"2012"
    
  } else if(grepl("13",DPW_Ef$Year[i])){
    
    DPW_Ef$Year2[i]<-"2013"
    
  } else if(grepl("14",DPW_Ef$Year[i])){
    
    DPW_Ef$Year2[i]<-"2014"
    
  }  else {
    
    DPW_Ef$Year2[i]<-"2015"
  }
}

#### Filter out NA's and 0's from performance column, and remove original "Year" column
DPW_Ex<-filter(DPW_Ef,!is.na(Expected))%>%
  select(-Year)
DPW_Ex<-DPW_Ex[DPW_Ex$Efficiency>0,]

DPW_Ef<-filter(DPW_Ef,!is.na(Efficiency)| Efficiency!=0)%>%
  select(-Year)
DPW_Ef<-DPW_Ef[DPW_Ef$Efficiency>0,]

#### Create plots for expected and actual efficiency
DPW_Explot<-ggplot(DPW_Ex,aes(x=Year2,y=Expected))+
  geom_bar(stat="identity",size=0.6)+
  facet_grid(facets=.~Measure)+
  ggtitle("DPW Expected Efficiency 2011-2015")+
  xlab("Year")+ylab("Performance/$100K")+
  theme(strip.text.x=element_text(size=8),
        axis.text.x=element_blank(),plot.title=element_text(size=13,face="bold",vjust=1))+
  geom_text(aes(y=Expected,ymax=Expected+1,label=round(Expected,0)),position=position_dodge(width=0.9),vjust=-.5,size=3.5)
print(DPW_Explot)

DPW_Efplot<-ggplot(DPW_Ef,aes(x=Year2,y=Efficiency))+
  geom_bar(stat="identity",size=0.6)+
  facet_grid(facets=.~Measure)+
  ggtitle("DPW Actual Efficiency 2011-2015")+
  xlab("Year")+ylab("Performance/$100K")+
  theme(strip.text.x=element_text(size=8),
        axis.text.x=element_blank(),plot.title=element_text(size=13,face="bold",vjust=1))+
    geom_text(aes(y=Efficiency,ymax=Efficiency+1,label=round(Efficiency,0)),position=position_dodge(width=0.9),vjust=-.5,size=3.5)
print(DPW_Efplot)

multiplot(DPW_Explot,DPW_Efplot)


### Performance Regressions

#### Run composite performance regression models with budgeted dollars as the independent variable
Performance_Netlm<-cbind(melt(select(df,Org,Index,Measure,Net_perf12:Net_perf14),
                          id.vars=c("Org","Index","Measure"),
                          measure.vars=c("Net_perf12","Net_perf13","Net_perf14"),
                          variable.name="Year",
                          value.name="Net_Performance"),
                     select(melt(select(df,Org,Index,Measure,Growth12:Growth14),
                          id.vars=c("Org","Index","Measure"),
                          measure.vars=c("Growth12","Growth13","Growth14"),
                          variable.name="Year",
                          value.name="Growth"),Growth))

Performance_lm<-cbind(melt(select(df,Org,Index,Measure,Status_2015,Status_2012:Status_2014),
                           id.vars=c("Org","Index","Measure"),
                           measure.vars=c("Status_2012","Status_2013","Status_2014"),
                           variable.name="Year",
                           value.name="Status"),
                      select(melt(select(df,Org,Index,Measure,Growth12:Growth14),
                                  id.vars=c("Org","Index","Measure"),
                                  measure.vars=c("Growth12","Growth13","Growth14"),
                                  variable.name="Year",
                                  value.name="Growth"),Growth))

#### Code a new year variable, "Year2" based on status and budget years for the two regression datasets
for (i in 1:nrow(Performance_Netlm)){
  
     if(grepl("11",Performance_Netlm$Year[i])){
       
        Performance_Netlm$Year2[i]<-"2011"
        
     } else if(grepl("12",Performance_Netlm$Year[i])){
       
        Performance_Netlm$Year2[i]<-"2012"
        
       } else if(grepl("13",Performance_Netlm$Year[i])){
         
           Performance_Netlm$Year2[i]<-"2013"
           
       } else if(grepl("14",Performance_Netlm$Year[i])){
         
           Performance_Netlm$Year2[i]<-"2014"
           
        }  else {
          
          Performance_Netlm$Year2[i]<-"2015"
            }
}


for (i in 1:nrow(Performance_lm)){
  
  if(grepl("11",Performance_lm$Year[i])){
    
    Performance_lm$Year2[i]<-"2011"
    
  } else if(grepl("12",Performance_lm$Year[i])){
    
    Performance_lm$Year2[i]<-"2012"
    
  } else if(grepl("13",Performance_lm$Year[i])){
    
    Performance_lm$Year2[i]<-"2013"
    
  } else if(grepl("14",Performance_lm$Year[i])){
    
    Performance_lm$Year2[i]<-"2014"
    
  }  else {
    
    Performance_lm$Year2[i]<-"2015"
  }
}

#### Filter out NA's from performance column, and remove original "Year" column
Performance_lm<-filter(Performance_lm,!is.na(Status))%>%
                         filter(Status<20)%>%
                              filter(!is.na(Growth))%>%
                              select(-Year)
Performance_Netlm<-filter(Performance_Netlm,!is.na(Net_Performance))%>%
  filter(Net_Performance<20)%>%
      filter(!is.na(Growth))%>%
        select(-Year)

#### Run regression model on net performance by budget growth 
Perf_Netlm<-ggplot(Performance_Netlm,aes(x=Growth,y=Net_Performance))+
  geom_point(shape=1)+
 geom_smooth(method="lm",se=TRUE)+
  ggtitle("Regression of the effect of budget growth on overall KPI performance")+
  geom_text(x=0,y=1,label=lm_eqn(lm(Net_Performance~Growth,Performance_Netlm)),parse=TRUE)+ ### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  labs(x="Growth",y="Net Performance")
print(Perf_Netlm)

#### Run regression model on net performance by budget growth 
Perf_lm<-ggplot(Performance_lm,aes(x=Growth,y=Status))+
  geom_point(shape=1)+
  geom_smooth(method="lm",se=TRUE)+
  ggtitle("Regression of the effect of budget growth on overall KPI performance")+
  geom_text(x=25,y=2.5,label=lm_eqn(lm(Status~Growth,Performance_lm)),parse=TRUE)+ ### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  labs(x="Growth",y="Status")
print(Perf_lm)

#### Generate regression plots for all departments
 for (i in levels(as.factor(Performance_lm$Org))){
   
   df<-Performance_lm[Performance_lm$Org==levels(Performance_lm$Org)[i],]
   
  print(ggplot(df,aes(x=df$Growth,y=df$Status))+
          geom_point(shape=1)+
          geom_smooth(method="lm",se=TRUE)+
          ggtitle("Regression of the effect of budget growth on overall KPI performance")+
          # geom_text(x=-5,y=2,label=lm_eqn(lm(Status~Growth,Performance_lm[i])),parse=TRUE)+ ### lm_eqn is a custom function that adds regression equation and Rsquared to plot
          labs(x="Growth",y="Status"))
 }



DPW_lm<-ggplot(Performance_lm[Performance_lm$Org=="Public Works",],aes(x=Growth,y=Status))+
  geom_point(shape=1)+
  geom_smooth(method="lm",se=TRUE)+
  ggtitle("Regression of the effect of budget growth on overall KPI performance - DPW")+
  geom_text(x=-5,y=2,label=lm_eqn(lm(Status~Growth,Performance_Netlm[Performance_Netlm$Org=="Public Works",])),parse=TRUE)+ ##### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  labs(x="Growth",y="Status")
print(DPW_lm)

DPW_Netlm<-ggplot(Performance_Netlm[Performance_Netlm$Org=="Public Works",],aes(x=Growth,y=Net_Performance))+
  geom_point(shape=1)+
  geom_smooth(method="lm",se=TRUE)+
  ggtitle("Regression of the effect of budget growth on overall KPI performance")+
  geom_text(x=-3,y=0.5,label=lm_eqn(lm(Net_Performance~Growth,Performance_Netlm[Performance_Netlm$Org=="Public Works",])),parse=TRUE)+ ##### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  labs(x="Growth",y="Net Performance")
print(DPW_Netlm)
