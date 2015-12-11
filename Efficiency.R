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

### Read needed files
KPIs_current<- select(read.xls("O:/Projects/ResultsNOLA/2015/2015 KPI Matrix MASTER.xlsx", header = TRUE, sheet = "Measures"),
                      Index, Org, Measure=Measure.1, YTDActual_2015=YTD.Actual, Type=Variable.Type., 
                      Target_2015 = X2015.Target, Strategy = X2015.Strategic.Alignment,Status_2015=Status)
KPIs_hist<-select(read.xls("O:/Projects/ResultsNOLA/2015/2015 KPI Matrix MASTER.xlsx", header = TRUE, sheet = "Seasonality-Historic Data"),
                      Index, Actual_2011=X2011.Year.End.Actual, Target_2011=X2011.Year.End.Target,
                      Actual_2011=X2011.Year.End.Actual, Target_2011=X2011.Year.End.Target,
                      Actual_2012=X2012.Year.End.Actual, Target_2012=X2012.Year.End.Target,
                      Actual_2013=X2013.Year.End.Actual, Target_2013=X2013.Year.End.Target,
                      Actual_2014=X2014.Year.End.Actual, Target_2014=X2014.Year.End.Target,
                      Status_2011=X2011.Met.Target,Status_2012=X2012.Met.Target,Status_2013=X2013.Met.Target,Status_2014=X2014.Met.Target)   
Budgets<-read.csv("O:/Projects/Budgeting for Outcomes/Dept Budgets.csv")


### Data Cleaning

#### Merge into one dataset 
df<-merge(KPIs_current,KPIs_hist,by="Index")
df<-merge(df,Budgets,by="Org")

#### Remove qualitative measures
df<-subset(df,Type!="Qualitative")
df<-subset(df,YTDActual_2015!="")

#### Re-code 'Management Statistic' and 'Establishing Baseline' targets to NA for each year
for(i in 1:nrow(df)){
if(grepl("Management Statistic",df$Target_2015[i])){
     df$Target_2015[i]<-"NA"
   } else if(grepl("Establishing Baseline",df$Target_2015[i])){
      df$Target_2015[i]<-"NA"
     } else {
        df$Target_2015[i]<-df$Target_2015[i]
       }
}

for(i in 1:nrow(df)){
  if(grepl("Management Statistic",df$Target_2014[i])){
    df$Target_2014[i]<-"NA"
  } else if(grepl("Establishing Baseline",df$Target_2014[i])){
    df$Target_2014[i]<-"NA"
  } else {
    df$Target_2014[i]<-df$Target_2014[i]
  }
}

for(i in 1:nrow(df)){
  if(grepl("Management Statistic",df$Target_2013[i])){
    df$Target_2013[i]<-"NA"
  } else if(grepl("Establishing Baseline",df$Target_2013[i])){
    df$Target_2013[i]<-"NA"
  } else {
    df$Target_2013[i]<-df$Target_2013[i]
  }
}

for(i in 1:nrow(df)){
  if(grepl("Management Statistic",df$Target_2012[i])){
    df$Target_2012[i]<-"NA"
  } else if(grepl("Establishing Baseline",df$Target_2012[i])){
    df$Target_2012[i]<-"NA"
  } else {
    df$Target_2012[i]<-df$Target_2012[i]
  }
}

for(i in 1:nrow(df)){
  if(grepl("Management Statistic",df$Target_2011[i])){
    df$Target_2011[i]<-"NA"
  } else if(grepl("Establishing Baseline",df$Target_2011[i])){
    df$Target_2011[i]<-"NA"
  } else {
    df$Target_2011[i]<-df$Target_2011[i]
  }
}

#### Remove percentage symbols from relevant columns
df[, 4]<-as.numeric(gsub("\\%", '', df[, 4]))
df[, 6]<-as.numeric(gsub("[[:punct:]]", '', df[, 6]))

#### Remove dashes from relevant columns
df[, 4]<-as.numeric(gsub('\\-', 'NA', df[, 4]))
df[, 6]<-as.numeric(gsub('\\-', 'NA', df[, 6]))
df[, 8]<-as.numeric(gsub('\\-', 'NA', df[, 8]))
df[,9:20]<-lapply(df[,9:20], function(x) as.numeric(gsub("\\-", 'NA',x)))

### Round to two decimal places for historical data
df[,9:16]<-lapply(df[,9:16], function(x) round(x,2))

#### Convert status and budget columns to numeric
df[ , 8] <- round(as.numeric(df[ , 8]))
df[ , 17:20] <- lapply(df[ , 17:20], function(x) round(as.numeric(x),2))
df[ , 21:25] <- lapply(df[ , 21:25], as.numeric)

#### Determine how far into current year data has been collected to normalize current year budget appropriately
q<-read.xls("O:/Projects/ResultsNOLA/2015/2015 KPI Matrix MASTER.xlsx", header = FALSE, sheet = "Sheet3")
q<-as.vector(q[1,2])

#### Calculate appropriate YTD budget dollars for current year
if(q=="0.25"){  
  df$FY2015<-df$FY2015*0.25  
} else if(q=="0.5"){  
  df$FY2015<-df$FY2015*0.5  
} else if(q=="0.75"){  
  df$FY2015<-df$FY2015*0.75
  } else {  
  df$FY2015<-df$FY2015
}

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
  geom_bar(stat="identity",fill=darkBlue,size=0.6)+
  facet_grid(facets=.~Measure)+
  ggtitle("DPW Expected Efficiency 2011-2015")+
  xlab("Year")+ylab("Performance/$100K")+
  theme(strip.text.x=element_text(size=8),
        axis.text.x=element_blank(),plot.title=element_text(size=13,face="bold",vjust=1))+
  geom_text(aes(y=Expected,ymax=Expected+1,label=round(Expected,0)),position=position_dodge(width=0.9),vjust=-.5,size=3.5)
print(DPW_Explot)

DPW_Efplot<-ggplot(DPW_Ef,aes(x=Year2,y=Efficiency))+
  geom_bar(stat="identity",fill=darkBlue,size=0.6)+
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
                              select(-Year)
Performance_Netlm<-filter(Performance_Netlm,!is.na(Net_Performance))%>%
  filter(Net_Performance<20)%>%
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
# for (i in 1:length(levels(Performance_lm$Org))){
# 
#  print(ggplot(Performance_lm[Performance_lm$Org==levels(Performance_lm$Org)[i],],aes(x=Growth,y=Status))+
#          geom_point(shape=1)+
#          geom_smooth(method="lm",se=TRUE)+
#          ggtitle("Regression of the effect of budget growth on overall KPI performance")+
#           geom_text(x=-5,y=2,label=lm_eqn(lm(Status~Growth,Performance_lm[i])),parse=TRUE)+ ### lm_eqn is a custom function that adds regression equation and Rsquared to plot
#          labs(x="Growth",y="Status"))
# }

DPW_lm<-ggplot(Performance_lm[Performance_lm$Org=="Public Works",],aes(x=Growth,y=Status))+
  geom_point(shape=1)+
  geom_smooth(method="lm",se=TRUE)+
  ggtitle("Regression of the effect of budget growth on overall KPI performance - DPW")+
  geom_text(x=-5,y=2,label=lm_eqn(lm(Status~Growth,DPW)),parse=TRUE)+ ##### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  labs(x="Growth",y="Status")
print(DPW_lm)

DPW_Netlm<-ggplot(Performance_Netlm[Performance_Netlm$Org=="Public Works",],aes(x=Growth,y=Net_Performance))+
  geom_point(shape=1)+
  geom_smooth(method="lm",se=TRUE)+
  ggtitle("Regression of the effect of budget growth on overall KPI performance")+
  geom_text(x=-3,y=0.5,label=lm_eqn(lm(Net_Performance~Growth,Performance_Netlm[Performance_Netlm$Org=="Public Works",])),parse=TRUE)+ ##### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  labs(x="Growth",y="Net Performance")
print(DPW_Netlm)
