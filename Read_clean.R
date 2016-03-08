## Read in, merge, and clean KPI and budget data

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
Budgets<-select(read.csv("O:/Projects/Budgeting for Outcomes/Dept Budgets.csv"),Org:FY2015)



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