setwd("/home/zach/Documents/CFE_empirical_workshop/basic_R/")

#Loading data
incident_data=read.csv(file= "/home/zach/Documents/CFE_empirical_workshop/basic_R/incidents_part1_part2.csv")
aggregate_state_data=read.csv(file= "/home/zach/Documents/CFE_empirical_workshop/basic_R/data.csv")

    #A quick Stata digression
library(haven)
stata_data=read_dta(file= "/home/zach/Documents/CFE_empirical_workshop/basic_R/AbramsDeterrence0.dta")
write.csv(stata_data, file="stata_data.csv")
      
#Looking at panel
  #identify unit and time dimension of the panel, as well as variables of interest
View(aggregate_state_data)

unique(aggregate_state_data$year) #List of number of year
length(unique(aggregate_state_data$year)) #How many years?

length(unique(aggregate_state_data$FSTATE)) #How many states?

#So, panel seems balanced. But, there are many missing values

#Restructuring our panel
colnames(aggregate_state_data)
colnames(aggregate_state_data)[3]="state"
      #From the original data's readme
          #yaddon (138) is treatment
          #pcrrobtgun (61) is an outcome variable (robbery totals)
          #state is (3) and year is (2)
aggregate_state_data=aggregate_state_data[,c(2,3,138,61)]
View(aggregate_state_data)
  #Check for balance in new panel
length(which(complete.cases(aggregate_state_data[,4])==TRUE)) #Unbalanced!
length(which(is.na(aggregate_state_data[,4])==TRUE)) #Unbalanced!
  #Balancing panel
      #Drop a some years to balance or drop states?
aggregate_state_data_balance_year=matrix(nrow=length(unique(aggregate_state_data[,1])), ncol=2)
aggregate_state_data_balance_year[,1]=unique(aggregate_state_data[,1])
for (i in 1:nrow(aggregate_state_data_balance_year)){
     aggregate_state_data_balance_year[i,2]=length(unique(aggregate_state_data[which((
       aggregate_state_data[,1]==aggregate_state_data_balance_year[i,1]) &
         is.na(aggregate_state_data[,4])==FALSE),2]))
     }
View(aggregate_state_data_balance_year) 
table(aggregate_state_data_balance_year[,2])

aggregate_state_data_balance_state=matrix(nrow=length(unique(aggregate_state_data[,2])), ncol=2)
aggregate_state_data_balance_state[,1]=unique(aggregate_state_data[,2])
for (i in 1:nrow(aggregate_state_data_balance_state)){
  aggregate_state_data_balance_state[i,2]=length(unique(aggregate_state_data[which((
    aggregate_state_data[,2]==aggregate_state_data_balance_state[i,1]) &
      is.na(aggregate_state_data[,4])==FALSE),1]))
}
View(aggregate_state_data_balance_state) 

table(aggregate_state_data_balance_state[,2]) 
    #45 States appear in every year. Let's balance based on state
aggregate_state_data_balance_state=aggregate_state_data_balance_state[which(
  aggregate_state_data_balance_state[,2]==38),] #Subset this to only states appearing in all 38 years
aggregate_state_data_balanced=aggregate_state_data[which((aggregate_state_data[,2] %in%
                                        aggregate_state_data_balance_state[,1])==TRUE),]
View(aggregate_state_data_balanced)

#Summary stats
mean(aggregate_state_data_balanced[,4]) #total mean
mean(aggregate_state_data_balanced[which(aggregate_state_data_balanced[,3]==1),4]) #treated only
mean(aggregate_state_data_balanced[which(aggregate_state_data_balanced[,3]==0),4]) #untreated only
      #Untreated mean is greater than treated mean
sd(aggregate_state_data_balanced[,4]) #total mean
sd(aggregate_state_data_balanced[which(aggregate_state_data_balanced[,3]==1),4]) #treated only
sd(aggregate_state_data_balanced[which(aggregate_state_data_balanced[,3]==0),4]) #untreated only
      #Untreated sd is greater than treated mean
#basic plots
treated_counts=matrix(nrow=length(unique(aggregate_state_data_balanced[,1])), ncol=2)
treated_counts[,1]=as.numeric(unique(aggregate_state_data_balanced[,1]))
for (i in 1:nrow(treated_counts)){
  treated_counts[i,2]=sum(aggregate_state_data_balanced[which(
    aggregate_state_data_balanced[,1]==treated_counts[i,1] ),3])
}
plot(treated_counts[,1], treated_counts[,2])
plot(treated_counts[,1], (treated_counts[,2]/length(unique(aggregate_state_data_balanced[,2]))))

#Merging some other data to our balanced panel
aggregate_state_data=read.csv(file= "/home/zach/Documents/CFE_empirical_workshop/basic_R/data.csv")
colnames(aggregate_state_data)
colnames(aggregate_state_data)[3]="state"
aggregate_state_data=aggregate_state_data[,c(2,3,5)]
aggregate_state_data=aggregate_state_data[which(aggregate_state_data[,2] %in% 
                                                  aggregate_state_data_balanced[,2]),]
length(unique(aggregate_state_data[,2])) # Now we're looking at the same 45 states

aggregate_state_data_balanced=merge(aggregate_state_data_balanced, aggregate_state_data,
                                    by=c("state", "year"))
ncol(aggregate_state_data_balanced) #Note the additional column has been added
aggregate_state_data_balanced[,5]=NULL
ncol(aggregate_state_data_balanced) #That new column has been dropped

#regression
summary(lm(aggregate_state_data_balanced$pcrrobgun~aggregate_state_data_balanced$yaddon))
   #Or a bit cleaner....
attach(aggregate_state_data_balanced)
summary(lm(pcrrobgun~yaddon))
   #Diff in Diff?
summary(lm(pcrrobgun~yaddon+factor(state)+factor(year)))
  #Ugly results, but the fixed effects lead to a more significant result
    #lets try with the fixest Package (also allows us to cluster SE)
library(fixest)
feols(pcrrobgun~yaddon|year+state, data=aggregate_state_data_balanced)
feols(pcrrobgun~yaddon|year+state, cluster="state", data=aggregate_state_data_balanced)

#incident level
colnames(incident_data)
    #Note, the spatial component. We'll ignore that for today, though.
    #Limit data to useful columns--district,date,incident type, and location
incident_data=incident_data[,c(2,5,9,11)]
    #Defining a treatment- July 28, mass shooting in 12th District 
incident_data$day=as.numeric(substr(incident_data$dispatch_date, 9, 10))
incident_data$month=as.numeric(substr(incident_data$dispatch_date,6, 7))
    #Subset to summer months
attach(incident_data)
incident_data=subset(incident_data, month==06 | month==07 |  month==08)
unique(incident_data$month)
    #Subsetting to Southwest Philadelphia districts to speed up computation
incident_data=subset(incident_data, dc_dist==12|dc_dist==16|dc_dist==18|dc_dist==19)
    #Creating a shooting indicator
unique(text_general_code)
attach(incident_data)
incident_data$shooting=ifelse(text_general_code=="Homicide - Criminal "|
                       text_general_code=="Homicide - Criminal" | 
                       text_general_code=="Aggravated Assault Firearm", 1, 0)
    #Aggregate to the block level to have a panel
block_panel=matrix(nrow = (length(unique(location_block))*length(unique(dispatch_date))),
                   ncol=6)
block_panel[,1]=rep(unique(location_block), each=length(unique(dispatch_date)))
block_panel[,2]=rep(unique(dispatch_date), by=length(unique(location_block)))
View(block_panel) #Note the panel's structure
block_panel=as.data.frame(block_panel)
colnames(block_panel)[1]="block"
colnames(block_panel)[2]="date"
colnames(block_panel)[3]="day"
colnames(block_panel)[4]="month"
colnames(block_panel)[5]="district"
colnames(block_panel)[6]="shooting_count"

block_panel$day=as.numeric(substr(block_panel$date, 9, 10))
block_panel$month=as.numeric(substr(block_panel$date,6, 7))
    #Aggregating our shooting counts
timeNow <- Sys.time()
for (i in 1:nrow(block_panel)){
  block_panel[i,6]=sum(incident_data[which(incident_data[,3]==block_panel[i,1] &
                                             incident_data[,2]==block_panel[i,2]),6])
    temp=incident_data[which(incident_data[,3]==block_panel[i,1]),]
    block_panel[i,5]=temp[1,1]
  cat("\r", round(i*100/nrow(block_panel), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
  #Assigning treatment status
block_panel$treated=ifelse((block_panel$date>28 & block_panel$month==7 & 
                             block_panel$district==12) | (block_panel$month==8 &
                              block_panel$district==12),1,0)
block_panel$shooting_count=as.numeric(block_panel$shooting_count)
  #Let's see if the mass shooting had an effect on future shootings in that district!
feols(shooting_count~treated|date+block, cluster="district", data=block_panel)
      #A modest negative and yet significant effect!