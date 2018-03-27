#this allows for sql queries
#install.packages("sqldf")
#install.packages("reldist")
#install.packages("Hmisc")
library(reldist)
library(sqldf)
library(Hmisc)
#ask R not to use scientific notation for serial numbers
options("scipen"=100, "digits"=4)

#import raw PUMS data
#C:/Users/Zantan/Desktop/FF/PUMS/Raw Data/
hnc <- read.table("C:/Users/Zantan/Downloads/pums/ss16hnc.csv", 
                  header = TRUE,
                  sep = ",")

pnc <- read.table("C:/Users/Zantan/Downloads/pums/ss16pnc.csv", 
                  header = TRUE,
                  sep = ",")


#only Forsyth county
hnc_forsyth <- subset(hnc, PUMA == 1801 | PUMA == 1802 | PUMA == 1803)
pnc_forsyth <- subset(pnc, PUMA == 1801 | PUMA == 1802 | PUMA == 1803)

#check populations
sum(pnc_forsyth$PWGTP)
sum(hnc_forsyth$WGTP)




#we are only using person file for demographic information
pncusevars <- c("SERIALNO", "RELP", "RAC1P" , "AGEP" , "HISP")
pncuse <- pnc_forsyth[pncusevars]

#create demographic variables
pncuse$Black <- ifelse(pncuse$RAC1P == 2, 1, 0)
pncuse$WhiteNH <- ifelse(pncuse$RAC1P == 1 & pncuse$HISP == 1 , 1, 0)
pncuse$Hispanic <- ifelse(pncuse$HISP == 1, 0, 1)
pncuse$Agecat <- ifelse(pncuse$AGEP < 18, "< 18",ifelse(pncuse$AGEP < 45, "18 - 44", "45 and over"))

pncuse$Agecat2[pncuse$AGEP < 18] <- "Under 18"
pncuse$Agecat2[pncuse$AGEP >= 18 & pncuse$AGEP <= 24] <- "18 - 24"
pncuse$Agecat2[pncuse$AGEP >= 25 & pncuse$AGEP <= 44] <- "25 - 44"
pncuse$Agecat2[pncuse$AGEP >= 45 & pncuse$AGEP <= 64] <- "45 - 64"
pncuse$Agecat2[pncuse$AGEP >= 65] <- "65 and older"



#we are only using HINCP and weights from the household file (also TYPE which identifies group quarters)
hncusevars <- c(c("SERIALNO", "TYPE", "HINCP" , "NP", "WGTP"),paste("WGTP",as.character(1:80),sep=""))
hncuse <- hnc_forsyth[hncusevars]

#assign quintiles
Q1=19475
Q2=36061
Q3=58816
Q4=96102

hncuse$Q1 <- ifelse(hncuse$HINCP < Q1, 1,0)
hncuse$Q2 <- ifelse(hncuse$HINCP >= Q1 & hncuse$HINCP < Q2, 1,0)
hncuse$Q3 <- ifelse(hncuse$HINCP >= Q2 & hncuse$HINCP < Q3, 1,0)
hncuse$Q4 <- ifelse(hncuse$HINCP >= Q3 & hncuse$HINCP < Q4, 1,0)
hncuse$Q5 <- ifelse(hncuse$HINCP >= Q4, 1,0)



#merge person and household files
ph <- merge(pncuse,hncuse,by="SERIALNO",all.x=TRUE)

#pnc with only householder characteristicswe only care about the householder characteristics here
#TYPE=1 excludes group quarters
phf <- subset(ph, RELP == 0 & TYPE==1)

#calculate the percent of a group that has certain characteristics
replicate_proportion <- function(test_name,df,numerator_var,numerator_value,denominator_var="TYPE",denominator_value=1,type='p'){

  #test_name="overall"
  #df <- phf
  #numerator_var <- "Q1"
  #numerator_value <- 1
  #denominator_var <-"TYPE"
  #denominator_value <- 1
  #type <- 'h'
  
  if (type=='p'){weight_type <- "PWGTP"
  } else {weight_type <- "WGTP"}
  #1540
  df <-  df[df[[denominator_var]]==denominator_value,]
  dfn <- df[df[[numerator_var]]==numerator_value,]

  
  #get population counts
  denominator_population <- sum(df[[weight_type]])
  numerator_population <- sum(dfn[[weight_type]])
  proportion <- numerator_population/denominator_population
  
  print(c(test_name,denominator_population,numerator_population,proportion))
  
  #calculate the square of the difference between observed and reweighted values
  #this formula can be found in https://usa.ipums.org/usa/repwt.shtml among other places
  sum_of_squared_diff <-0
  #this makes WGTP/PWGTP 1-80
  weightlist <- paste(weight_type,as.character(1:80),sep="")
  for (weightnum in weightlist){
    denominator_population_tmp <- sum(df[[weightnum]])
    numerator_population_tmp <- sum(dfn[[weightnum]])    
    proportion_tmp <- numerator_population_tmp/denominator_population_tmp
    diff <- proportion-proportion_tmp
    sqdiff <- diff^2
    #add the squared diff to a running total
    sum_of_squared_diff <- sum_of_squared_diff+sqdiff
    
  }
  SE <- sqrt(sum_of_squared_diff/20)
  CV=100*SE/proportion
  
  return(c(test_name,proportion,SE,CV,numerator_population,denominator_population))
}

#this series names the variables and is a base to add results to
results=as.data.frame(c("Order","Title","Proportion",'SE',"CV","Numerator","Denominator"))

#list the paramaters for all the calculations
titles <- c('All households','Black','Hispanic','WhiteNH','18-44','45 and older',"Under 18","18 - 24","25 - 44","45 - 64","65 and older")
denomvars <- c('TYPE','Black','Hispanic','WhiteNH','Agecat','Agecat','Agecat2','Agecat2','Agecat2','Agecat2','Agecat2')
denomvals <- c(1,1,1,1,'18 - 44','45 and over',"Under 18","18 - 24","25 - 44","45 - 64","65 and older")


#iterate over the paramaters
for (i in 1:11){
  title=titles[i]
  dvar=denomvars[i]
  dval=denomvals[i]
  
  for (quartile in c('Q1','Q2',"Q3",'Q4','Q5')){
    tmp <- c(i,replicate_proportion(paste(title," ",quartile),phf,quartile,1,type='h',denominator_var=dvar,denominator_value=dval))  
    results[[tmp[2]]] <- tmp
  }
  
    
}


#export
write.csv(t(results),'income quintiles.csv')



#aera