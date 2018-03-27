#Extract block and ZCTA level variables from ACS using tidy census


#libraries
#install.packages("tidycensus")
#install.packages("tidyverse")
#install.packages("reshape")
#install.packages("gdata")
#install.packages("readxl")
library(tidycensus)
library(tidyverse)
library(reshape)
library(gdata)
library(readxl)

#see the tables to download.
 
v16 <- load_variables(2016, "acs1", cache = TRUE)
#v16$year <- 2016
##it looks like this function doesn't work before 2012 (maybe because they care most about 5yr data?), but I know for this table the variables don't change
#for (y in 2015:2008){
#  print(y)
#  x<-y
#  if(y<2012){x<-2012}
#  v <- load_variables(x, "acs1", cache = TRUE)
#  v$year <- x
#  v16 <- rbind(v16,v)
#}
#View(v16) 


#we only need B05003 for adult citizens


census_api_key("3d94584bfd87fb43977be01dbddfc797af127c5c") 
options(tigris_use_cache = FALSE) # optional - to cache the Census shapefile



B05003 <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                                table="B05003", #get all vars in tables
                                                year = 2016, #this year
                                                state = "NC", 
                                                county = "Forsyth",
                                                geometry = F, # no mapping geometry
                                                survey = "acs1",
                                                cache_table=T), error=function(e) NA))
B05003$year="2016"
B05003$demographic=""


#race/eth files
for (race_eth in c("B","H","I")){
  #
  tmp <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                        table=paste("B05003",race_eth,sep=""), #get all vars in tables
                                        year = 2016, #this year
                                        state = "NC", 
                                        county = "Forsyth",
                                        geometry = F, # no mapping geometry
                                        survey = "acs1",
                                        cache_table=T), error=function(e) NA))
  tmp$year="2016"
  if(race_eth=="B"){tmp$demographic="Black"}
  if(race_eth=="H"){tmp$demographic="Hispanic"}
  if(race_eth=="I"){tmp$demographic="Whitenh"}
  
  B05003 <- rbind(B05003,tmp)
}

#Forsyth over time
for (year in c("2016","2015","2014","2013","2012")){
  for (county in c("Forsyth")){
    
    tmp <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                          table="B05003", #get all vars in tables
                                          year = year, #this year
                                          state = "NC", 
                                          county = county,
                                          geometry = F, # no mapping geometry
                                          survey = "acs1",
                                          cache_table=T), error=function(e) NA))
    tmp$year=year
    tmp$demographic=""
    B05003 <- rbind(B05003,tmp)
  }
}

#counties over time
for (year in c("2016","2015","2014","2013","2012")){
  for (county in c("Cumberland","Guilford","Gaston")){
    
    tmp <- as.data.frame(tryCatch(get_acs(geography = "county", 
                                            table="B05003", #get all vars in tables
                                            year = year, #this year
                                            state = "NC", 
                                            county = county,
                                            geometry = F, # no mapping geometry
                                            survey = "acs1",
                                            cache_table=T), error=function(e) NA))
    tmp$year=year
    tmp$demographic=""
    B05003 <- rbind(B05003,tmp)
  }
}

#age has to come from PUMS; the only age breakdowns by citizenship in AFF are over/under 18





#manually add data from 2010 and 2008, since that is not available in the api...
path_data <- "C:/Users/staff/Downloads/ncvoter/old election data.xlsx"

old_data <- read_excel(path_data, sheet = "old election data")


B05003 <- rbind(B05003,old_data)



##add variable names
#B05003$name_for_merging <- paste(B05003$variable,"E",sep="")
B05003$name <- paste(B05003$variable,"E",sep="")



B05003m <- merge(B05003,v16, by="name",all.x=TRUE)




#remove "Estimate" from the variable names
B05003m$label <- gsub("Estimate!!", "", B05003m$label)

#change labels to reflect variable tables

#index <- B05003m$concept == "SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (BLACK OR AFRICAN AMERICAN ALONE)"
#B05003m$label[index] <- paste(B05003m$label[index],": African American")

#index <- B05003m$concept == "SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (HISPANIC OR LATINO)"
#B05003m$label[index] <- paste(B05003m$label[index],": Hispanic")

#index <- B05003m$concept == "SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (WHITE ALONE, NOT HISPANIC OR LATINO)"
#B05003m$label[index] <- paste(B05003m$label[index],": White, nonHispanic")


#rename the geography column
names(B05003m)[names(B05003m) == 'NAME'] <- 'Location'



#calculate SE
B05003m$SE <- B05003m$moe/1.645
#replace missing SEs with 0; this means that the error is statistically controlled and should be treated as 0
B05003m$SE[is.na(B05003m$SE)] <- 0

B05003m$demographic[is.na(B05003m$demographic)] <- ""


#get just the variables we care about
varlist=c("Total!!Male!!18 years and over!!Foreign born!!Not a U.S. citizen",
          "Total!!Female!!18 years and over!!Foreign born!!Not a U.S. citizen",
          "Total!!Male!!18 years and over",
          "Total!!Female!!18 years and over")

B05003m <- B05003m[B05003m$label %in% varlist,c("Location","year","label","concept","demographic","estimate","SE")]


#currently each data point has one row
#transpose the data so that each county/year combination has one row, to allow for easier calculations

B05003md <- melt(B05003m,id=c("Location","year","label","concept","demographic"))

#this reshapes, but requires a function (in case there are multiple records with the same location/year/concept).
#THIS WILL NOT WORK CORRECTLY IF THERE ARE MULTIPLE ROWS PER LOCATION/YEAR/CONCEPT
#we can use B05003mdc <- cast(B05003md,Location+year+concept~label + variable) (not specifying the aggregation function which defaults to count) to verify that there is only one combination of each.
#but ideally we would use a reshaping function that throws an error if the grouping variables do not uniquely identify rows.

B05003mdc <- cast(B05003md,Location+year+concept+demographic~label + variable,mean)


#subtraction function takes two variables, subtracts them, and recalculates SE
acs_subtract <- function(df,var1,var2,varname){
  
  values <- df[[paste(var1,"_estimate",sep="")]]-df[[paste(var2,"_estimate",sep="")]]
  SEs <- sqrt(df[[paste(var1,"_SE",sep="")]]^2+df[[paste(var2,"_SE",sep="")]]^2)
  
  df[[paste(varname,"_estimate",sep="")]] <- values
  df[[paste(varname,"_SE",sep="")]] <- SEs
  return(df)
}

acs_add <- function(df,var1,var2,varname){
  
  values <- df[[paste(var1,"_estimate",sep="")]]+df[[paste(var2,"_estimate",sep="")]]
  SEs <- sqrt(df[[paste(var1,"_SE",sep="")]]^2+df[[paste(var2,"_SE",sep="")]]^2)
  
  df[[paste(varname,"_estimate",sep="")]] <- values
  df[[paste(varname,"_SE",sep="")]] <- SEs
  return(df)
}




B05003mdc <- acs_subtract(B05003mdc,"Total!!Female!!18 years and over","Total!!Female!!18 years and over!!Foreign born!!Not a U.S. citizen","Female adult citizens")

B05003mdc <- acs_subtract(B05003mdc,"Total!!Male!!18 years and over","Total!!Male!!18 years and over!!Foreign born!!Not a U.S. citizen","Male adult citizens")

B05003mdc <- acs_add(B05003mdc,"Female adult citizens","Male adult citizens","adult citizens")


zscore_test <- function(val1,SE1,val2,SE2){
  z <- abs(val1-val2)/sqrt(SE1^2+SE2^2)
  return(z)
}








acs_zscore_test <- function(df,year1,Location1,demographic1,
                            year2,Location2,demographic2){
  #demographic<-""
  #year<-2016
  #Location<-"Forsyth County, North Carolina"
  #var1<-"adult citizens"
  
  
  var1_estimate <-
    df[(df$Location==Location1)&
              (df$year==year1)&
              (df$demographic==demographic1),
                paste(var1,"_estimate",sep="")]  
  
  var1_SE <-
    df[(df$Location==Location1)&
         (df$year==year1)&
         (df$demographic==demographic1),
       paste(var1,"_SE",sep="")]  

  var2_estimate <-
    df[(df$Location==Location2)&
         (df$year==year2)&
         (df$demographic==demographic2),
       paste(var1,"_estimate",sep="")]  
  
  var2_SE <-
    df[(df$Location==Location2)&
         (df$year==year2)&
         (df$demographic==demographic2),
       paste(var2,"_SE",sep="")]  
  
  
  
  z <- abs(var1_estimate-var2_estimate)/sqrt(var1_SE^2+var2_SE^2)
  
  if(z<=1.96){comparison<-"not significantly significant compared to"}
  else if(z>0){comparison<-"greater than"}
  else if(z<0){comparison<-"less than"}
  else {comparison<-"error"}

  return(z)
}
















