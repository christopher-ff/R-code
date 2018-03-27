#library useful for string filtering; not actually used here
#install.packages("stringr")

#for sql code
#install.packages("sqldf")
library(stringr)


#for sql
library(sqldf)

#the file you store the raw data in.  NOT THE SAME AS THIS PROJECT OR THE PUMS DATA WILL BE BACKED UP ON GITHUB!
if (Sys.info()['user']=="staff"){data_directory <- "C:/Users/staff/Downloads/ncvoter/"
}else if  (Sys.info()['user']=="Zantan"){data_directory <- "C:/Users/staff/Downloads/ncvoter/"
}else{stop("please enter your data directory to the code")}

#data_directory <- "C:/Users/Zantan/Downloads/ncvoter/"

#county codes are found in the data format page on the FTP
#Forsyth 34
#Guilford 41
#Gaston 36
#Cumberland 26
counties_of_interest <- c("34","41","36","26")
#AJ was told that we should be paying attention to voted_county_id rather than county_id, but I'm pulling them both so I can make some extra checks

#based on unique(vhis_c$election_desc)
elections <- c("11/03/2009 GENERAL","11/03/2009 MUNICIPAL GENERAL","11/03/2015 MUNICIPAL","11/04/2008 GENERAL","11/04/2014 GENERAL","11/05/2013 MUNICIPAL GENERAL","11/06/2012 GENERAL","11/08/2011 MUNICIPAL","11/02/2010 GENERAL","11/08/2016 GENERAL")




county_data <- function(county){
  
  #a simple function that combines and filters the data files for a given county
  #for (county in c("34","41","36","26")){
  
  
  #get the voter history file for the county
  vhis_c <- read.table(paste(data_directory,"ncvhis",county,".txt",sep=""), 
                       header = TRUE,stringsAsFactors=FALSE,
                       sep = "\t")
  
  #filter to only get general or certain larger municipal
  vhis_c_small <- vhis_c[vhis_c$election_desc %in% elections, ]
  
  #only counties of interest
  vhis_c_small <- vhis_c_small[(vhis_c_small$county_id %in% counties_of_interest)|(vhis_c_small$voted_county_id %in% counties_of_interest), ]
  
  #merge to the voter registration file
  #v_registration <- read.table(paste(data_directory,"ncvoter",county,".txt",sep=""), 
  #                     header = TRUE,stringsAsFactors=FALSE,
  #                     sep = "\t")
  
  #the variables to pull from 
  #regvars <- c("voter_reg_num","status_cd","voter_status_desc","reason_cd","voter_status_reason_desc","municipality_abbrv","municipality_desc", "birth_year","birth_age","race_code","gender_code","ethnic_code","state_cd","zip_code","mail_city")   
  #votes_with_reg_data <- merge(vhis_c_small,v_registration[ , regvars], by="voter_reg_num",all.x=TRUE)
  
  #that was a left join, so now let's check the number of rows to make sure that none were gained (can't loose any)
  votes <- nrow(vhis_c_small)
  #votes_after_merge <- nrow(votes_with_reg_data)
  
  #if (votes != votes_after_merge){
  #  stop(paste("Problem with merge in",county,votes,"votes turned into",votes_after_merge,"records after merge"))
  #       }
  print(paste("county",county," imported with",votes,"records"))
  #return(votes_with_reg_data)
  return(vhis_c_small)
}


#voter history
#start with county 00
election_data <- county_data("1")

#get voter history data for the other 99 counties
for (cty in 2:100){
  cty_str <- toString(cty)
  election_data <- rbind(election_data,county_data(cty_str))
}

#now we make a list of unique voter registration numbers that we need to use to extract registration data.
#this is only necessary if we can expect matching records from different counties; this does not appear to be the case.
#for now skip over this code, since it takes a long time to run
#reg_ids <- unique(election_data$voter_reg_num)



##############  TIME CONSUMING REGISTRATION LOOKUP/MERGE; SKIP THIS CODE #############################################################

#now create a registration file based on these registration numbers

#first county 1
tmp <- read.table(paste(data_directory,"ncvoter","1",".txt",sep=""), 
                  header = TRUE,stringsAsFactors=FALSE,
                  sep = "\t")
registration_data <- tmp[tmp$voter_reg_num %in% reg_ids, ]

#now the other 99
for (cty in 2:100){
  cty_str <- toString(cty)
  tmp <- read.table(paste(data_directory,"ncvoter",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp_small <- tmp[tmp$voter_reg_num %in% reg_ids, ]
  registration_data <- rbind(registration_data,tmp_small)
  print(paste(cty_str,toString(nrow(tmp)),toString(nrow(tmp_small))))
}


regvars <- c("voter_reg_num","status_cd","voter_status_desc","reason_cd","voter_status_reason_desc","municipality_abbrv","municipality_desc", "birth_year","birth_age","race_code","gender_code","ethnic_code","state_cd","zip_code","mail_city")   

#merge the registration and voter history files
#I'm breaking this up by election because merging both files together takes forever

#empty list.  There has to be a better way to do this...
all_elections <- vector("list", 10)

election=elections[1]
print(election)
tmp <-  election_data[election_data$election_desc == election]
votes_with_reg_data <- merge(tmp,registration_data[ , regvars], by="voter_reg_num",all.x=TRUE)


for (election_num in 2:10){
  election=elections[election_num]
  print(election)
  tmp <-  election_data[election_data$election_desc == election]
  tmp_with_reg_data <- merge(tmp,registration_data[ , regvars], by="voter_reg_num",all.x=TRUE)
  votes_with_reg_data <- rbind(votes_with_reg_data,tmp_with_reg_data)
}

#this takes a long time to do, so save the output file for later
saveRDS(votes_with_reg_data, file='voter_data.rds')

#make sure no duplicate ids increased the number of rows
votes <- nrow(election_data)
votes_after_merge <- nrow(votes_with_reg_data)

if (votes != votes_after_merge){
  print(paste("Problem with merge in",county,votes,"votes turned into",votes_after_merge,"records after merge"))
}

####################  investigating people with different voted_county_id/county_id that can be ignored ##########################

#there were 5089 records which didn't seem to have matches in the registration files which I will now look at
missing_registration <- sqldf("select * from votes_with_reg_data where ethnic_code is null")

#just the 2016 election; after all, we only really want demographic data for that one
#there are only 70 missing from 2016, which is pretty insignificant
missing_registration_2016 <- missing_registration[missing_registration$election_desc=="11/08/2016 GENERAL",]

#is this all the cases where the voted county does not match the county?
#yes, we still get 5089
voted_county_mismatch <- sqldf("select * from votes_with_reg_data where voted_county_id != county_id")

misreg <- sqldf("select election_desc, count(*) from missing_registration group by election_desc")

misregc <- sqldf("select election_desc,voted_county_desc, count(*) from missing_registration group by election_desc,voted_county_desc")



#9119434 voted in 2008 in the alamance file but voted in guilford.  this number is not in the registration file for alamance, is it anywhere? probably guilford?
#it is nowhere in the registration file, but voted in two elections!
#let's try 9121406, who did onestop instead of in person
#this person voted once

#wait, AJ said that they get reassigned registration numbers when they move between counties; does that mean that their old registration is DELETED?
#less than 1% of votes are invalidated this way, but according to the census 5-7% of Forsyth's population moved from another county/state each year...similar nationally...

#actually 210641 has records in 29 26 and 2, with registrations in 29 and 26.  The registrations in 29 and 26 have completely different names!

#put any registration number here and it will identify any instances of it in any county file
#this takes a long time because
reg=210641 
for (cty in 1:100){
  cty_str <- toString(cty)
  tmp <- read.table(paste(data_directory,"ncvoter",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$voter_reg_num == reg,]
  if (nrow(tmp)>=1){
    print(tmp$voter_reg_num)}
  results <- toString(nrow(tmp))
  print(paste("voter",reg,"has",results,"records in the",cty_str,"registration file."))
  
  tmp <- read.table(paste(data_directory,"ncvhis",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$voter_reg_num == reg,]
  if (nrow(tmp)>=1){
    print(tmp$voter_reg_num)}
  results <- toString(nrow(tmp))
  print(paste("voter",reg,"has",results,"records in the",cty_str,"voter history file."))
}

#what about the ncid? BY480869 (9119434) was in alamance/guilford.  let's get everyone with ncid BY480869 in those counties (1,41) and see what we get.
#test this for AA100704 which is in Guilford 34

ncid="BY480869"
for (cty in 1:100){
  cty_str <- toString(cty)
  tmp <- read.table(paste(data_directory,"ncvoter",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$ncid == ncid,]
  results <- toString(nrow(tmp))
  print(paste("voter",ncid,"has",results,"records in the",cty_str,"registration file."))
  
  
  tmp <- read.table(paste(data_directory,"ncvhis",cty_str,".txt",sep=""), 
                    header = TRUE,stringsAsFactors=FALSE,
                    sep = "\t")
  tmp <- tmp[tmp$ncid == ncid,]
  results <- toString(nrow(tmp))
  print(paste("voter",ncid,"has",results,"records in the",cty_str,"voter history file."))
}

####################  end investigation ##########################






##############  BEGIN ALAYSIS #############################################################





#the saved file
votes_with_reg_data <- readRDS('voter_data.rds')


#let's look at the voter numbers in each election
election_results <- sqldf("select voted_county_desc,election_desc,count(*) 
                          from votes_with_reg_data 
                          where voted_county_id in ('34','41','36','26')
                          group by voted_county_desc,election_desc")
print(election_results)

#it's weird that the ftp files were organized by county_id when voted_county_id is so important.  Let's look at those
voted_county_vs_county <- sqldf("select voted_county_desc,county_desc, count(*) from votes_with_reg_data group by voted_county_desc,county_desc")

print(voted_county_vs_county)


#now we'll get frequencies for each variable of interest
#sql does not ignore null values, so they should show up here
#when each demographic is NA it means that there was no registration data
#we're only doing demographic analysis of Forsyth's 2016 election, so we'll only look at that here

#there are 179,321 votes
#there are 19 voters with NA in all demographics (moved and re-registered in another county since the election?)
#there are 453 people born in 1900 (missing ages)
#one person was born in 1999, their age needs to be reassigned to 1998 so they are 18
#there are 4,251 people with undesignated gender (U)
#there are 6,009 people with undesignated race (U)
#there are 38,370 people with undesignated ethnicity (U)


for (demographic_var in c("birth_year","birth_age","race_code","gender_code","ethnic_code")){
  tmp <- sqldf(paste("select",demographic_var,",count(*) 
                     from votes_with_reg_data 
                     where voted_county_desc='FORSYTH' AND election_desc='11/08/2016 GENERAL'
                     group by",demographic_var))
  print(tmp)
  
  
}




#add county names that match the ACS data
votes_with_reg_data$Location <- ""
votes_with_reg_data$Location[votes_with_reg_data$voted_county_desc == "FORSYTH"] <- "Forsyth County, North Carolina"
votes_with_reg_data$Location[votes_with_reg_data$voted_county_desc == "CUMBERLAND"] <- "Cumberland County, North Carolina"
votes_with_reg_data$Location[votes_with_reg_data$voted_county_desc == "GASTON"] <- "Gaston County, North Carolina"
votes_with_reg_data$Location[votes_with_reg_data$voted_county_desc == "GUILFORD"] <- "Guilford County, North Carolina"


#replace birth year 1999 with 1998
votes_with_reg_data$birth_year[votes_with_reg_data$birth_year==1999] <- 1988

#replace birth year 1900 with null
votes_with_reg_data$birth_year[votes_with_reg_data$birth_year==1900] <- NA

#calculate age in 2016
votes_with_reg_data$age_2016 <- 2016-votes_with_reg_data$birth_year



#summarize values by county
#if the repeated parts were variables errors would be less likely, but then we'd have to use paste() which would make it less readable...
#alternatively, this could have looped through sets of groupings/filters/names

voter_counts_16 <- sqldf("select Location,count(*) as votes_16
                          from votes_with_reg_data 
                          where voted_county_id in ('34','41','36','26') and election_desc = '11/08/2016 GENERAL'
                          group by Location")

voter_counts_14 <- sqldf("select Location,count(*) as votes_14
                          from votes_with_reg_data 
                         where voted_county_id in ('34','41','36','26') and election_desc = '11/04/2014 GENERAL'
                         group by Location")

voter_counts_12 <- sqldf("select Location,count(*) as votes_12
                          from votes_with_reg_data 
                         where voted_county_id in ('34','41','36','26') and election_desc = '11/06/2012 GENERAL'
                         group by Location")

voter_counts_10 <- sqldf("select Location,count(*) as votes_10
                          from votes_with_reg_data 
                         where voted_county_id in ('34','41','36','26') and election_desc = '11/02/2010 GENERAL'
                         group by Location")

voter_counts_08 <- sqldf("select Location,count(*) as votes_08
                          from votes_with_reg_data 
                         where voted_county_id in ('34','41','36','26') and election_desc = '11/04/2008 GENERAL'
                         group by Location")

voter_counts_16_hisp <- sqldf("select Location,count(*) as votes_16_hisp
                          from votes_with_reg_data 
                          where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and ethnic_code='HL'
                          group by Location")

voter_counts_16_hisp_unknown <- sqldf("select Location,count(*) as votes_16_hisp_unknown
                          from votes_with_reg_data 
                              where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and ethnic_code='UN'
                              group by Location")

voter_counts_16_male <- sqldf("select Location,count(*) as votes_16_male
                          from votes_with_reg_data 
                                      where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and gender_code='M'
                                      group by Location")
voter_counts_16_female <- sqldf("select Location,count(*) as votes_16_female
                          from votes_with_reg_data 
                                      where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and gender_code='F'
                                      group by Location")
voter_counts_16_gender_unknown <- sqldf("select Location,count(*) as votes_16_gender_unknown
                          from votes_with_reg_data 
                              where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and gender_code='U'
                              group by Location")

voter_counts_16_black <- sqldf("select Location,count(*) as votes_16_black
                          from votes_with_reg_data 
                                      where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and race_code='B'
                                      group by Location")
voter_counts_16_white <- sqldf("select Location,count(*) as votes_16_white
                          from votes_with_reg_data 
                                      where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and race_code='W'
                                      group by Location")
voter_counts_16_whitenh <- sqldf("select Location,count(*) as votes_16_whitenh
                          from votes_with_reg_data 
                               where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and race_code='W' and ethnic_code='NH'
                               group by Location")
voter_counts_16_whiteun <- sqldf("select Location,count(*) as votes_16_whiteun
                          from votes_with_reg_data 
                               where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and race_code='W' and ethnic_code='UN'
                               group by Location")

voter_counts_16_18_24 <- sqldf("select Location,count(*) as votes_16_18_24
                          from votes_with_reg_data 
                                 where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and age_2016>=18 and age_2016<25
                                 group by Location")
voter_counts_16_25_44 <- sqldf("select Location,count(*) as votes_16_25_44
                          from votes_with_reg_data 
                                 where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and age_2016>=25 and age_2016<45
                                 group by Location")
voter_counts_16_45_64 <- sqldf("select Location,count(*) as votes_16_45_64
                          from votes_with_reg_data 
                               where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and age_2016>=45 and age_2016<65
                               group by Location")
voter_counts_16_65_plus <- sqldf("select Location,count(*) as votes_16_65_plus
                          from votes_with_reg_data 
                               where voted_county_id in ('34') and election_desc = '11/08/2016 GENERAL' and age_2016>=65
                               group by Location")


#combine all of these tables
combined_voting_results <- Reduce(function(...) merge(..., all = TRUE, by = "Location"),
       list(voter_counts_16, voter_counts_14, voter_counts_12,voter_counts_10,voter_counts_08,voter_counts_16_hisp,voter_counts_16_hisp_unknown,
            voter_counts_16_male,voter_counts_16_female,voter_counts_16_gender_unknown,
            voter_counts_16_black,voter_counts_16_white,voter_counts_16_whitenh,voter_counts_16_whiteun,
            voter_counts_16_18_24,voter_counts_16_25_44,voter_counts_16_45_64,voter_counts_16_65_plus))




#do PUMS analysis for age





B05003 <- 0


##########################  ACS Data Extraction


#annual data for each county
for (year in c("2016","2015","2014","2013","2012")){
  
  
  B05003 <- acs_getvar(B05003,counties,"B05003_008",paste("male adults",year),year=year)
  B05003 <- acs_getvar(B05003,counties,"B05003_012",paste("noncitizen male adults",year),year=year)
  B05003 <- acs_getvar(B05003,counties,"B05003_019",paste("female adults",year),year=year)
  B05003 <- acs_getvar(B05003,counties,"B05003_023",paste("noncitizen female adults",year),year=year)

  #calculate adult citizens by gender and overall
  B05003 <- acs_subract(B05003,paste("male adults",year),paste("noncitizen male adults",year),paste("male adult citizens",year))
  B05003 <- acs_subract(B05003,paste("female adults",year),paste("noncitizen female adults",year),paste("female adult citizens",year))
  B05003 <- acs_add(B05003,paste("male adult citizens",year),paste("female adult citizens",year),paste("adult citizens",year))  
  
}

race_eth <- c("B","H","I")
race_eth_names <- c("Black","Hispanic","Whitenh")

#demographics for Forsyth
for (i in 1:3){
  r_e <- race_eth[i]
  r_e_name <- race_eth_names[i]
  
  #get variables
  B05003 <- acs_getvar(B05003,"Forsyth",paste0("B05003",r_e,"_008"),paste("male adults",r_e_name))
  B05003 <- acs_getvar(B05003,"Forsyth",paste0("B05003",r_e,"_012"),paste("noncitizen male adults",r_e_name))
  B05003 <- acs_getvar(B05003,"Forsyth",paste0("B05003",r_e,"_019"),paste("female adults",r_e_name))
  B05003 <- acs_getvar(B05003,"Forsyth",paste0("B05003",r_e,"_023"),paste("noncitizen female adults",r_e_name))
  
  #calculate adult citizens by gender and overall
  B05003 <- acs_subract(B05003,paste("male adults",r_e_name),paste("noncitizen male adults",r_e_name),paste("male adult citizens",r_e_name))
  B05003 <- acs_subract(B05003,paste("female adults",r_e_name),paste("noncitizen female adults",r_e_name),paste("female adult citizens",r_e_name))
  B05003 <- acs_add(B05003,paste("male adult citizens",r_e_name),paste("female adult citizens",r_e_name),paste("adult citizens",r_e_name))
}

df <- B05003
var1 <- paste("male adults",r_e_name)
var2 <- paste("noncitizen male adults",r_e_name)





