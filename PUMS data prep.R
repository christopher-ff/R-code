#the file you store the raw data in.  NOT THE SAME AS THIS PROJECT OR THE PUMS DATA WILL BE BACKED UP ON GITHUB
data_directory <- "C:/Users/staff/Desktop/Data/R/Data/"



state_FIPS <- c("al","ak","az","ar","ca","co","ct","de","dc","fl","ga","hi","id","il","in","ia","ks","ky","la","me","md","ma","mi","mn","ms","mo","mt","ne","nv","nh","nj","nm","ny","nc","nd","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","vt","va","wa","wv","wi","wy")
names(state_FIPS)=c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56")


currentyearn <- 2016  
st <- "nc"  
sampletypen <- 1  

  
download_raw_pums <- function(currentyearn,st,sampletypen,data_directory){
  #check whether you have the raw hnc/pnc files, and download them if you don't have them
  
  currentyear <- toString(currentyearn)

  sampletype <- toString(sampletypen)
  
  #last two characters of year
  yr2 <- substr(currentyear,3,4)
  
  
  #do this for both h and p (housing and person files)
  for (hp in c("h","p")){
    
  
    #location of census pums data
    target_address <- paste("https://www2.census.gov/programs-surveys/acs/data/pums/",currentyear,"/",sampletype,"-Year/csv_",hp,"nc.zip",sep="")
    #name of the file as it appears in the .zip.  This name cannot be kept because it doesn't note the sample (1/5)!
    
    file_in_zip <- paste("ss",yr2,hp,st,".csv",sep="")
    #print(file_in_zip)
    
    #the filename we will use
    output_filename <- paste("ss",yr2,hp,st,sampletype,"yr",sep="")

    data_file_directory <- paste(data_directory,output_filename,'.rds',sep="")
    
    #check if the data for this year/sample has already been created
    #create it if it hasn't been created
    if(!file.exists(data_file_directory)){
      print(paste(data_file_directory,"not found"))
      #make space for a temporary file
      temp <- tempfile()
      #download into that file
      download.file(target_address,temp)
      #extract the one needed file from the .zip
      print(unzip(temp, list = TRUE))
      csv <- unz(temp, file_in_zip)
      #convert from .csv to .Rda
      pumsfile <- read.table(csv, 
                        header = TRUE,
                        sep = ",")
      saveRDS(pumsfile, file=data_file_directory)
      unlink(temp)
    }
  
  }
}


#Identify the PUMA12s and PUMA10s needed using crosswalks from http://mcdc.missouri.edu/websas/geocorr90_htmls/geocorr.help.html

county_to_puma00 <- read.table(paste(data_directory,"County to PUMA00.csv",sep=""), 
                  header = TRUE,
                  sep = ",")
county_to_puma12 <- read.table(paste(data_directory,"County to PUMA12.csv",sep=""), 
                  header = TRUE,
                  sep = ",")




create_county_file <- function(fipscode,yearn,samplen,data_directory){
  #combine p and h files for a county based on the county's 5 digit FIPS code
  
  
  
  fipscode_str <- toString(fipscode)
  state_code <- substr(fipscode_str,1,2)
  
  state <- state_FIPS[state_code]
  st <- state_FIPS[state_code]
  print(st)
  
  
  year <- toString(yearn)
  sampletype <- toString(samplen)
  yr2 <- substr(year,3,4)
  
  #get the required data if necessary
  download_raw_pums(yearn,state,samplen,data_directory)
  
  
  
  #get a list of PUMAs to extract from the state file
  PUMAs_12 <- as.numeric(as.vector(county_to_puma12[['puma12']][county_to_puma12$county == fipscode]))
  PUMAs_2k <- as.numeric(as.vector(county_to_puma00[['puma2k']][county_to_puma00$county == fipscode]))
  
  cntyname <- as.vector(county_to_puma12[['cntyname']][county_to_puma12$county == fipscode])[1]
  
  
  data_file_directory <- paste(data_directory,"ss",yr2,"ph",fipscode_str,"_",sampletype,"yr",'.rds',sep="")
  
  #check whether the combined file has already been created
  if(!file.exists(data_file_directory)){
      
    h_filename <- paste("ss",yr2,"h",st,sampletype,"yr",sep="")
    p_filename <- paste("ss",yr2,"p",st,sampletype,"yr",sep="")
    
    h_data_file_directory <- paste(data_directory,h_filename,'.rds',sep="")
    p_data_file_directory <- paste(data_directory,p_filename,'.rds',sep="")
    
    #h_st <- load(h_data_file_directory)
    #p_st <- load(p_data_file_directory)
    
    h_st <- readRDS(h_data_file_directory)
    p_st <- readRDS(p_data_file_directory)

    #make supsets of the p and h files based on PUMAs.  5 year files 2012-2015 use both kinds.
    #determine which PUMA lookup table to use
    if (samplen == 1 & yearn > 2011) {
      h_county <- h_st[h_st$PUMA %in% PUMAs_12,]
      p_county <- p_st[p_st$PUMA %in% PUMAs_12,]
      
    } else if (samplen == 1 & yearn <=2011){
      h_county <- h_st[h_st$PUMA %in% PUMAs_2k,]
      p_county <- p_st[p_st$PUMA %in% PUMAs_2k,]
      
    } else if (samplen == 5 & yearn >= 2016){
      h_county <- h_st[h_st$PUMA %in% PUMAs_12,]
      p_county <- p_st[p_st$PUMA %in% PUMAs_12,]
      
    } else if (samplen == 5 & yearn <= 2011){
      h_county <- h_st[h_st$PUMA %in% PUMAs_2k,]
      p_county <- p_st[p_st$PUMA %in% PUMAs_2k,]
      
    } else if (samplen == 5 & yearn > 2011 & yearn < 2016) {
      h_county <- h_st[h_st$PUMA10 %in% PUMAs_12 | h_st$PUMA00 %in% PUMAs_2k,]
      p_county <- p_st[p_st$PUMA10 %in% PUMAs_12 | p_st$PUMA00 %in% PUMAs_2k,] 
      
    } else stop("please use a four digit year and 1 or 5 for the sample type")
    
    #merge the h and p files
    if (samplen == 5 & yearn > 2011 & yearn < 2016) {
      ph <- merge(p_county,h_county,by=c("SERIALNO","PUMA00","PUMA10"),all.x=TRUE)
      
    } else{
      ph <- merge(p_county,h_county,by=c("SERIALNO","PUMA"),all.x=TRUE)
      
    }
    
    #add the county name from the crosswalk
    ph$cntyname <- cntyname
    
    status <- "created"
    saveRDS(ph, file=data_file_directory)
  }else{
    ph <- readRDS(data_file_directory)
    status <- "already created"
  }
  
  #county population for validation 
  pop <- sum(ph$PWGTP)
  
  
  print(paste(cntyname,": pop",pop,year,sampletype, status))
}  


#create_county_file(37067,2016,1,data_directory)




#iterate over the paramaters
for (samplespan in c(1,5)){
  for (county in c(37067)){
    for (year in c(2016,2015,2014,2013,2012,2011,2010)){
      create_county_file(37067,year,samplespan,data_directory)
    }
  }
}

#iterate over the early years
for (samplespan in c(1)){
  for (county in c(37067)){
    for (year in c(2009,2008,2007)){
      create_county_file(37067,year,samplespan,data_directory)
    }
  }
}







replicate_calculation <- function(observed,reweighted_calculations){
  #calculate a SE based on the square difference between the observed value and 80 other values based on different weights
  #REMEMBER TO USE PWGTP FOR PERSON LEVEL ANALYSIS AND WGTP FOR HOUSEHOLD LEVEL ANALYSIS.
  #reweighted_calculations must be an 80 item list
  if (!length(reweighted_calculations)==80){stop("The second argument did not have an 80 item list")}
  
  #this doesn't work well when calculating medians / quantiles
  #and can return a SE of 0 if there are few enough records that they are all the same
  #(e.g. if  there is only one record that is a hispanic citizen over 65 in Forsyth, the unemployment rate of this group value would be 0 or 100 no matter how you reweight the calculation, causing the SE to be 0)
  
  
  #calculate the sum of squared differences
  #this formula can be found in https://usa.ipums.org/usa/repwt.shtml among other places
  sum_of_squared_diff <-0
  for (reweighted_calculation in reweighted_calculations){
    
    diff <- observed-reweighted_calculation
    sqdiff <- diff^2
    #add the squared diff to a running total
    sum_of_squared_diff <- sum_of_squared_diff+sqdiff
    
  }
  SE <- sqrt(sum_of_squared_diff/20)

  return(SE)
  
}

#calculate the percent of a group that has certain characteristics
replicate_proportion <- function(test_name,df,numerator_var,numerator_value,denominator_var="TYPE",denominator_value=1,type='p'){
  

  #define weights as either the person/household weights
  if (type=='p'){weight_type <- "PWGTP"
  } else {weight_type <- "WGTP"}

  #filter the base dataframe
  df <-  df[df[[denominator_var]]==denominator_value,]
  dfn <- df[df[[numerator_var]]==numerator_value,]
  
  
  #get population counts
  denominator_population <- sum(df[[weight_type]])
  numerator_population <- sum(dfn[[weight_type]])
  proportion <- numerator_population/denominator_population
  
  #a blank vector to paste values into
  reweighted_results <- vector("list", 80)
  reweighted_num <- vector("list", 80)
  reweighted_denom <- vector("list", 80)
  
  #this makes WGTP/PWGTP 1-80
  for (weightnum in 1:80){
    weightvar <- paste(weight_type,as.character(weightnum),sep="")
    #reweighted values
    denominator_population_tmp <- sum(df[[weightvar]])
    numerator_population_tmp <- sum(dfn[[weightvar]])    
    proportion_tmp <- numerator_population_tmp/denominator_population_tmp
    
    #save the reweighted proportion
    reweighted_results[weightnum] <- proportion_tmp
    reweighted_num[weightnum] <- numerator_population_tmp
    reweighted_denom[weightnum] <- denominator_population_tmp
    
    
  }
  SE <- replicate_calculation(proportion,proportion_tmp)
  SE_num <- replicate_calculation(numerator_population,numerator_population_tmp)
  SE_denom <- replicate_calculation(denominator_population,denominator_population_tmp)
  CV=100*SE/proportion
  
  return(c(test_name,proportion,SE,CV,numerator_population,SE_num,denominator_population,SE_denom))
}




