counties=c("Forsyth","Cumberland","Guilford","Gaston")




acs_getvar <- function(df,counties,acs_var,varname,survey = "acs1",year = 2016){
  #add an ACS variable and its SE to a df
  #ideally this should work for both the variable name and label for code readability
  
  #get_acs isn't written to handle pulling counties from multiple states at once.  We'll have to address that later.
  
  #the resulting table will be merged to the input table.  if there is no input table (not a dataframe) it will merge it to population counts
  
  #get population counts if necessary
  if (!is.data.frame(df)){
    df <- as.data.frame(get_acs(geography = "county", 
                                         variables=("B01001_001"),
                                         #get all vars in tables
                                         year = year, #this year
                                         state = "NC", 
                                         county = counties,
                                         geometry = F, # no mapping geometry
                                         survey = "acs1",
                                         cache_table=T))
    
    #rename the variables
    names(df)[names(df) == 'estimate'] <- 'Total population'
    
    #drop variable name and moe
    df <- df[c('GEOID','NAME','Total population')]
  }
  
  #pull variable

  tmp <- as.data.frame(get_acs(geography = "county", 
                                         variables=acs_var,
                                         #get all vars in tables
                                         year = year, #this year
                                         state = "NC", 
                                         county = counties,
                                         geometry = F, # no mapping geometry
                                         survey = survey,
                                         cache_table=T))
  

  
  
  
  #replace missing moes with 0
  #how do other moe errors (e.g. small n) appear?  there needs to be a check for that
  tmp$moe[is.na(tmp$moe)] <- 0
  
  #replace moe with SE
  tmp$SE <- tmp$moe/1.645
  
  #rename the resulting variables
  names(tmp)[names(tmp) == 'estimate'] <- varname
  names(tmp)[names(tmp) == 'SE'] <- paste0(varname,"_SE")
  
  #remove moe and variable name
  tmp <- tmp[c('GEOID','NAME',varname,paste0(varname,"_SE"))]
  
  #verify that you have the right variable
  varlist <- load_variables(year, survey, cache = TRUE)
  fullvarname <- varlist[varlist$name==paste0(acs_var,"E"),c('concept','label')]
  
  
  print(paste(varname,"created based on",toString(fullvarname[1]),toString(fullvarname[2])))

  
  #merge to the base df
  df_out <- merge(df,tmp, by=c('GEOID','NAME'),all.x=TRUE)
  
  return(df_out)
}



acs_subract <- function(df,var1,var2,varname){
  #subtract two variables

  values <- df[[var1]]-df[[var2]]
  SEs <- sqrt(df[[paste(var1,"_SE",sep="")]]^2+df[[paste(var2,"_SE",sep="")]]^2)
  
  df[[varname]] <- values
  df[[paste0(varname,"_SE")]] <- SEs
  return(df)
}


acs_add <- function(df,var1,var2,varname){
  #adds two variables together
  #I should adapt this to accept a list of variables rather than two variables
  #This should be mostly replaced with acs_add_sumofvars which takes a list of ACS variables to import and sum
  values <- df[[var1]]+df[[var2]]
  SEs <- sqrt(df[[paste(var1,"_SE",sep="")]]^2+df[[paste(var2,"_SE",sep="")]]^2)
  
  df[[varname]] <- values
  df[[paste0(varname,"_SE")]] <- SEs
  return(df)
}

acs_add_sumofvars <- function(df,varlist,varname){
  #adds several variables together; this way none of the intermediary tables appear in the df

}

acs_proportion <- function(df,varlist,varname){
  #adds several variables together; this way none of the intermediary tables appear in the df
  
}