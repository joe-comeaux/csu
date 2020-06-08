
#install.packages("collections")
require(readxl)
require(tidyverse)
#library(collections)

#pops <- get_population_1("CAN",3)


get_countries <- function () {
  countries <- read.csv("countries.txt",header=TRUE)
  countries$ABBR <- trimws(countries$ABBR,which=c("both"))
  countries$COUNTRY <- trimws(countries$COUNTRY,which=c("both"))
  
  return (countries)
}

get_population <- function() {
  #create a list of the files from your target directory
  file_list <- list.files(path="Population/")
  
  popls <- data.frame()
  
  
  for (i in 1:length(file_list)){
    file = paste("Population/",file_list[i],sep="")
    list = read.table(file,skip=2,header=TRUE)
    temp <- unlist(strsplit(file_list[i],"\\."))
    ctry = temp[1]
    list$Coutry = ctry
    popls <- rbind(popls,list)
  
  }
#  print (popls)
  return (popls)
}

get_population_1 <- function(ctry,popYear) {
  #create a list of the files from your target directory
  file <- paste("Population/",ctry,".Population.txt",sep="")
 
  ts <- data.frame()
  popls <- data.frame()
  
    list <- read.table(file,skip=2,header=TRUE)
  #  temp <- unlist(strsplit(file_list[i],"\\."))
  #  ctry = temp[1]
    list$Coutry <- ctry
    popls <- rbind(popls,list)
    hold <- filter(popls,str_detect(popls$Year,"\\+",negate=TRUE))
    hold$Year <- str_replace(hold$Year,"-","")
    popls <- hold
     year_min <- min(popls$Year)
     year_max <-  max(popls$Year)
    
   
# get Total population   
      pops <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("Year","Total","Male","Female","MalePct","FemalePct","Growth","Hypo"))
     
      for (j in year_min:year_max) {
         temp <- filter(popls,Year == j)
         
         totl <- sum(temp$Total)
         male <- sum(temp$Male)
         female <- sum(temp$Female)
        
         malepp <- 100*male/totl
         femalepp <- 100*female/totl
         if (j > year_min) {
            growth <- 100*(totl - totl0)/totl
            res <- t.test(temp$Male, temp$Female, var.equal = FALSE)
            # assign pass/fail for each results .. these are changed to 
            status <- ""
            if (abs(res$p.value) >= .05) {
              status = "Pass"
            } else if (abs(res$p.value) < .05) {
              status = "Fail"
            } 
            h <- res$p.value
         } else {
            growth <- 0
            status <- ""
            h <- -1
         }
         totl0 <- totl
         temp0 <- temp
         pops[nrow(pops)+1,] <- c(j,totl,male,female,malepp,femalepp,growth,h)
      }
      
      popa <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("Year","Total","Male","Female","MalePct","FemalePct","Growth","Hypo"))
      
      for (j in 0:100) {
        temp <- filter(popls,Age == j)
        
        totl <- mean(temp$Total)
        male <- mean(temp$Male)
        female <- mean(temp$Female)
        malePct <- 100 * male/totl
        femalePct <- 100 * female/totl
      
        if (j > 0) {
          growth <- 100 * (totl - totl0)/totl0
        } else {
          growth <- 0
        }
        
        res <- t.test(temp$Male, temp$Female, var.equal = TRUE)
        # assign pass/fail for each results .. these are changed to 
        status <- ""
        if (abs(res$p.value) >= .05) {
          status = "Pass"
        } else if (abs(res$p.value) < .05) {
          status = "Fail"
        } 
        h <- res$p.value
        
        
        popa[nrow(popa)+1,] <- c(j,totl,male,female,malePct,femalePct,growth,h)
        totl0 <- totl
      }
      
      hold <- list(pops,popa)
      
      return (hold)
 # return (ts)
}


get_population_1Age <- function(ctry,popYear) {
  #create a list of the files from your target directory
  file <- paste("Population/",ctry,".Population.txt",sep="")
  
  
  
  ts <- data.frame()
  popls <- data.frame()
  
  list <- read.table(file,skip=2,header=TRUE)
  #  temp <- unlist(strsplit(file_list[i],"\\."))
  #  ctry = temp[1]
  list$Coutry <- ctry
  popls <- rbind(popls,list)
  hold <- filter(popls,str_detect(popls$Year,"\\+",negate=TRUE))
  hold$Year <- str_replace(hold$Year,"-","")
  popls <- hold
  year_min <- min(popls$Year)
  year_max <-  max(popls$Year)
  
  
  # get Total population   
  pops <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Year","Total","Male","Female","MalePct","FemalePct"))
  
  for (j in 0:100) {
    temp <- filter(popls,Age == j)
    
    totl <- mean(temp$Total)
    male <- mean(temp$Male)
    female <- mean(temp$Female)
    malePct <- 100 * male/totl
    femalePct <- 100 * female/totl
    pops[nrow(pops)+1,] <- c(j,totl,male,female,malePct,femalePct)
  }
  
  hold <- list(pops,pops)
  return (hold)
  # return (ts)
}
