library(ggplot2)
library(moments)
library(tseries) 


### Read the CSV File 
CsvRead <- function(data_location) {
    ##Function to define the dataframes,
    # converts the date column into r date format 
    # for easy reading 
    
    df <- read.csv(data_location, header = TRUE)
    df$date <- strptime(df$date, '%Y%m%d')
    return(df)
}

df <- CsvRead('cat.csv')


Moments <- function (df) { 
    ###Question 1a: Find all the moments of the data sets. 
    ## This function creates a dataframe that contains the moments of the data provided 
    #Moment 1: mean 
    #Moment 2: standard deviation 
    #Moment 3: skewness 
    #Moment 4: kurtosis
    #Min and Max are outputted 



    df_no_date <- df[-c(2)]
    mean <- apply(df_no_date, 2, moment)
    sd <- apply(df_no_date, 2, sd)
    skewness <- apply(df_no_date, 2, skewness)
    kurtosis <- apply(df_no_date, 2, kurtosis)
    min <- apply(df_no_date, 2, min) 
    max <- apply(df_no_date, 2, max) 

    return (rbind(mean, sd, skewness, kurtosis, min, max) ) 

}


### Read CSV
cat <- CsvRead('cat.csv') 
proc_gamble <- CsvRead('Proc_Gam.csv') 

### Question 1a: Calculate mean, standard deviation, skewness, kurtosis, min, max
cat_moments <- Moments(cat) 
proc_gamble_moments <- Moments(proc_gamble)

### Question 1b: Empirical Density Function of simple returns on Catapillar stock. 
empirical_cat_ret <- ecdf(cat$RET) 

plot(empirical_cat_ret, xlab = 'Simple Returns', ylab = 'Frequency', main = 'Cat Cumulative Density Function For Simple Returns') 
hist(cat$RET, xlab = 'Simple Returns', ylab = 'Frequency', main = 'Cat Probability Distribution Function for Simple Returns')

##Tests for skewness and 
jarque.test(cat$RET)

agostino.test(cat$RET) 

anscombe.test(cat$RET) 


cat$LOGRET <- log(cat$RET) 
head(cat) 





