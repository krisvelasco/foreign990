## File for Downloading Specified PDFs from ProPublica API
# ANTI-LGBTQ EINs
# 06.06.322
# Kristopher Velasco and Sebastian Rojas Cabal. Adapted from Andrew Messamore.

library( RCurl )
library( jsonlite )
library( dplyr )
library( pander )
library(readxl)

rm(list=ls())

## Create Directory
setwd("/Users/srojascabal/Desktop/F990 Project/")
dir.create( "IRS990_PDFs_AntiLGBTQ" )
setwd( "./IRS990_PDFs_AntiLGBTQ" )

## Download Data #Change Link Below
antilgbtq_eins  <- read_csv("/Users/srojascabal/Desktop/F990 Project/Data_Main/eins_antilgbtq_20132020_220603.csv")
antilgbtq_eins <- antilgbtq_eins %>% 
  rename(ein_org = ein) %>%
  mutate(ein_org = as.character(ein_org))

## Write Function
download_pdfs <- function( ein )
{
  
  URL <- paste0( "https://projects.propublica.org/nonprofits/api/v2/organizations/", ein, ".json" )
  
  results.json <- getURL( URL, ssl.verifypeer = FALSE )
  
  r2 <- gsub( "\n", "", results.json )
  
  d <- fromJSON( r2 )
  
  pdfs <- d$filings_without_data
  
  if( nrow(pdfs) > 0 )
  {
    for( i in 1:nrow(pdfs) )
    {  
      
      file.name <- paste0( "EIN_", ein , "_YEAR_", pdfs$tax_prd_yr[i], "_FORMTYPE_", pdfs$formtype_str[i],  ".pdf" )
      download.file( url=pdfs$pdf_url[i], 
                     destfile=file.name,
                     mode = "wb")
  }
  
  return( dir() )
  
  }

}


## Pull PDFS

error_counter <- 0
# 517 errors

for (i in antilgbtq_eins$ein_org){
  
  skip_to_next <- FALSE
  
  tryCatch(download_pdfs(i), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next){ 
    print("Error Found")
    error_counter <- error_counter +1
    next
  }
  
  Sys.sleep(30)
}





