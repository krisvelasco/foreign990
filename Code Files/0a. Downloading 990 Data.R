## Project: Nonprofit Foreign Expenditures
## Date: December 24, 2021
## Overview: 
  # This file downloads all of the 990 data provided by Jacob Fenton. 
  # Fenton provided base 990 + Schedule I + Schedule F for all nonprofits
  # These data are across 41 files -- so lots of data
  # Note: These data are in CSV and quite messy. It'll take some work to clean

options(timeout = 300)

## Base Form
  # Return Header
    url <- "http://www.jacobfenton.com/990data/velasco/returnheader990x_part_i.csv.gz" 
    tmp <- tempfile()
    download.file(url,tmp)
    return_header <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
    write.csv(return_header,"return_header.csv", row.names = FALSE)

  # Part 0
      url <- "http://www.jacobfenton.com/990data/velasco/part_0.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_0 <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_0,"part_0.csv", row.names = FALSE)
      
  # Part 1
      url <- "http://www.jacobfenton.com/990data/velasco/part_i.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_i <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_i,"part_i.csv", row.names = FALSE)
  
  # Part 2
      url <- "http://www.jacobfenton.com/990data/velasco/part_ii.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_ii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_ii,"part_ii.csv", row.names = FALSE)
      
  # Part 3
      url <- "http://www.jacobfenton.com/990data/velasco/part_iii.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_iii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_iii,"part_iii.csv", row.names = FALSE)
  
  # Part 4
      url <- "http://www.jacobfenton.com/990data/velasco/part_iv.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_iv <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_iv,"part_iv.csv", row.names = FALSE)
      
  # Part 5
      url <- "http://www.jacobfenton.com/990data/velasco/part_v.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_v <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_v,"part_v.csv", row.names = FALSE)
      
  # Part 6
      url <- "http://www.jacobfenton.com/990data/velasco/part_vi.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_vi <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_vi,"part_vi.csv", row.names = FALSE)
  
  # Part 7
      url <- "http://www.jacobfenton.com/990data/velasco/part_vii.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_vii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_vii,"part_vii.csv", row.names = FALSE)
  
  # Part 8
      url <- "http://www.jacobfenton.com/990data/velasco/part_viii.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_viii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_viii,"part_viii.csv", row.names = FALSE)
  
  # Part 9
      url <- "http://www.jacobfenton.com/990data/velasco/part_ix.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_ix <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_ix,"part_ix.csv", row.names = FALSE)
   
  # Part 10
      url <- "http://www.jacobfenton.com/990data/velasco/part_x.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_x <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_x,"part_x.csv", row.names = FALSE)
  
  # Part 11
      url <- "http://www.jacobfenton.com/990data/velasco/part_xi.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_xi <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_xi,"part_xi.csv", row.names = FALSE)
      
  # Part 12
      url <- "http://www.jacobfenton.com/990data/velasco/part_xii.csv.gz" 
      tmp <- tempfile()
      download.file(url,tmp)
      part_xii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(part_xii,"part_xii.csv", row.names = FALSE)
      
  # Repeated Items
    #Contractor Compensation
        url <- "http://www.jacobfenton.com/990data/velasco/CntrctrCmpnstn.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        contractor_compensation <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(contractor_compensation,"contractor_compensation.csv", row.names = FALSE)
  
    #Foreign Country: Part V Line 4b
        url <- "http://www.jacobfenton.com/990data/velasco/FrgnCntryCd.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        foreign_country <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(foreign_country,"foreign_country.csv", row.names = FALSE)
        
    #Executive Compensation
        url <- "http://www.jacobfenton.com/990data/velasco/Frm990PrtVIISctnA.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        exec_compensation <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(exec_compensation,"exec_compensation.csv", row.names = FALSE)   
     
    #Other Expenses
        url <- "http://www.jacobfenton.com/990data/velasco/OthrExpnss.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        other_expenses <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(other_expenses,"other_expenses.csv", row.names = FALSE)   
        
    #Other Revenue
        url <- "http://www.jacobfenton.com/990data/velasco/OthrRvnMsc.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        other_revenue <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(other_revenue,"other_revenue.csv", row.names = FALSE)   
        
    #Program Service Accomplishments
        url <- "http://www.jacobfenton.com/990data/velasco/PrgSrvcAccmActyOthr.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        psa <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(psa,"psa.csv", row.names = FALSE)   
        
    #Program Service Revenues
        url <- "http://www.jacobfenton.com/990data/velasco/PrgrmSrvcRvn.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        psr <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(psr,"psr.csv", row.names = FALSE)   
        
    #NA
        url <- "http://www.jacobfenton.com/990data/velasco/SpclCndtnDsc.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        SpclCndtnDsc <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(SpclCndtnDsc,"SpclCndtnDsc.csv", row.names = FALSE)   
        
    #NA
        url <- "http://www.jacobfenton.com/990data/velasco/SttsWhrCpyOfRtrnIsFldCd.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        SttsWhrCpyOfRtrnIsFldCd <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(SttsWhrCpyOfRtrnIsFldCd,"SttsWhrCpyOfRtrnIsFldCd.csv", row.names = FALSE)   
        
    
# Schedule F
    #Part 1
      url <- "http://www.jacobfenton.com/990data/velasco/skedf_part_i.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_f_i <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_f_i,"sched_f_i.csv", row.names = FALSE)   
      
    #Part 2
      url <- "http://www.jacobfenton.com/990data/velasco/skedf_part_ii.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_f_ii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_f_ii,"sched_f_ii.csv", row.names = FALSE)   

    #Part 3
      url <- "http://www.jacobfenton.com/990data/velasco/skedf_part_iii.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_f_iii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_f_iii,"sched_f_iii.csv", row.names = FALSE)   
      
    #Part 4
      url <- "http://www.jacobfenton.com/990data/velasco/skedf_part_iv.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_f_iv <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_f_iv,"sched_f_iv.csv", row.names = FALSE)   

    #Part 5
      url <- "http://www.jacobfenton.com/990data/velasco/skedf_part_v.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_f_v <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_f_v,"sched_f_v.csv", row.names = FALSE)   
      
    #Repeated Items
        #Account Activities Outside US
          url <- "http://www.jacobfenton.com/990data/velasco/SkdFAccntActvtsOtsdUS.csv.gz"
          tmp <- tempfile()
          download.file(url,tmp)
          sched_f_activities <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
          write.csv(sched_f_activities,"sched_f_activities.csv", row.names = FALSE)   
      
        #Individual Grants
          url <- "http://www.jacobfenton.com/990data/velasco/SkdFFrgnIndvdlsGrnts.csv.gz"
          tmp <- tempfile()
          download.file(url,tmp)
          sched_f_individ_grants <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
          write.csv(sched_f_individ_grants,"sched_f_individ_grants.csv", row.names = FALSE)   
 
        #Org Grants
          url <- "http://www.jacobfenton.com/990data/velasco/SkdFGrntsTOrgOtsdUS.csv.gz"
          tmp <- tempfile()
          download.file(url,tmp)
          sched_f_org_grants <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
          write.csv(sched_f_org_grants,"sched_f_individ_grants.csv", row.names = FALSE)   
          
         #Supplemental Information
          url <- "http://www.jacobfenton.com/990data/velasco/SkdFSpplmntlInfrmtnDtl.csv.gz"
          tmp <- tempfile()
          download.file(url,tmp)
          sched_f_supplement <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
          write.csv(sched_f_supplement,"sched_f_supplement.csv", row.names = FALSE)   
          
# Schedule I
    # Part 1
      url <- "http://www.jacobfenton.com/990data/velasco/skedi_part_i.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_i_i <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_i_i,"sched_i_i.csv", row.names = FALSE)   
          
    # Part 2
      url <- "http://www.jacobfenton.com/990data/velasco/skedi_part_ii.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_i_ii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_i_ii,"sched_i_ii.csv", row.names = FALSE)   

    # Part 3
      url <- "http://www.jacobfenton.com/990data/velasco/skedi_part_iii.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_i_iii <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_i_iii,"sched_i_iii.csv", row.names = FALSE)   
      
     # Part 4
      url <- "http://www.jacobfenton.com/990data/velasco/skedi_part_iv.csv.gz"
      tmp <- tempfile()
      download.file(url,tmp)
      sched_i_iv <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
      write.csv(sched_i_iv,"sched_i_iv.csv", row.names = FALSE)   
      
    #Repeated Items
      #Individual Grants
        url <- "http://www.jacobfenton.com/990data/velasco/SkdIGrntsOthrAsstTIndvInUS.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        sched_i_individ_grants <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(sched_i_individ_grants,"sched_i_individ_grants.csv", row.names = FALSE)   
      
      #Recipient Table
        url <- "http://www.jacobfenton.com/990data/velasco/SkdIRcpntTbl.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        sched_i_recipient <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(sched_i_recipient,"sched_i_recipient.csv", row.names = FALSE)   
        
      #Supplemental Information
        url <- "http://www.jacobfenton.com/990data/velasco/SkdISpplmntlInfrmtnDtl.csv.gz"
        tmp <- tempfile()
        download.file(url,tmp)
        sched_i_supplement <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
        write.csv(sched_i_supplement,"sched_i_supplement.csv", row.names = FALSE)   
        
# Schedule J
    url <- "http://www.jacobfenton.com/990data/velasco/skedj_part_i.csv.gz"
    tmp <- tempfile()
    download.file(url,tmp)
    sched_j_i <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
    write.csv(sched_j_i,"sched_j_i.csv", row.names = FALSE)  
    
    
# Schedule O
    url <- "http://www.jacobfenton.com/990data/velasco/skedo_part_i.csv.gz"
    tmp <- tempfile()
    download.file(url,tmp)
    sched_o_i <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
    write.csv(sched_o_i,"sched_o_i.csv", row.names = FALSE)  
    
    #Repeated Items
    url <- "http://www.jacobfenton.com/990data/velasco/SkdOSpplmntlInfrmtnDtl.csv.gz"
    tmp <- tempfile()
    download.file(url,tmp)
    sched_o_supplement <- read.csv(gzfile(tmp), sep=",",header=TRUE, stringsAsFactors=FALSE)
    write.csv(sched_o_supplement,"sched_o_supplement.csv", row.names = FALSE)  
    
  