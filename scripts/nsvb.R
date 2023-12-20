# FIA VOLUME BY STATE AND COUNTY


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(httr)         # EVALIDator query

# library(magrittr)


# Functions ---------------------------------------------------------------
  
  
    EVALIDator <- function(argList, static = F){
      # make request
      if(static == F){
        url_resp <- "https://apps.fs.usda.gov/fiadb-api/fullreport"
      } else{
        url_resp <- "https://apps.fs.usda.gov/fiadbstatic-api/fullreport"
      }
    
    # make request
    resp <- POST(url=url_resp, body=argList, encode="form")
   
    # parse response from JSON to R list
    respObj <- content(resp, "parsed", encoding = "ISO-8859-1")
    # create empty output list
    
    # add estimates data frame to output list
    
    if(resp$status_code != 200){
      message("FIADB/EVALIDator response different to 200")
      output <- data.frame(ESTIMATE = NA, PLOT_COUNT = NA, SE = NA, SE_PERCENT = NA, VARIANCE = NA)
      
    } else {
      output <- as.data.frame(do.call(rbind,respObj$estimates))
    }
    
    return(output)
  }





#
# EVALIDator parameters ---------------------------------------------------

  wc <- read_xlsx("./EVALIDator_parameters/wc.xlsx", sheet = "wc") %>% 
    mutate(YEAR = as.numeric(substr(EVAL_GRP, nchar(EVAL_GRP)-3, EVAL_GRP)))  
    # %>% filter(YEAR>2000)
  
  
  
  rselected <- read_xlsx("./EVALIDator_parameters/rselected.xlsx")
  
  argList<- list(snum= NA, 
                 wc = NA,
                 rselected = "Forest type group",
                 cselected = "Stand origin",
                 estOnly = "N", 
                 outputFormat='NJSON')
  
  
  

#
# Validating forestland area, tpa and volume bf ---------------------------


  
  # Area estimation example
    argList$snum = 2
    
    
    t <- Sys.time()
    fia_area <- NULL
    
    
    for(i in seq(wc$EVAL_GRP)){
      argList$wc = wc$EVAL_GRP[i]
      
      fia_area <- fia_area %>%
        rbind(bind_cols(STATE = wc$STATE[i],
                            YEAR = wc$YEAR[i],
                            EVAL_GRP = wc$EVAL_GRP[i],
                            EVALIDator(argList) %>% rename_all(~paste0(., "_v210")),
                            EVALIDator(argList, static = T) %>% rename_all(~paste0(., "_v207")) 
        )
        )
      print(i)
    }
    
    Sys.time() - t
    
      fia_area <- fia_area %>% 
        mutate_all(unlist)
      
    write.csv(fia_area,"./output/forestland_area.csv", row.names = F)  
  
  
  
  
  # TPA estimation example
    argList$snum = 4
    
    
    t <- Sys.time()
    fia_tpa <- NULL
    
    
    for(i in seq(wc$EVAL_GRP)){
      argList$wc = wc$EVAL_GRP[i]
      
      fia_tpa <- fia_tpa %>%
        rbind(bind_cols(STATE = wc$STATE[i],
                        YEAR = wc$YEAR[i],
                        EVAL_GRP = wc$EVAL_GRP[i],
                        EVALIDator(argList) %>% rename_all(~paste0(., "_v210")),
                        EVALIDator(argList, static = T) %>% rename_all(~paste0(., "_v207")) 
        )
        )
      print(i)
    }
    
    Sys.time() - t
    
    fia_tpa <- fia_tpa %>% 
      mutate_all(unlist)
    
    write.csv(fia_tpa,"./output/forestland_tpa.csv", row.names = F) 
  
  
  
  # Gross board foot estimation example
    argList$snum = 22
    
    
    t <- Sys.time()
    fia_volbf <- NULL
    
    
    for(i in 675:772){
      argList$wc = wc$EVAL_GRP[i]
      
      fia_volbf <- fia_volbf %>%
        rbind(bind_cols(STATE = wc$STATE[i],
                        YEAR = wc$YEAR[i],
                        EVAL_GRP = wc$EVAL_GRP[i],
                        EVALIDator(argList) %>% rename_all(~paste0(., "_v210")),
                        EVALIDator(argList, static = T) %>% rename_all(~paste0(., "_v207")) 
        )
        )
      print(i)
    }
    
    Sys.time() - t
    
    fia_volbf <- fia_volbf %>% 
      mutate_all(unlist)
    
    write.csv(fia_volbf,"./output/forestland_volbf.csv", row.names = F) 
  
  

# EVALIDator query --------------------------------------------------------
  # https://www.fs.usda.gov/research/programs/fia/nsvb, 7. How does this affect previous FIA report.  
  # Variables to query: cubic-foot volume, total aboveground biomass, biomass of tree components, and carbon pool estimates
    
    
    
    argList_v210 <- argList_v207 <- argList
    
    
    
    variables <- tribble(
      ~snum_v210,  ~snum_v207,
      # Volume
      # 11001, "Gross total-stem wood volume of live trees (timber species at least 1 inch d.b.h.), in cubic feet, on forest land",
      # 11002, "Gross total-stem bark volume of live trees (timber species at least 1 inch d.b.h.), in cubic feet, on forest land",
      # 574173, "Gross bole wood volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land",
      # 11009,  "Gross bole bark volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land",
      # 11005,  "Gross stump wood volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land",
      # 11006,  "Gross stump bark volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land",
      # 11010, "Gross stem-top (above 4-inch top diameter) wood volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land",
      # 11011, "	Gross stem-top (above 4-inch top diameter) bark volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land",
      
      # Aboveground biomass tons
         10,       10, # Aboveground biomass of live trees (at least 1 inch d.b.h./d.r.c.), in dry short tons, on forest land
      60000,       60, # Aboveground biomass of live trees (woodland species at least 1 inch d.r.c.), in dry short tons, on forest land
      11000,       11, # Merchantable bole bark and wood biomass of live trees (timber species at least 5 inches d.b.h.), in dry short tons, on forest land
      56000,       56, # Top and limb bark and wood biomass of live trees (timber species at least 5 inches d.b.h.), in dry short tons, on forest land
      57000,       57, # Aboveground biomass of live saplings (timber species at least 1 and less than 5 inches d.b.h.), in dry short tons, on forest land
      58000,       58, # Stump bark and wood biomass of live trees (timber species at least 5 inches d.b.h.), in dry short tons, on forest land
         59,       59, # Belowground biomass of live trees (at least 1 inch d.b.h./d.r.c.), in dry short tons, on forest land
      
         96,       96, # Aboveground biomass of standing dead trees (at least 5 inches d.b.h./d.r.c.), in dry short tons, on forest land

      # carbon tons
      53000,       53, # Aboveground carbon in live trees (at least 1 inch d.b.h./d.r.c.), in short tons, on forest land
      54000,       54, # Belowground carbon in live trees (at least 1 inch d.b.h./d.r.c.), in short tons, on forest land
      55000,       55, # Aboveground and belowground carbon in live trees (at least 1 inch d.b.h./d.r.c.), in short tons, on forest land
      47000,       47, # Aboveground and belowground carbon in standing dead trees (at least 1 inch d.b.h./d.r.c.), in short tons, on forest land
         97,       97, # Total carbon, in short tons, on forest land
         
      # Total carbon tonnes
         

    )
      
  
    t <- Sys.time()
    
    evalidator_v210<- NULL
    evalidator_v207<- NULL
    
    for(j in seq(variables$snum_v210)){
      argList_v210$snum = variables$snum_v210[j]
      argList_v207$snum = variables$snum_v207[j]
      
      for(i in seq(wc$EVAL_GRP)){
        argList_v210$wc = wc$EVAL_GRP[i]
        argList_v207$wc = wc$EVAL_GRP[i]
        
        evalidator_v210 <- evalidator_v210 %>%
                bind_rows(bind_cols(VARIABLE = variables$snum_v210[j],
                                    EVAL_GRP = wc$EVAL_GRP[i],
                                    EVALIDator(argList_v210)))

        evalidator_v207 <- evalidator_v207 %>%
          bind_rows(bind_cols(VARIABLE = variables$snum_v207[j],
                              EVAL_GRP = wc$EVAL_GRP[i],
                              EVALIDator(argList_v207, static = T)))
                
        print(c(j, i, variables$snum_v210[j], wc$EVAL_GRP[i]))
      }
      
    }
    
    Sys.time() - t
    
    evalidator_v210 <- evalidator_v210 %>% 
      replace(.=="NULL", NA) %>% 
      mutate_all(unlist) 

    evalidator_v207 <- evalidator_v207 %>% 
      replace(.=="NULL", NA) %>% 
      mutate_all(unlist) 
    
    evalidator_data <- evalidator_v210 %>% 
      left_join(evalidator_v207, 
                by = c("VARIABLE", "EVAL_GRP", "GRP1", "GRP2"),
                suffix = c(".v210", ".v207")) %>% 
      mutate(FORTYPGRPCD = as.numeric(substr(GRP1, 2, 5)),
             FORTYPGRPDES = substr(GRP1, 7, length(GRP1)),
             STDORGCD = as.numeric(substr(GRP2, 2, 5)),
             STDORGDES = substr(GRP2, 7, length(GRP2)))

    
    write.csv(fia_totals,"./output/forestland_AGB_AGC.csv", row.names = F)  
  

# Recycling ---------------------------------------------------------------

  # Area estimation example

  
  
  
  
  
  argList$snum = 2
  
  
  t <- Sys.time()
  
  post_v210 <- NULL
  post_v207 <- NULL
  
  
  for(i in seq(wc$EVAL_GRP)){
    argList$wc = wc$EVAL_GRP[i]
    post_v210 <- post_v210 %>% 
      bind_rows(bind_cols(STATE = wc$STATE[i],
                          YEAR = wc$YEAR[i],
                          EVAL_GRP = wc$EVAL_GRP[i],
                          EVALIDator(argList)$estimates
      ))
    post_v207 <- post_v207 %>% 
      bind_rows(bind_cols(STATE = wc$STATE[i],
                          YEAR = wc$YEAR[i],
                          EVAL_GRP = wc$EVAL_GRP[i],
                          EVALIDator(argList, static = T)$estimates, 
      ))
    
    print(i)
  }
  
  Sys.time() - t
  
  
  
  area <- post_v210 %>% 
    
    mutate_all(unlist) %>% 
    left_join(post_v207 %>% 
                mutate_all(unlist),
              by = "EVAL_GRP",
              suffix = c("_v210", "_v207")) 
  
  
  

# Parellel 
  
  # library(doParallel)
  # detectCores()
  # cl <- makeCluster(10)
  # registerDoParallel(cl)
  # 
  # 
  # 
  # # Area estimation example
  # argList$snum = 2
  # 
  # 
  # t <- Sys.time()
  # 
  # fia <- foreach(i = seq(wc$EVAL_GRP), .combine=rbind, .packages = c("httr","dplyr")) %do% {
  #   argList$wc = wc$EVAL_GRP[i]
  #   fia <- bind_cols(EVAL_GRP = wc$EVAL_GRP[i],
  #                    EVALIDator(argList)$estimates %>% rename_all(~paste0(., "_v210")),
  #                    EVALIDator_static(argList)$estimates %>% rename_all(~paste0(., "_v207")))
  #   }
  # 
  # Sys.time() - t
  # 
  
  
  
  


























