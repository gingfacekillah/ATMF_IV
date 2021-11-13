# *********** AT THE MONEY FORWARD IMPLIED VOLATILITY EMMULATION  ************ #
# **************************************************************************** # 
# *********** GOAL: EMMULATE THE VIX FOR DIFFERENT SECURITIES **************** #
# **************************************************************************** # 
# **************************** MOTIVATION ************************************ # 
# ******** FOR ESTIMATION OF IMPLIED VOLATILITY SUMMARY STATISTICS *********** #
################################################################################
# LOAD THE PACKAGES ******************************************************** ####
suppressMessages(pacman::p_load("PerformanceAnalytics","quantmod",
                                "LSMRealOptions","RQuantLib","fOptions",
                                "PortfolioAnalytics","reticulate","LSMonteCarlo",
                                "anytime","pbapply","lubridate",
                                "TTR","dplyr","IBrokers","alphavantager",
                                "tidyquant","httr","purrr","rvest",
                                "devtools","urca","derivmkts","tidyr",
                                "data.table","jsonlite","stringr","xml2","tibble"))
# DATE AND TIME FORMATTING ************************************************* ####
start_time <- Sys.time()
first.date <- Sys.Date()-(365*1)
last.date <- Sys.Date()
# FUNCTIONS TO PULL OPTIONS ************************************************ ####
# * Citation: https://github.com/jgQuantScripts/CBOE_OPTIONS * #
# function to extract Expirations, Flag, and Strike from Option Name
getEFS = function(x){
  expiry = str_sub(x, -15,-10)
  expiry = as.character(as.Date(expiry,format="%y%m%d"))
  flag   = str_sub(x,-9,-9)
  strike = str_sub(x,-8,-1)
  left   = str_sub(strike,-8,-4)
  right  = str_sub(strike,-3,-1)  
  strike = paste0(left,".",right)
  strike = as.numeric(strike)
  as.data.frame(cbind(expiry,flag,strike))
}
# get Options + Calculate IV & Greeks
Pull.Options = function(symbol){
  # url to get options - will read in all using json
  #url = "https://cdn.cboe.com/api/global/delayed_quotes/options/_SPX.json"
  url = paste0("https://cdn.cboe.com/api/global/delayed_quotes/options/",symbol,".json")
  # read in data from page
  df = read_json(url,simplifyVector = TRUE)
  # convert as data frame
  opts = as.data.frame(df$data$options)
  # get Expiration, Flag, & Strike
  efs <- getEFS(opts$option)
  # combine with options data
  opts <- cbind(opts,efs)
  # fix last_trade_time
  opts$last_trade_time <- as.character(as.POSIXct(opts$last_trade_time,
                                                  format="%Y-%m-%dT%H:%M:%S"))
  opts$stkClose <- df$data$close
  # add date pulled
  opts$Date = as.character(Sys.Date())
  # add Days to Expiration
  opts$days2Exp = as.Date(opts$expiry) - as.Date(opts$Date) 
  # Option Mid Price  
  opts$Mid = round((opts$bid + opts$ask)/2,2)
  # add ticker column
  opts$Symbol = symbol
  opts
}
# PULL THE INDEX/ASSETS' OPTIONS ******************************************* ####
SYMBOL = "_SPX" #"_SPX", "SPY", "TSLA"
ASSIGNMENT = "european" # "european" or "american"
OPTIONZ <- suppressWarnings(Pull.Options(SYMBOL))
# BASIC CHAIN FILTRATION *************************************************** ####
# Clean the Chain
CLEAN.CHAIN <- function(OPTIONZ){
  # Omit the NAs from the Chain
  OPTIONZ   <- OPTIONZ %>% dplyr::select(option,bid,ask,Mid,expiry,flag,strike,days2Exp)
  # Format
  OPTIONZ$days2Exp  <- OPTIONZ$days2Exp %>% as.numeric()
  OPTIONZ$strike    <- OPTIONZ$strike %>% as.numeric()
  OPTIONZ$expiry    <- as.Date(OPTIONZ$expiry)
  # Get a Current Spot Price
  SYMBOL = if (SYMBOL ==  "_SPX"){"^GSPC"}else{SYMBOL}
  UND_SPOT            <- getQuote(SYMBOL)[2] %>% as.numeric(); UND_SPOT
  OPTIONZ$UND_SPOT <- replicate(nrow(OPTIONZ),UND_SPOT)
  # Return the OPTIONZ
  OPTIONZ
}
OPTIONZ     <- CLEAN.CHAIN(OPTIONZ)
# Get the DTEs for the Chain
get.CHAIN.DTEs <- function (OPTIONZ,SYMBOL){
  # Keep all Options where Days to Expiration 23<=T<=37
  OPTIONZ    <- OPTIONZ[OPTIONZ$days2Exp>=23 & OPTIONZ$days2Exp<=37,]
  # empty xts object to find the 3rd friday of each month
  DTE        = xts(rep(NA,length(unique(OPTIONZ$expiry))), order.by=unique(OPTIONZ$expiry))
  # location
  NUM        = options.expiry(DTE)
  # FORMAT Expiration that falls on the 3rd Friday
  THIRD_FRI  = format(index(DTE)[NUM], "%y%m%d")
  # Symbol Reformat
  SYMBOL     = "SPX"
  # Exclude non-standard options (those that don't expire AM)
  OPTIONZ = OPTIONZ[!str_detect(OPTIONZ$option, pattern = paste0(SYMBOL,"W",THIRD_FRI)),]
  # unique expirations
  unique(OPTIONZ$expiry)
  OPTIONZ_NEAR = subset(OPTIONZ,OPTIONZ$days2Exp == min(unique(OPTIONZ$days2Exp)))
  OPTIONZ_NEXT = subset(OPTIONZ,OPTIONZ$days2Exp == max(unique(OPTIONZ$days2Exp)))
  OPTIONZ      = list(OPTIONZ_NEAR,OPTIONZ_NEXT)
  OPTIONZ
}
OPTIONZ        <- get.CHAIN.DTEs(OPTIONZ)
# GET THE NEAR AND NEXT TERM CALLS & PUTS - Generate Functions ************* ####
# Near Term Functions
get.OPTIONZ_NEAR_CALLS <- function (OPTIONZ){
  # Unlist
  OPTIONZ                  <- OPTIONZ[[1]] %>% data.frame()
  # Subset for Calls
  OPTIONZ                  <- OPTIONZ[OPTIONZ$flag == "C",] 
  # Get the ITM Options
  OPTIONZ_NEAR_CALLS_ITM   <- OPTIONZ[as.numeric(OPTIONZ$strike) < OPTIONZ$UND_SPOT,]
  # Get the OTM Options
  OPTIONZ_NEAR_CALLS_OTM   <- OPTIONZ[as.numeric(OPTIONZ$strike) > OPTIONZ$UND_SPOT,]
  # Add the ITM and OTM Identifiers
  OPTIONZ_NEAR_CALLS_ITM$Moneyness <- "ITM"
  OPTIONZ_NEAR_CALLS_OTM$Moneyness <- "OTM"
  # Bind
  OPTIONZ_NEAR_CALLS <- rbind(
    OPTIONZ_NEAR_CALLS_ITM,
    OPTIONZ_NEAR_CALLS_OTM
    )
  return(OPTIONZ_NEAR_CALLS)
}
get.OPTIONZ_NEAR_PUTS  <- function (OPTIONZ){
  # Unlist
  OPTIONZ                  <- OPTIONZ[[1]]
  # Subset for PUTS
  OPTIONZ                  <- OPTIONZ[OPTIONZ$flag == "P",] 
  # Get the ITM Options
  OPTIONZ_NEAR_PUTS_ITM    <- OPTIONZ[as.numeric(OPTIONZ$strike) > OPTIONZ$UND_SPOT,]
  # Get the OTM Options
  OPTIONZ_NEAR_PUTS_OTM    <- OPTIONZ[as.numeric(OPTIONZ$strike) < OPTIONZ$UND_SPOT,]
  # Add the ITM and OTM Identifiers
  OPTIONZ_NEAR_PUTS_ITM$Moneyness <- "ITM"
  OPTIONZ_NEAR_PUTS_OTM$Moneyness <- "OTM"
  # Bind
  OPTIONZ_NEAR_PUTS <- rbind(
    OPTIONZ_NEAR_PUTS_ITM,
    OPTIONZ_NEAR_PUTS_OTM
  )
  return(OPTIONZ_NEAR_PUTS)
}
# Next Term Functions
get.OPTIONZ_NEXT_CALLS <- function (OPTIONZ){
  # Unlist
  OPTIONZ                  <- OPTIONZ[[2]] %>% data.frame()
  # Subset for Calls
  OPTIONZ                  <- OPTIONZ[OPTIONZ$flag == "C",] 
  # Get the ITM Options
  OPTIONZ_NEXT_CALLS_ITM   <- OPTIONZ[as.numeric(OPTIONZ$strike) < OPTIONZ$UND_SPOT,]
  # Get the OTM Options
  OPTIONZ_NEXT_CALLS_OTM   <- OPTIONZ[as.numeric(OPTIONZ$strike) > OPTIONZ$UND_SPOT,]
  # Add the ITM and OTM Identifiers
  OPTIONZ_NEXT_CALLS_ITM$Moneyness <- "ITM"
  OPTIONZ_NEXT_CALLS_OTM$Moneyness <- "OTM"
  # Bind
  OPTIONZ_NEXT_CALLS <- rbind(
    OPTIONZ_NEXT_CALLS_ITM,
    OPTIONZ_NEXT_CALLS_OTM
  )
  return(OPTIONZ_NEXT_CALLS)
}
get.OPTIONZ_NEXT_PUTS  <- function (OPTIONZ){
  # Unlist
  OPTIONZ                  <- OPTIONZ[[2]] %>% data.frame()
  # Subset for PUTS
  OPTIONZ                  <- OPTIONZ[OPTIONZ$flag == "P",] 
  # Get the ITM Options
  OPTIONZ_NEXT_PUTS_ITM    <- OPTIONZ[as.numeric(OPTIONZ$strike) > OPTIONZ$UND_SPOT,]
  # Get the OTM Options
  OPTIONZ_NEXT_PUTS_OTM    <- OPTIONZ[as.numeric(OPTIONZ$strike) < OPTIONZ$UND_SPOT,]
  # Add the ITM and OTM Identifiers
  OPTIONZ_NEXT_PUTS_ITM$Moneyness <- "ITM"
  OPTIONZ_NEXT_PUTS_OTM$Moneyness <- "OTM"
  # Bind
  OPTIONZ_NEXT_PUTS <- rbind(
    OPTIONZ_NEXT_PUTS_ITM,
    OPTIONZ_NEXT_PUTS_OTM
  )
  return(OPTIONZ_NEXT_PUTS)
}
# Get the Calls and Puts for Near and Next Terms
OPTIONZ_NEAR_CALLS     <- get.OPTIONZ_NEAR_CALLS (OPTIONZ)
OPTIONZ_NEAR_PUTS      <- get.OPTIONZ_NEAR_PUTS  (OPTIONZ)
OPTIONZ_NEXT_CALLS     <- get.OPTIONZ_NEXT_CALLS (OPTIONZ)
OPTIONZ_NEXT_PUTS      <- get.OPTIONZ_NEXT_PUTS  (OPTIONZ)
# Rebind the Rows
OPTIONZ                <- do.call(rbind,OPTIONZ)
# GET TIME TO EXPIRATION IN YEARS ****************************************** ####
# Convert the Days to Expiration Along with System Time to Minutes
DTE_YR_FRAC            <- function (OPTIONZ){
  # Make a Temp df for the DTEs
  tmp             <- unique(OPTIONZ$days2Exp) %>% data.frame() %>% t() %>% data.frame()
  # Get DTE Near in Minutes
  DTE_NEAR     = as.numeric(tmp[1])/365
  ttt.tz       = if (Sys.timezone() == "US/Pacific"){"21:00"}else if (Sys.timezone() == "US/Central"){"23:00"}else if (Sys.timezone() == "US/Central"){"24:00"}
  ttt          = difftime(strptime(strftime(Sys.time(), format="%H:%M:%S"), format = "%H:%M"), strptime(ttt.tz, format = "%H:%M"), units = "mins") %>% as.numeric()
  ttt          = if (ttt > 0 & Sys.timezone() == "US/Pacific"){
    as.numeric((-1)*difftime(strptime(strftime(Sys.time(), format="%H:%M:%S"), format = "%H:%M"), strptime(ttt.tz, format = "%H:%M"), units = "mins")) + (24*60)
  }else{
    as.numeric((-1)*difftime(strptime(strftime(Sys.time(), format="%H:%M:%S"), format = "%H:%M"), strptime(ttt.tz, format = "%H:%M"), units = "mins"))
  }
  ttt
  M_CR_DAY        = ttt
  M_ST_DAY_NEAR   = 510
  DTE_NEAR        = ((24*60*as.numeric(tmp[1]))+M_ST_DAY_NEAR+M_CR_DAY)/525600

  # Get DTE Next in Minutes
  DTE_NEXT        = as.numeric(tmp[2])/365
  M_ST_DAY_NEXT   = 900
  DTE_NEXT        = ((24*60*as.numeric(tmp[2]))+M_ST_DAY_NEXT+M_CR_DAY)/525600
  # Combine into Data Frame
  DTE.DF          <- data.frame(DTE_NEAR,DTE_NEXT)
  colnames(tmp)   <- names(DTE.DF)
  DTE.DF          <- rbind(tmp,DTE.DF)
  rownames(DTE.DF)<- c("days2Exp","mins2Exp")
  DTE.DF
}
# Get the DTE in minutes
DTE.DF                 <- DTE_YR_FRAC(OPTIONZ)
# CALCULATE THE TREAS SPLINES FOR THE DTEs ********************************* ####
T_YIELDS_SPLINE_INTERPOLATION <- function (YEAR){
  # Make a function to pull the yields
  read_a_year <- function(y){
    url_stub <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=XXXX"
    url <- gsub("XXXX", y, url_stub)
    # Read in the Yields page
    yield_page <- read_html(url)
    yield_data <- as_tibble(html_table(yield_page, fill = TRUE)[[2]] )
    # Return the Data
    return(yield_data)
  }
  # bring in the data one year at a time.
  y = YEAR
  seq.seq <- YEAR:YEAR
  yields_l <- pblapply(seq.seq, read_a_year)
  diff.diff <- (last(seq.seq) - first(seq.seq)) %>% as.numeric()
  # ROW BIND 
  RATES <- do.call("rbind", yields_l) %>%
    mutate(Date = as.Date(Date, format = c("%m/%d/%y"))) %>% column_to_rownames(var="Date") %>% data.frame()
  names(RATES) <- gsub("X","",names(RATES))
  # Get vector of Listed Treasuries Maturities
  t <- c(1/12, 1/6, 0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
  # Get Last Row for most current Maturities
  y <- RATES[nrow(RATES),] %>% as.vector()
  # Cubic Spline the Yields to the 
  spl <- smooth.spline(y ~ t)
  t.near <- seq(from = (DTE.DF[1,1]/365)*.5, to = 30, by = (DTE.DF[1,1]/365)*.5)
  t.next <- seq(from = (DTE.DF[1,2]/365)*.5, to = 30, by = (DTE.DF[1,2]/365)*.5)
  # Combine Predicitions into DF
  SPLINE.NEAR <- predict(spl, t.near) %>% data.frame
  SPLINE.NEXT <- predict(spl, t.next) %>% data.frame
  # Get Interpolated Treasuries
  t.near <- DTE.DF[1,1]
  t.next <- DTE.DF[1,2]
  # Find where the difference is minimal 
  TREAS_NEAR <- (SPLINE.NEAR[which.min(abs(SPLINE.NEAR$x - t.near/365)),]$y %>% as.numeric())/100
  TREAS_NEXT <- (SPLINE.NEXT[which.min(abs(SPLINE.NEXT$x - t.next/365)),]$y %>% as.numeric())/100
  # Combine into a List
  TREASURIES <- list(TREAS_NEAR,TREAS_NEXT)
  TREASURIES
}
YEAR                          = format(Sys.Date(),"%Y") %>% as.numeric()
TREASURIES                    <- T_YIELDS_SPLINE_INTERPOLATION(YEAR)
TREASURIES                    <- do.call(rbind,TREASURIES) %>% data.frame() %>% t() %>% data.frame()
colnames(TREASURIES)          <- names(DTE.DF)
rownames(TREASURIES)          <- "TREAS"
# Clean up and Combine 
DTE.DF                        <- rbind(DTE.DF,TREASURIES); rm(TREASURIES)
# mins2Exp
OPTIONZ_NEAR_CALLS$mins2Exp   <- replicate(nrow(OPTIONZ_NEAR_CALLS),DTE.DF[2,1])
OPTIONZ_NEAR_PUTS$mins2Exp    <- OPTIONZ_NEAR_CALLS$mins2Exp
OPTIONZ_NEXT_CALLS$mins2Exp   <- replicate(nrow(OPTIONZ_NEXT_CALLS),DTE.DF[2,2])
OPTIONZ_NEXT_PUTS$mins2Exp    <- OPTIONZ_NEXT_CALLS$mins2Exp
# TREAS
OPTIONZ_NEAR_CALLS$TREAS      <- replicate(nrow(OPTIONZ_NEAR_CALLS),DTE.DF[3,1])
OPTIONZ_NEAR_PUTS$TREAS       <- replicate(nrow(OPTIONZ_NEAR_PUTS), DTE.DF[3,1])
OPTIONZ_NEXT_CALLS$TREAS      <- replicate(nrow(OPTIONZ_NEXT_CALLS),DTE.DF[3,2])
OPTIONZ_NEXT_PUTS$TREAS       <- replicate(nrow(OPTIONZ_NEXT_PUTS), DTE.DF[3,2])
# DETERMINE THE FORWARD INDEX LEVEL **************************************** ####
get_K_KNOT_NEAR <- function (OPTIONZ_NEAR_CALLS,OPTIONZ_NEAR_PUTS){
  # Subset for Similiar Strikes
  OPTIONZ_NEAR_CALLS <- OPTIONZ_NEAR_CALLS[OPTIONZ_NEAR_CALLS$strike %in% OPTIONZ_NEAR_PUTS$strike,]
  OPTIONZ_NEAR_PUTS  <- OPTIONZ_NEAR_PUTS[OPTIONZ_NEAR_PUTS$strike %in% OPTIONZ_NEAR_CALLS$strike,]
  OPTIONZ_NEAR_PUTS  <- OPTIONZ_NEAR_PUTS[order(OPTIONZ_NEAR_PUTS$strike),]
  # Subset the Calls and Rename
  OPTIONZ_NEAR_CALLS <- OPTIONZ_NEAR_CALLS %>% dplyr::select(strike,Mid)
  colnames(OPTIONZ_NEAR_CALLS)[colnames(OPTIONZ_NEAR_CALLS) == "Mid"] <- "Call_Mid"
  # Subset the Puts and Rename
  OPTIONZ_NEAR_PUTS <- OPTIONZ_NEAR_PUTS %>% dplyr::select(strike,Mid)
  colnames(OPTIONZ_NEAR_PUTS)[colnames(OPTIONZ_NEAR_PUTS) == "Mid"] <- "Put_Mid"
  # Bind the Columns
  OPTIONZ_NEAR <- cbind(
    OPTIONZ_NEAR_CALLS$strike,
    OPTIONZ_NEAR_CALLS$Call_Mid,
    OPTIONZ_NEAR_PUTS$Put_Mid) %>% data.frame()
  colnames(OPTIONZ_NEAR) <- c("strike","Call_Mid","Put_Mid")
  # Calculate the Differences
  OPTIONZ_NEAR$Difff <- abs(as.numeric(OPTIONZ_NEAR$Call_Mid) - as.numeric(OPTIONZ_NEAR$Put_Mid)) 
  # Find Minimal Difference
  OPTIONZ_NEAR <- OPTIONZ_NEAR[OPTIONZ_NEAR$Difff == min(OPTIONZ_NEAR$Difff),]
  K_KNOT_NEAR  <- OPTIONZ_NEAR$strike
  K_KNOT_NEAR
  
}
get_K_KNOT_NEXT <- function (OPTIONZ_NEXT_CALLS,OPTIONZ_NEXT_PUTS){
  # Subset for Similiar Strikes
  OPTIONZ_NEXT_CALLS <- OPTIONZ_NEXT_CALLS[OPTIONZ_NEXT_CALLS$strike %in% OPTIONZ_NEXT_PUTS$strike,]
  OPTIONZ_NEXT_PUTS  <- OPTIONZ_NEXT_PUTS[OPTIONZ_NEXT_PUTS$strike %in% OPTIONZ_NEXT_CALLS$strike,]
  OPTIONZ_NEXT_PUTS  <- OPTIONZ_NEXT_PUTS[order(OPTIONZ_NEXT_PUTS$strike),]
  # Subset the Calls and Rename
  OPTIONZ_NEXT_CALLS <- OPTIONZ_NEXT_CALLS %>% dplyr::select(strike,Mid)
  colnames(OPTIONZ_NEXT_CALLS)[colnames(OPTIONZ_NEXT_CALLS) == "Mid"] <- "Call_Mid"
  # Subset the Puts and Rename
  OPTIONZ_NEXT_PUTS <- OPTIONZ_NEXT_PUTS %>% dplyr::select(strike,Mid)
  colnames(OPTIONZ_NEXT_PUTS)[colnames(OPTIONZ_NEXT_PUTS) == "Mid"] <- "Put_Mid"
  # Bind the Columns
  OPTIONZ_NEXT <- cbind(
    OPTIONZ_NEXT_CALLS$strike,
    OPTIONZ_NEXT_CALLS$Call_Mid,
    OPTIONZ_NEXT_PUTS$Put_Mid) %>% data.frame()
  colnames(OPTIONZ_NEXT) <- c("strike","Call_Mid","Put_Mid")
  # Calculate the Differences
  OPTIONZ_NEXT$Difff <- abs(as.numeric(OPTIONZ_NEXT$Call_Mid) - as.numeric(OPTIONZ_NEXT$Put_Mid)) 
  # Find Minimal Difference
  OPTIONZ_NEXT <- OPTIONZ_NEXT[OPTIONZ_NEXT$Difff == min(OPTIONZ_NEXT$Difff),]
  K_KNOT_NEXT  <- OPTIONZ_NEXT$strike
  K_KNOT_NEXT
  
}
# Get The Forward Strikes
K_KNOT_NEAR <- {
  get_K_KNOT_NEAR(
    OPTIONZ_NEAR_CALLS,
    OPTIONZ_NEAR_PUTS
  )
}
K_KNOT_NEXT <- {
  get_K_KNOT_NEXT(
    OPTIONZ_NEXT_CALLS,
    OPTIONZ_NEXT_PUTS
  )
}
# Make a function to generate the forward prices
get_FW_LEVEL <- function(CHAIN_1,CHAIN_2,K){
  CHAIN_1  = CHAIN_1
  CHAIN_2  = CHAIN_2
  K        = K
  TREAS    = CHAIN_1$TREAS[1]
  DTE      = CHAIN_1$mins2Exp[1]
  CALL_MID = CHAIN_1[CHAIN_1$strike == K,]$Mid
  PUT_MID  = CHAIN_2[CHAIN_2$strike == K,]$Mid
  # Write the Equation
  FW_K     = K + exp(TREAS*DTE)*(CALL_MID-PUT_MID)
  FW_K
}
FW_LEVEL_NEAR      <- get_FW_LEVEL(OPTIONZ_NEAR_CALLS,OPTIONZ_NEAR_PUTS,K_KNOT_NEAR)
FW_LEVEL_NEXT      <- get_FW_LEVEL(OPTIONZ_NEXT_CALLS,OPTIONZ_NEXT_PUTS,K_KNOT_NEXT)
# Get the Forward Strikes
FW_K               <- function(OPTIONZ_NEAR_CALLS,OPTIONZ_NEXT_CALLS,FW_LEVEL_NEAR,FW_LEVEL_NEXT){
  # Get the Forward Strike of the Near Terms aka K_0,1 & K_0,2 from the paper
  FW_K_NEAR <- OPTIONZ_NEAR_CALLS[OPTIONZ_NEAR_CALLS$strike <  FW_LEVEL_NEAR,]
  FW_K_NEAR <- FW_K_NEAR[which.min(abs(FW_LEVEL_NEAR - FW_K_NEAR$strike)),]$strike
  # Get the Forward Strike of the Next Terms
  FW_K_NEXT <- OPTIONZ_NEXT_CALLS[OPTIONZ_NEXT_CALLS$strike <  FW_LEVEL_NEXT,]
  FW_K_NEXT <- FW_K_NEXT[which.min(abs(FW_LEVEL_NEXT - FW_K_NEXT$strike)),]$strike
  # Return a Data Frame
  FW_K      <- data.frame(FW_K_NEAR,FW_K_NEXT)
  rownames(FW_K) <- "FW_STRIKE"
  FW_K
}
FW_K_DF            <- FW_K(OPTIONZ_NEAR_CALLS,OPTIONZ_NEXT_CALLS,FW_LEVEL_NEAR,FW_LEVEL_NEXT)
# FIND ANY CONSECUTIVE PUTS AND CALLS AND REMOVE THEM ********************** ####
REMOVE_CONSEC_ZERO_BIDS <- function (CHAIN){
  # Order the Chain and Get Strictly OTM PUTS
  CHAIN <- CHAIN[CHAIN$Moneyness == "OTM",]
  # How to Order the Chain?
  decreasing <- if (CHAIN$flag[1] == "P"){FALSE}else{TRUE}
  CHAIN <- CHAIN[order(CHAIN$strike,decreasing = decreasing),]
  # Remove the Options where the Difference is equal to exactly 0
  CHAIN <- CHAIN[diff(as.numeric(CHAIN$bid)) != 0, ]
  # The function leaves a remainder of the top row with bid == 0, but remove that too
  CHAIN <- CHAIN[-1, ]
  # How to Order the Chain?
  decreasing <- FALSE
  CHAIN <- CHAIN[order(CHAIN$strike,decreasing = decreasing),]
  # Return the Data Frame
  return(CHAIN)
}
# Remove the Options with bids == 0
OPTIONZ_NEAR_PUTS  <- OPTIONZ_NEAR_PUTS  %>% REMOVE_CONSEC_ZERO_BIDS()
OPTIONZ_NEAR_CALLS <- OPTIONZ_NEAR_CALLS %>% REMOVE_CONSEC_ZERO_BIDS()
OPTIONZ_NEXT_CALLS <- OPTIONZ_NEXT_CALLS %>% REMOVE_CONSEC_ZERO_BIDS()
OPTIONZ_NEXT_PUTS  <- OPTIONZ_NEXT_PUTS  %>% REMOVE_CONSEC_ZERO_BIDS()
# Compute the Average of the Put and Call Mid Prices
# Near Term Row
MID_NEAR_CHAIN_ROW <- function(OPTIONZ,FW_K_DF){
  NEAR_CALL_MID <-{
    OPTIONZ[OPTIONZ$strike == FW_K_DF$FW_K_NEAR[1] 
            & OPTIONZ$days2Exp == DTE.DF$DTE_NEAR[1]
            & OPTIONZ$flag == "C",] %>% dplyr::select(strike,flag,Mid)
  }
  NEAR_PUT_MID <-{
    OPTIONZ[OPTIONZ$strike == FW_K_DF$FW_K_NEAR[1] 
            & OPTIONZ$days2Exp == DTE.DF$DTE_NEAR[1]
            & OPTIONZ$flag == "P",] %>% dplyr::select(strike,flag,Mid)
  }
  NEAR_STRIKE_AVE <- (NEAR_CALL_MID$strike + NEAR_PUT_MID$strike)/2
  NEAR_MID_AVE    <- (NEAR_CALL_MID$Mid + NEAR_PUT_MID$Mid)/2
  tmp_row         <- data.frame(NEAR_STRIKE_AVE,"Put/Call_Average",NEAR_MID_AVE,"ATM")
  names(tmp_row)  <- c("strike","flag","Mid","Moneyness")
  tmp_row
}
MID_NEAR_ROW <- MID_NEAR_CHAIN_ROW(OPTIONZ,FW_K_DF)
# Next Term Row
MID_NEXT_CHAIN_ROW <- function(OPTIONZ,FW_K_DF){
  NEXT_CALL_MID <-{
    OPTIONZ[OPTIONZ$strike == FW_K_DF$FW_K_NEXT[1] 
            & OPTIONZ$days2Exp == DTE.DF$DTE_NEXT[1]
            & OPTIONZ$flag == "C",] %>% dplyr::select(strike,flag,Mid)
  }
  NEXT_PUT_MID <-{
    OPTIONZ[OPTIONZ$strike == FW_K_DF$FW_K_NEXT[1] 
            & OPTIONZ$days2Exp == DTE.DF$DTE_NEXT[1]
            & OPTIONZ$flag == "P",] %>% dplyr::select(strike,flag,Mid)
  }
  NEXT_STRIKE_AVE <- (NEXT_CALL_MID$strike + NEXT_PUT_MID$strike)/2
  NEXT_MID_AVE    <- (NEXT_CALL_MID$Mid + NEXT_PUT_MID$Mid)/2
  tmp_row         <- data.frame(NEXT_STRIKE_AVE,"Put/Call_Average",NEXT_MID_AVE,"ATM")
  names(tmp_row)  <- c("strike","flag","Mid","Moneyness")
  tmp_row
}
MID_NEXT_ROW <- MID_NEXT_CHAIN_ROW(OPTIONZ,FW_K_DF)
# Bind up the Rows for the Near
OPTIONZ_NEAR <- rbind(OPTIONZ_NEAR_CALLS,OPTIONZ_NEAR_PUTS) %>% 
  dplyr::select(strike,flag,Mid,Moneyness) %>% data.frame()
OPTIONZ_NEAR <- rbind(OPTIONZ_NEAR,MID_NEAR_ROW); colnames(OPTIONZ_NEAR) <- colnames(OPTIONZ_NEAR) 
OPTIONZ_NEAR <- OPTIONZ_NEAR[order(OPTIONZ_NEAR$strike),]
# Bind up the Rows for the Next
OPTIONZ_NEXT <- rbind(OPTIONZ_NEXT_CALLS,OPTIONZ_NEXT_PUTS) %>% 
  dplyr::select(strike,flag,Mid,Moneyness) %>% data.frame()
OPTIONZ_NEXT <- rbind(OPTIONZ_NEXT,MID_NEXT_ROW); colnames(OPTIONZ_NEXT) <- colnames(OPTIONZ_NEXT) 
OPTIONZ_NEXT <- OPTIONZ_NEXT[order(OPTIONZ_NEXT$strike),]
# TIME TO CALCULATE THE CONTRIBUTION BY STRIKE ***************************** ####
CONTRIB_NEAR_FUN <- function(opt,DTE.DF){
  # Define a List
  CONTRIB = list()
  # Define the Treasury
  R_      = DTE.DF$DTE_NEAR[3]
  # Define the DTE in yr frac
  T_      = DTE.DF$DTE_NEAR[2]
  # Generate a for loop to computer contribution by strike
  for (i in 1:nrow(opt)){
    DELTA_K      = opt$strike[i+1] - opt$strike[i]
    K_SQUARED    = (opt$strike[i])^2
    EXPON        = exp(R_*T_)
    Q_FUN        = opt$Mid[i]
    CONTRIB[[i]] = (DELTA_K/K_SQUARED)*EXPON*Q_FUN
  }
  CONTRIB <- data.frame(do.call(rbind,CONTRIB)); colnames(CONTRIB) <-"CONTRIB"
  opt     <- cbind(opt,CONTRIB)
  opt     <- na.omit(opt)
  opt     <- (sum(opt$CONTRIB))*(2/T_)
  opt
}
CONTRIB_NEXT_FUN <- function(opt,DTE.DF){
  # Define a List
  CONTRIB = list()
  # Define the Treasury
  R_      = DTE.DF$DTE_NEXT[3]
  # Define the DTE in yr frac
  T_      = DTE.DF$DTE_NEXT[2]
  # Generate a for loop to computer contribution by strike
  for (i in 1:nrow(opt)){
    DELTA_K      = opt$strike[i+1] - opt$strike[i]
    K_SQUARED    = (opt$strike[i])^2
    EXPON        = exp(R_*T_)
    Q_FUN        = opt$Mid[i]
    CONTRIB[[i]] = (DELTA_K/K_SQUARED)*EXPON*Q_FUN
  }
  CONTRIB <- data.frame(do.call(rbind,CONTRIB)); colnames(CONTRIB) <-"CONTRIB"
  opt     <- cbind(opt,CONTRIB)
  opt     <- na.omit(opt)
  opt     <- (sum(opt$CONTRIB))*(2/T_)
  opt
}
CONTRIB_NEAR <- CONTRIB_NEAR_FUN(OPTIONZ_NEAR,DTE.DF); CONTRIB_NEAR
CONTRIB_NEXT <- CONTRIB_NEXT_FUN(OPTIONZ_NEXT,DTE.DF); CONTRIB_NEXT
# CALCULATE THE MEAN SQUARED ERROR ***************************************** ####
# Near
MSE_NEAR     = ( ( (FW_LEVEL_NEAR/FW_K_DF$FW_K_NEAR) - 1 )^2 )/ DTE.DF$DTE_NEAR[3]; MSE_NEAR
# Next
MSE_NEXT     = ( ( (FW_LEVEL_NEXT/FW_K_DF$FW_K_NEXT) - 1 )^2 )/ DTE.DF$DTE_NEXT[3]; MSE_NEXT
# STEP 3: CALCULATE THE VIX ************************************************ ####
# Calculate the Variances First
VAR_NEAR     = CONTRIB_NEAR - MSE_NEAR; VAR_NEAR
VAR_NEXT     = CONTRIB_NEXT - MSE_NEXT; VAR_NEXT
# Get the current Times 
NT___1  = DTE.DF$DTE_NEAR[2]*525600
NT___2  = DTE.DF$DTE_NEXT[2]*525600
NT__30  = 43200 
NT_365  = 525600
# Calculate the VIX
A       = ((DTE.DF$DTE_NEAR[2]*VAR_NEAR)*((NT___2 - NT__30)/(NT___2 - NT___1)))
B       = ((DTE.DF$DTE_NEXT[2]*VAR_NEXT)*((NT__30 - NT___1)/(NT___2 - NT___1)))
IV_SPOT = (sqrt((A+B)*(NT_365/NT__30)))*100; IV_SPOT
#############################################################################
# Finish
end.time = Sys.time()
Finish_Line = end.time - start_time; Finish_Line



