
 
so <- 'mac'
if (so == 'mac'){
  main_path <- "/Users/Diosdenombrecarlos/Desktop/TFG"
}
wkdir <- paste (main_path, "/programs", sep='') 
setwd (wkdir)
path_airbase_o3   <- paste (main_path, '/data/mda8_o3_processed_final/',     sep='')
path_ncep_ncar    <- paste (main_path, '/data/ncep_ncar_interpol/',      sep='')
path_out          <- paste (main_path, '/output/', sep='')

source ( paste (main_path, "/programs/dropAll.R", sep='') )
  
p <- 0.00000001	#Prob (coefs different from zero) = 99.999999%
  
  ii = 1  
  prefix <- c ("IT1247A")
  suffix <- c ("_o3_dymax")  #  "_pm10_day" 
  
  if (suffix [ii] == "_o3_dymax") {file <- paste (path_airbase_o3,   prefix[ii], 
                                                  suffix[ii], '_1999_2012.txt', sep='')}
                                                  
  fileheader <- as.matrix ( read.table (file, nrows=1) )  # read header
  filein     <- as.matrix ( read.table (file, skip =1) )  # read data
  
  pyyyy <- as.numeric (filein [,which (fileheader == "yyyy")])
  pmm   <- as.numeric (filein [,which (fileheader == "mm")])
  pdd   <- as.numeric (filein [,which (fileheader == "dd")])
    
  if (suffix [ii] == "_o3_dymax") { o3  <- as.numeric (filein [,which (fileheader == "o3(ugm-3)")])}

  for (jj in 1:16) {
    if (jj == 1) { field <- 'tmax.2m.gauss'  }
    if (jj == 2) { field <- 'uwnd.10m.gauss' }
    if (jj == 3) { field <- 'vwnd.10m.gauss' }
    if (jj == 4) { field <- 'dswrf.sfc.gauss' }
    if (jj == 5) { field <- 'rhum.sig995' }
    if (jj == 6) { field <- 'slp' }
    if (jj == 7) { field <- 'air.2m.gauss'}
    if (jj == 8) { field <- 'cprat.sfc.gauss'}
    if (jj == 9) { field <- 'dlwrf.sfc.gauss'}
    if (jj == 10) { field <- 'hgt'}
    if (jj == 11) { field <- 'lftx.sfc'}
    if (jj == 12) { field <- 'prate.sfc.gauss'}
    if (jj == 13) { field <- 'shum.2m.gauss'}
    if (jj == 14) { field <- 'tcdc.eatm.gauss'}
    if (jj == 15) { field <- 'uwnd'}
    if (jj == 16) { field <- 'vwnd'}
    
    file_meteo <- paste (path_ncep_ncar, prefix [ii], suffix[ii], '_', 
                         field, '.1998.2012.txt', sep='')

    fileheader <- as.matrix ( read.table (file_meteo, nrows=1) )  # read header
    filein     <- as.matrix ( read.table (file_meteo, skip =1) )  # read data

    # 'r' here is for reanalysis
    ryyyy <- as.numeric (filein [,which (fileheader == "yyyy")])
    rmm   <- as.numeric (filein [,which (fileheader == "mm")])
    rdd   <- as.numeric (filein [,which (fileheader == "dd")])

    if (jj == 1) { tmax  <- as.numeric (filein [,which (fileheader == "tmax(degC)")]) }
    if (jj == 2) { uwnd  <- as.numeric (filein [,which (fileheader == "uwnd(m/s)")]) }
    if (jj == 3) { vwnd  <- as.numeric (filein [,which (fileheader == "vwnd(m/s)")]) }
    if (jj == 4) { dswrf <- as.numeric (filein [,which (fileheader == "dswrf(W/m^2)")]) }
    if (jj == 5) { rhum  <- as.numeric (filein [,which (fileheader == "rhum(%)")]) }
    if (jj == 6) { slp   <- as.numeric (filein [,which (fileheader == "slp(hPa)")]) }
    if (jj == 7) { air   <- as.numeric (filein [,which (fileheader == "air(degC)")]) }
    if (jj == 8) { cprat   <- as.numeric (filein [,which (fileheader == "cprat(Kg/m^2/s)")]) }
    if (jj == 9) { dlwrf   <- as.numeric (filein [,which (fileheader == "dlwrf(W/m^2)")]) }
    if (jj == 10) { hgt   <- as.numeric (filein [,which (fileheader == "hgt(dam)")]) }
    if (jj == 11) { lftx   <- as.numeric (filein [,which (fileheader == "lftx(degK)")]) }
    if (jj == 12) { prate   <- as.numeric (filein [,which (fileheader == "prate(Kg/m^2/s)")]) }
    if (jj == 13) { shum   <- as.numeric (filein [,which (fileheader == "shum(g/kg)")]) }
    if (jj == 14) { tcdc   <- as.numeric (filein [,which (fileheader == "tcdc(%)")]) }
    if (jj == 15) { uhwnd   <- as.numeric (filein [,which (fileheader == "uwnd(m/s)")]) }
    if (jj == 16) { vhwnd   <- as.numeric (filein [,which (fileheader == "vwnd(m/s)")]) }
  }
  
  print (length (pyyyy)) ; print (length (pmm))  # 7671 7671
  print (length (ryyyy)) ; print (length (rmm))  # 5479 5479  
  
  psummer <- (pyyyy >= 1999 & pyyyy <= 2012) & (pmm == 6 | pmm == 7 | pmm == 8)
  rsummer <- (ryyyy >= 1999 & ryyyy <= 2012) & (rmm == 6 | rmm == 7 | rmm == 8)

  print (length (psummer) ) # 7671
  print (length (rsummer) ) # 5479
  
  if (suffix [ii] == "_o3_dymax") {pollutant <- o3   [psummer]} 
 
  temp <- tmax        [rsummer]
  u        <- uwnd    [rsummer]
  v        <- vwnd    [rsummer]
  swf      <- dswrf   [rsummer]
  rh       <- rhum    [rsummer]
  press    <- slp     [rsummer]
  tdaily   <- air     [rsummer]
  convprep <- cprat   [rsummer]
  lwf      <- dlwrf   [rsummer]
  geop     <- hgt     [rsummer]
  surlift  <- lftx    [rsummer]
  prep     <- prate   [rsummer]
  q        <- shum    [rsummer]
  cloud    <- tcdc    [rsummer]
  u850     <- uhwnd   [rsummer]
  v850     <- uhwnd   [rsummer]
  
  
  
  
  
  # Checking dimensions are the same
  if ( length(temp) != length(pollutant) ) {
    print ( length (pollutant) )
    print ( length (temp) )
 
    stop ("pollutant & meteo fields have different length")
  }   
  # selection of year and months
  yyyy  <- pyyyy [psummer]
  mm    <- pmm   [psummer]
  
  
  
  ########################################################################
  # All this is not needed
  
  # Checking that variables have same dimensions
  length (yyyy)      # 1380
  length (pollutant) # 1380
  length (temp)      # 1380
  length (u)         # 1380
 
  # Have a quick look at the daily O3 & T values per each year
 # plot (yyyy, pollutant)
  #plot (yyyy, temp)

  # Print those daily values for some of the years in the period
  pollutant [yyyy == 1998]
  pollutant [yyyy == 2003]
  pollutant [yyyy == 2012]
  

  # One could get some information about the pollutant
 # min (pollutant, na.rm=T); mean (pollutant, na.rm=T); max (pollutant, na.rm=T)
  
  # Having a look at the distribution of the pollutant field. Note that you
  # can try to predict log (pollutant) instead of pollutant. It might
  # give better results for PM, but probably not for O3.
 # hist (pollutant,       freq=T)
#  hist (log (pollutant), freq=T)
  # help (hist)
  
  
  #############################################################################
  # I know that T-squared might be a better predictor than T in the case of O3,
  # so I create an additional array with T-squared
  T2          <- vector ( length = length(temp) )
  
  negatT      <- (temp <  0)
  positT      <- (temp >= 0)
  
  T2 [negatT] <- -temp [negatT]^2
  T2 [positT] <-  temp [positT]^2
  
  # Checking scatter plots of O3 vs T & T2
  plot (temp, pollutant)
  plot (T2,   pollutant)
 # Otras variables no lineales
  lswf=log(swf)

  #############################################################################
  # Note that I will just put u & v components in the model, but one could
  # try wind speed and direction instead.

  # The calculation of windir given here is not right!!
  wspeed <- sqrt (u**2 + v**2)
  wdir   <- atan (u/v) * 180/pi

  negat  <- (wdir < 180)
  posit  <- (wdir > 180)
  
  wdir [negat] <-  wdir [negat] + 180
  wdir [posit] <-  wdir [posit] - 180
  

  
  #############################################################################
  # Linear model of the pollutant concentrations as a function of
  # the meteo parameters, either using T or T2.
  
  # First build separate linear models on temp and T2
  m1 <- lm (pollutant ~ temp) 
  m2 <- lm (pollutant ~ T2)

  s1 <- summary (m1)
  s2 <- summary (m2)

  # Create multiple linear model on T (or T2) and the rest of the meteo fields
 # if (s1$r.squared >= s2$r.squared) {
  #    model0 <- lm (pollutant ~ temp + swf + u + v + rh + press )
  #} else {
  #    model0 <- lm (pollutant ~ T2   + swf + u + v + rh + press )
  #}
  
  # Modelo a falta de introducir dos variables roc convprep 0.5771
   #model0 <- lm (pollutant ~ temp + swf + u + v + rh + press +q + prep + tdaily + geop + surlift + cloud + u850 + v850)
  #model0 <- lm (pollutant ~ temp + T2 )
  #model0 <- lm (pollutant ~ T2 + geop )
 # model0 <- lm (pollutant ~ geop + rh + surlift + swf + cloud)
  model0 <- lm (pollutant ~ temp + swf + u + v + rh + press + q + prep + tdaily + geop + surlift + cloud + convprep + lwf)
  #model0 <- lm (pollutant ~ temp + swf + u + v + rh + press + q + prep + tdaily + geop  + cloud + convprep + lwf)
  
  # Print basic results
  model0
  
  # Print a summary. Have a look at R-quared and signifance of the coefficients.
  sum0 = summary (model0)
  
  ##########################################################################
  # Calling the function dropAll that sequentially removes all variables.
  # It removes step by step the variable with highest probability that its 
  # coefficient is zero. This probability is based on the values of
  # the statistics Akaike Information Criterion (AIC).
  # Choose "full = T" in dropAll.
  dr      <- dropAll(model0) 
  
  print (dr)
  
  print (dr$ranking)
  
  
  # List of variables and their ranking  
  rank.vars <- dr$ranking["rank"]
  vars      <- rownames (rank.vars)
  order     <- rank.vars$rank  

  nvars     <- length (vars)  # number of variables
   
   
  ##########################################################################
  # Remove the least significant variables to create a simpler model
  
  # It should be OK with just checking the 10 best variables instead of all of them,
  # because we intend to have a short model in the end
  #dr.probs <- cbind (dr$df10[,6], dr$df9[,6], dr$df8[,6], dr$df7[,6], dr$df6[,6], 
  #                   dr$df5 [,6], dr$df4[,6], dr$df3[,6], dr$df2[,6], dr$df1[,6])
  
  # But we have tried only 6 here
 dr.probs <- cbind ( dr$df14[,6], dr$df13[,6], dr$df12[,6], dr$df11[,6], dr$df10[,6], dr$df9[,6], dr$df8[,6], dr$df7[,6], dr$df6[,6], dr$df5[,6], dr$df4[,6], 
                     dr$df3[,6], dr$df2[,6], dr$df1[,6])
  #dr.probs <- cbind (  dr$df13[,6], dr$df12[,6], dr$df11[,6], dr$df10[,6], dr$df9[,6], dr$df8[,6], dr$df7[,6], dr$df6[,6], dr$df5[,6], dr$df4[,6], 
   #                   dr$df3[,6], dr$df2[,6], dr$df1[,6])
  
    print(dr.probs)
  
  # probabilities of variables in the model
  max.probs <- vector (length=nvars)

  for (kk in 1:nvars) { 
    max.probs[kk] <- max(dr.probs[,kk], na.rm=T)
  }

  
  #Keep only the variables with probability that their
  #coefficient is different from zero higher than a
  #certain value given by the stopping criterion
  #Prob (coefs different from zero) = (1 - p) * 100 (%)
  #
  # This gives the number of variables to remove. Those are the variables
  # with highets proabability that their coefficient is zero.  
  number2remove <- length(max.probs[max.probs > p])

  # number of vars. to keep in the final model:
  limit <- nvars - number2remove

   
  # Final model in which we keep only a few variables 
  vars2remove <- vars [order > limit]
  remove      <- paste ('. ~ . ',paste(paste('-',vars2remove, sep=''),collapse=' '))

  modelf      <- update  (model0, as.formula(remove))
  sumf        <- summary (modelf)
  
  
  #---------------------------------------------------
  # Coefics, ndf and explained variance of the model,
  # predictors, ...
  # All this will be print out to output file
  
  coeffs  <- sumf$coef
  ndf     <- modelf$df.residual
  explVar <- sumf$adj.r.squared
 
  predictors <- rownames (attr(modelf$terms,"factors")) # names of predictors
  lp         <- length   (predictors) - 1               # number of predictors

 
 
  #---------------------------------------------------
  # Write results to output file
  fileout <- paste (path_out, prefix[ii], suffix[ii], ".txt", sep='')
  
  write (" ", file= fileout, append=F) # deletes stuff in the file  
 
  write (paste(lp, " predictors:"), file= fileout, append=T)
  write (predictors, file= fileout, append=T)
  write (" ", file= fileout, append=T)
 
  write.table (round(coeffs, 4), file= fileout, append=T)
  write (" ", file= fileout, append=T)
  
  write (paste("explained variance(%): ", round(100*explVar,2)), file= fileout, append=T)
  write (paste("ndf: ", ndf), file= fileout, append=T)
  
  write (" ", file= fileout, append=T)

 

  #############################################################################
  # Plot predicted vs. observed values of the pollutant

  # Need to be carefull, because the model does not consider NANs
  length (pollutant)            #  1380
  length (modelf$fitted.values) #  1348

  yes.data  <- as.numeric(rownames(modelf$model))
  no.data   <- modelf$na.action

  predicted <- vector (length = length(yes.data) + length(no.data))

  predicted [yes.data] <- modelf$fitted.values
  predicted [no.data]  <- NaN
  
  plot (pollutant, predicted, cex=.5, xlab = 'measured')

  
    
  #####################################################################################
  # Checking plots of residuals to see how good the model is.
  
  # # Visualise results on screen  
   plot (modelf)
  
  
  # Save it to a png file
  nameplot <- paste (path_out, "resid_", prefix[ii], suffix[ii], ".png", sep='')
  png (file = nameplot, width = 1000, height = 1000, bg = "white")
  
  opar <- par()
  par  (mfcol = c(2, 2))
  plot (modelf, cex=.2)
  dev.off()
  

  # Save it to an eps file
  nameplot <- paste (path_out, "resid_", prefix[ii], suffix[ii], ".eps", sep='')
  postscript (nameplot, horizontal = FALSE, width = 6, height = 7, 
              pagecentre = FALSE, pointsize = 10)

  opar <- par()
  par  (mfcol = c(2, 2))
  plot (modelf, cex = 0.05)
  dev.off()
  
  
 
  #############################################################################
  # Calculation of meteorologically adjusted O3/PM10:
  #   O3 adjusted = O3 mean + (O3 - O3 predicted by the model)
  #
  # Then plot the yearly meadians of measured & adjusted concentrations
  
  avg  <- mean  (pollutant, na.rm = T)
  adj  <- avg + (pollutant - predicted)	
 
 
  #**********************************************************************
  # Calculating the median of the adjusted and measured pollutant
  # for each summer, nr. regs per year, trends, confidence intervals, 
  # and plotting the trends
  #**********************************************************************

  nyear       <- as.numeric(levels(factor(yyyy)))	;print(nyear)

  median.meas <- vector(mode ="numeric", length = length(nyear))
  median.adj  <- vector(mode ="numeric", length = length(nyear))

  nlong.meas  <- vector(mode ="numeric", length = length(nyear))
  nlong.adj   <- vector(mode ="numeric", length = length(nyear))

  nbad.meas   <- vector(mode ="numeric", length = length(nyear))
  nbad.adj    <- vector(mode ="numeric", length = length(nyear))

  ngood.meas  <- vector(mode ="numeric", length = length(nyear))
  ngood.adj   <- vector(mode ="numeric", length = length(nyear))

  #Meteo adjusted O3 & measured O3
  kk <- 0
  for (jj in min(yyyy): max(yyyy)) {
        # Select measured and adjusted data on that year
        sel.meas  <- pollutant [which (yyyy == jj)]
        sel.adj   <- adj       [which (yyyy == jj)]

        # Fill in arrays with yearly medians of the pollutant
        kk               <- kk+1  
        median.meas [kk] <- median (sel.meas, na.rm=T)
        median.adj  [kk] <- median (sel.adj,  na.rm=T)

        # Fill in arrays with number of days in the year-season
        nlong.meas  [kk] <- length (sel.meas)
        nlong.adj   [kk] <- length (sel.adj)

        # Look for missing data to get the
        # number of missing data points per year
        s.meas         <- summary (sel.meas)
        s.adj          <- summary (sel.adj)

        nbad.meas [kk] <- s.meas ["NA's"]
        nbad.adj  [kk] <- s.adj  ["NA's"]
  }  
  
  
  # The arrays with bad number of days show NA instead of 0
  # when no bad days where found in a year
  print (nbad.meas) 
  print (nbad.adj) 

  # Amend that
  amend.meas <- is.na(nbad.meas) == T
  amend.adj  <- is.na(nbad.adj)  == T

  nbad.meas [amend.meas] <- 0
  nbad.adj  [amend.adj]  <- 0

  # Now I can calculate the arrays with number of good data points per year
  ngood.meas  <- nlong.meas - nbad.meas
  ngood.adj   <- nlong.adj  - nbad.adj
  
  # One season in one year is around 90 days.
  # Use data only when there are data for at least 60 from those 90 days 
  useYear.meas <- which (ngood.meas >= 60)
  useYear.adj  <- which (ngood.adj  >= 60)

  median.meas	<- median.meas [useYear.meas]
  median.adj 	<- median.adj  [useYear.adj]

  nyear.meas 	<- nyear [useYear.meas]
  nyear.adj 	<- nyear [useYear.adj]
  
  # Model that we use to get the linear trend in O3
  # (the trend is the slope of the regression against the year)
  trendMod.meas <- lm (median.meas ~ nyear.meas)
  trendMod 	<- lm (median.adj  ~ nyear.adj)

  #Trend Plots
  if (suffix [ii] == "_o3_dymax") {
     nameplot  <- paste (path_out, prefix[ii], suffix[ii], "_trends.png",   sep='')
     ylab_meas <- "meas. ozone (ug m-3)"
     ylab_adj  <- "adj. ozone (ug m-3)"
  }

  if (suffix [ii] == "_pm10_day") {
     nameplot  <- paste (path_out, prefix[ii], suffix[ii], "_trends.png", sep='')
     ylab_meas <- "meas. PM10 (ug m-3)"
     ylab_adj  <- "adj.  PM10 (ug m-3)"
  }

  png (file = nameplot, width = 600, height = 300, bg = "white")
  par(mfcol = c(1, 2), mar = c(3, 3, 2, 2), mgp = c(1.5,0.5,0))

  xrange <- c (min(nyear.meas,  nyear.adj),      max(nyear.meas, nyear.adj))
  yrange <- c (min(median.meas, median.adj) - 2, max(median.meas, median.adj) + 2)


  plot (nyear.meas, median.meas, xlim = xrange, ylim = yrange, pch = 21, 
        col = "maroon", bg = "lightgreen", xlab = "year", ylab = ylab_meas, 
        cex = 1.5, cex.axis = 0.8, cex.lab = 0.9)

  abline(trendMod.meas, col = "maroon", lw = 1.5)
  title (paste (prefix[ii], "   Summer"))


  plot (nyear.adj, median.adj, xlim = xrange, ylim = yrange, pch = 21, 
        col = "maroon", bg = "lightgreen", xlab = "year", ylab = ylab_adj, 
        cex = 1.5, cex.axis = 0.8, cex.lab = 0.9)
  
  abline(trendMod, col = "maroon", lw = 1.5)
 
  dev.off()
  
#}
