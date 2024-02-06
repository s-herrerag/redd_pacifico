### Functions --------------

# For residualising the outcome variable:
ATThetero <- function(data,distance=5000, first_stage= NULL,yname="def_ha_acc", gname = "treat",wname = NULL, wtr = NULL, horizon = NULL,pretrends = NULL, cluster_var = NULL, second_stage = NULL) {
  #Calculate individual treatment effects for heterogeneity analyisis
  
  zz000weight <- zz000adj <- NULL
  lhs <- term <- NULL
  
  # Extract first stage vars from formula
  if (is.null(first_stage)) {
    first_stage <- glue::glue("0 | cluster + time")
  } else if (inherits(first_stage, "formula")) {
    first_stage <- as.character(first_stage)[[2]]
  }
  
  # Formula for fitting the first stage
  formula <- stats::as.formula(glue::glue("{yname} ~ {first_stage}"))
  
  # Get list of event_time
  event_time <- unique(data$rel_treat[is.finite(data$rel_treat)])
  
  # horizon/allhorizon options
  if (is.null(wtr)) {
    
    # event-study
    if (!is.null(horizon)) {
      # create event time weights
      
      # allhorizons
      if (all(horizon == TRUE)) horizon <- event_time
      
      # Create wtr of horizons
      wtr <- paste0("zz000wtr", event_time[event_time >= 0])
      
      # Generate one wtr dummy variable for each event time
      # data <- copy(as.data.frame(data)) %>% setDT()
      #  setDT(data)
      e = 0
      for (var in wtr) {
        data[,var] <- if_else(is.na(rel_treat), 0, 1 * (rel_treat == e))
      }
      
    } else {
      wtr <- "zz000wtrtreat"
      data[, wtr] <- 1 * data$treat_ind == 1
    }
  }
  
  # Weights specified or not
  if (is.null(wname)) {
    data[, "zz000weight"] <- 1 #All get weight of 1 assigned
  } else {
    data[, "zz000weight"] <- data[,wname]
  }
  
  # Estimate Y(0) using untreated observations
  first_stage_est <- fixest::feols(formula,
                                   se = "standard",
                                   data = data[data$treat_ind == 0, ],
                                   weights = ~zz000weight,
                                   warn = FALSE, notes = FALSE
  )
  
  # Residualize outcome variable(s)
  data[, "adj_out"] <- data[[yname]] - stats::predict(first_stage_est, newdata = data)

  return(data) 
}

# For bootstrapping standard errors:
lo.boot <- function(y="string.y", x="string.x", data, N=500, seq=NULL, cil=0.025, cih=0.975, span=0.75,
                    plot.boot=FALSE, plot.ci=FALSE, plot.o=FALSE, ...){ 
  
  #if seq is NULL then use equal interval along range of X data
  if(is.null(seq)){
    seq <- seq(min(data[x]), max(data[x]), length.out=30)
  }
  
  #store Y and X data
  data <- data[c(y, x)]
  
  #number of rows in data
  datasize <- dim(data)[1]
  
  #plot original data
  if(plot.o){
    plot(data[,2], data[,1])
  }#end if(plot.o)
  
  #create matrix to hold loess predictions
  predmat <- matrix(0, N, length(seq))
  
  #loop for bootstraps
  for(i in 1:N){
    
    #take sample of rows of length(X)
    xind <- sample(1:datasize, datasize, replace=T)
    #create X,Y data based on random sample
    x=data[xind,]
    #loess on random sample
    low <- loess(x[,1] ~ x[,2], span=span, ...)
    predmat[i,] <- predict(low, seq)     
    
    #plot bootstraps if called
    if(plot.boot){
      
      lines(seq, predmat[i,])     #Plot a sample bootstrap curve.   
      
    }#end if
    
  }#end for
  
  #dim confidence interval vectors
  cih.lo <- vector()
  cil.lo <- vector()
  warn <-vector()
  j <- 1
  
  #store CI data foreach prediction of seq
  for(i in 1:length(seq)){
    
    #check for missing values in the prdicted data
    if(any(is.na(predmat[,i]))){
      warn[j] <- i
      j <- j + 1
    }#end if
    
    cih.lo[i] <- quantile(predmat[,i], cih, na.rm=T)
    cil.lo[i] <- quantile(predmat[,i], cil, na.rm=T)
  }#end for
  
  #give warning if there were any NAs in the predicted data
  if(j > 1){
    warning("There were missing predicted values in column(s) ", warn, "\n", 
            "You may want to reduce the range of your sequence to avoid predictions at the tails", call.=F)
  }
  
  #plot pointwise bootstrap confidence intervals if called
  if(plot.ci){		
    lines(seq, cih.lo, col="red")
    lines(seq, cil.lo, col="red")
  }
  
  #store output
  out <- list(predmat=predmat, cil=cil.lo, cih=cih.lo)
  return(out)
}

### Call functions and plot variables ------------------
r3Hetero <- ATThetero(yname = outcome_hetero, data=o2aCircGFC)

ResidOut <- r3Hetero %>% 
  group_by(id) %>% 
  mutate(year_of_mining = treat+2000) %>%
  filter(!is.na(adj_out),rel_treat==5)

plots <- list()
for (covariate in c("s3aPAs","s3hAccess","s3iSuitability","year_of_mining")) {
  all_vars <- c("s3aPAs","s3hAccess","s3iSuitability","year_of_mining")
  xlab_title <- c("Distance to protected areas (m)","Travel time to city (h)","Agricultural Suitability Index score","Year of mine establishment")[which(all_vars==covariate)]
  
  sp = 0.75
  step = 1
  cov <- as.matrix(ResidOut[,covariate])
  #seq.sl <- seq(min(cov),max(cov),(max(cov)-min(cov))/100)
  seq.sl <- seq(min(cov), max(cov), length.out=30)
  low3 <- loess(ResidOut$adj_out~cov, span=sp)
  low3p <- predict(low3, seq.sl, se=TRUE)
  
  cboots <- lo.boot(y="adj_out",x=covariate,seq=seq.sl,data=as.data.frame(ResidOut),cil=0.05, cih=0.95)
  cild <- cboots$cil
  cihd <- cboots$cih
  
  # If you want analyitical confidence bands:
  
  #cild <- low3p$fit - low3p$se.fit * 1.96 
  #cihd <- low3p$fit + low3p$se.fit * 1.96 
  
  fit3 <- low3p$fit
  g <- data.frame(cild, cihd, fit3,seq.sl)
  p <- ggplot(g,aes(x=seq.sl)) +
    geom_ribbon(aes(ymin=cild, ymax=cihd), fill = "#3CBB75FF",alpha = 0.6) +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    theme_bw() +
    geom_line(aes(x=seq.sl, y=fit3),color="#0D0887FF") +
    xlab(xlab_title) +
    ylab("Additional forest loss (ha)")
  
  plots[[length(plots)+1]] <- p
}
plots[[1]] + ylim(-800, 1600) + plots[[2]] + ylim(-800, 1600) + plots[[3]] + ylim(-800, 1600) + plots[[4]] + ylim(-800, 1600) + plot_annotation(tag_levels = "a")


