# Functions

## groupwiseCFA.mi.sep and globalMI.mi.sep functions are the modified versions of 
## groupwiseCFA and globalMI functions that are part of the MIE R package. 
## See Rudnev M. (2018-2019) Measurement Invariance Explorer. 
## Retrieved from: https://github.com/MaksimRudnev/MIE


# CFA models

groupwiseCFA.mi.sep <- function(model, data,  ..., out = c("fit", "modification", "goodfit")) {
  fit.list <- lapply(data, function(y) 
    lapply(y, function(x)
      lavaan::cfa(model, data = x, ...)
    ))
  
  tb.countrywise <- lapply(fit.list, function(y) 
    lapply(y, function(x)
      data.frame(
        converged <- x@optim$converged, 
        CFI <- ifelse (x@optim$converged, fitMeasures(x)[c("cfi.scaled")],  NA),
        RMSEA <- ifelse (x@optim$converged, fitMeasures(x)["rmsea.scaled"], NA),
        SRMR <- ifelse (x@optim$converged, fitMeasures(x)["srmr"],NA),
        CHI.sq <- ifelse (x@optim$converged, fitMeasures(x)[c("chisq.scaled")],NA),
        Pvalue <- ifelse (x@optim$converged, fitMeasures(x)[c("pvalue.scaled")],NA),
        
        Low.Load <- ifelse (x@optim$converged, ifelse(any(inspect(x, what="std")$lambda < 0.3 & inspect(x, what="std")$lambda != 0), TRUE, FALSE), NA),
        Neg.Var <- ifelse (x@optim$converged, ifelse(any(inspect(x, what="std")$lambda > 1), TRUE, FALSE), NA),
        
        stringsAsFactors = F)))
  
  tb.countrywise1 <- lapply(tb.countrywise, function(x)
    Reduce("rbind", x))
  
  for (i in 1:length(tb.countrywise1)) {
    colnames(tb.countrywise1[[i]]) <- c("Conv", "CFI", "RMSEA", "SRMR", "CHI.sq", "Pvalue",
                                        "Low.Load", "Neg.Var")
  }
  
  tb.countrywise1
  
  if("fit" %in% out) { 
    
    b <- lapply(tb.countrywise1, function(x) data.frame(
      CFI <- ifelse(length(unique(round(x$CFI, 3))) > 1, 
                    paste0(
                      "[", round(min(x$CFI), 3), "; ", round(max(x$CFI), 3), "]"), round(min(x$CFI), 3)),
      RMSEA <- ifelse(length(unique(round(x$RMSEA, 3))) > 1, 
                      paste0(
                        "[", round(min(x$RMSEA), 3), "; ", round(max(x$RMSEA), 3), "]"), round(min(x$RMSEA), 3)),
      SRMR <- ifelse(length(unique(round(x$SRMR, 3))) > 1, 
                     paste0(
                       "[", round(min(x$SRMR), 3), "; ", round(max(x$SRMR), 3), "]"), round(min(x$SRMR), 3)),
      CHI.sq <- ifelse(length(unique(round(x$CHI.sq, 2))) > 1, 
                       paste0(
                         "[", round(min(x$CHI.sq), 2), "; ", round(max(x$CHI.sq), 2), "]"), round(min(x$CHI.sq), 2)),
      Pvalue <- ifelse(length(unique(round(x$Pvalue, 3))) > 1, 
                       paste0(
                         "[", round(min(x$Pvalue), 3), "; ", round(max(x$Pvalue), 3), "]"), round(min(x$Pvalue), 3)),
      Low.Load <- ifelse(any(x$Low.Load), "+", ""),
      Neg.Var <- ifelse(any(x$Neg.Var), "+", "")
      
    ))
    
    b <- Reduce("rbind", b)
    rownames(b) <- names(fit.list)
    
    colnames(b) <- c("CFI", "RMSEA", "SRMR", "CHI.sq", "Pvalue",
                     "Low.Load", "Neg.Var")
    b <- b[, c("CHI.sq", "Pvalue", "CFI", "RMSEA", "SRMR", "Low.Load", "Neg.Var")]
    
    
    df_to_viewer(b)
    return(b)
    
    
  }
  
  if("goodfit" %in% out) { 
    
    b <- lapply(tb.countrywise1, function(x) data.frame(
      CFI.min <- round(min(x$CFI), 3),
      RMSEA.max <- round(max(x$RMSEA), 3),
      SRMR.max <- round(max(x$SRMR), 3),
      
      CFI <- ifelse(length(unique(round(x$CFI, 3))) > 1, 
                    paste0(
                      "[", round(min(x$CFI), 3), "; ", round(max(x$CFI), 3), "]"), round(min(x$CFI), 3)),
      RMSEA <- ifelse(length(unique(round(x$RMSEA, 3))) > 1, 
                      paste0(
                        "[", round(min(x$RMSEA), 3), "; ", round(max(x$RMSEA), 3), "]"), round(max(x$RMSEA), 3)),
      SRMR <- ifelse(length(unique(round(x$SRMR, 3))) > 1, 
                     paste0(
                       "[", round(min(x$SRMR), 3), "; ", round(max(x$SRMR), 3), "]"), round(max(x$SRMR), 3)),
      CHI.sq <- ifelse(length(unique(round(x$CHI.sq, 2))) > 1, 
                       paste0(
                         "[", round(min(x$CHI.sq), 2), "; ", round(max(x$CHI.sq), 2), "]"), round(max(x$CHI.sq), 2)),
      Pvalue <- ifelse(length(unique(round(x$Pvalue, 3))) > 1, 
                       paste0(
                         "[", round(min(x$Pvalue), 3), "; ", round(max(x$Pvalue), 3), "]"), round(max(x$Pvalue), 3)),
      Low.Load <- ifelse(any(x$Low.Load), "+", ""),
      Neg.Var <- ifelse(any(x$Neg.Var), "+", "")
      
    ))
    
    b <- Reduce("rbind", b)
    rownames(b) <- names(fit.list)
    colnames(b) <- c("CFI.min", "RMSEA.max", "SRMR.max", "CFI", "RMSEA", "SRMR", "CHI.sq", "Pvalue",
                     "Low.Load", "Neg.Var")
    
    b <- b[!(b$Low.Load=="+"| b$Neg.Var=="+"),]
    b <- b[(b$CFI.min>=0.9 & b$RMSEA.max<=0.08 & b$SRMR.max<=0.08),]
    b <- b[rowSums(is.na(b)) != ncol(b),]
    
    b <- b[, c("CHI.sq", "Pvalue", "CFI", "RMSEA", "SRMR")]
    
    df_to_viewer(b)
    return(b)
    
  }
  
  
  if("modification" %in% out) { 
    
    b <- lapply(tb.countrywise1, function(x) data.frame(
      CFI.min <- round(min(x$CFI), 3),
      RMSEA.max <- round(max(x$RMSEA), 3),
      SRMR.max <- round(max(x$SRMR), 3),
      
      CFI <- ifelse(length(unique(round(x$CFI, 3))) > 1, 
                    paste0(
                      "[", round(min(x$CFI), 3), "; ", round(max(x$CFI), 3), "]"), round(min(x$CFI), 3)),
      RMSEA <- ifelse(length(unique(round(x$RMSEA, 3))) > 1, 
                      paste0(
                        "[", round(min(x$RMSEA), 3), "; ", round(max(x$RMSEA), 3), "]"), round(max(x$RMSEA), 3)),
      SRMR <- ifelse(length(unique(round(x$SRMR, 3))) > 1, 
                     paste0(
                       "[", round(min(x$SRMR), 3), "; ", round(max(x$SRMR), 3), "]"), round(max(x$SRMR), 3)),
      CHI.sq <- ifelse(length(unique(round(x$CHI.sq, 2))) > 1, 
                       paste0(
                         "[", round(min(x$CHI.sq), 2), "; ", round(max(x$CHI.sq), 2), "]"), round(max(x$CHI.sq), 2)),
      Pvalue <- ifelse(length(unique(round(x$Pvalue, 3))) > 1, 
                       paste0(
                         "[", round(min(x$Pvalue), 3), "; ", round(max(x$Pvalue), 3), "]"), round(max(x$Pvalue), 3)),
      Low.Load <- ifelse(any(x$Low.Load), "+", ""),
      Neg.Var <- ifelse(any(x$Neg.Var), "+", "")))
    
    for (i in 1:length(b)) { 
      colnames(b[[i]]) <- c("CFI.min", "RMSEA.max", "SRMR.max", 
                            "CFI", "RMSEA", "SRMR", "CHI.sq", "Pvalue",
                            "Low.Load", "Neg.Var")
      
    }
    
    b <- Reduce("rbind", b)
    rownames(b) <- names(fit.list)
    
    b <- b[!(b$Low.Load=="+"| b$Neg.Var=="+"| 
               (b$CFI.min>=0.9 & b$RMSEA.max<=0.08 & b$SRMR.max<=0.08)),]
    b <- b[rowSums(is.na(b)) != ncol(b),]
    
    fit.list1 <- subset(fit.list, subset = names(fit.list) %in% rownames(b))
    modind <- lapply(fit.list1, function(y) 
      lapply(y, function(x) data.frame(
        mod.ind <- ifelse (lavInspect(x, what = "post.check"), 
                           paste(modindices(x, sort = T)[1,1:3], collapse = ""), ""),
        mod.ind.v <- ifelse (lavInspect(x, what = "post.check"),  (round(modindices(x, sort = T)[1,4], 3)), "")
      )))
    
    modind <- lapply(modind, function(x)
      Reduce("rbind", x))
    
    for (i in 1:length(modind)) {
      colnames(modind[[i]]) <- c("ModInd", "Value")
    }
    
    modind <- lapply(modind, function(x) data.frame(
      x$ModInd <- gsub("~~",  " W ",  x$ModInd) %>% unique(.) %>% paste(., collapse = '; '),
      
      x$Value <- ifelse(is.numeric(x$Value),
                        ifelse(length(unique(round(x$Value, 3))) > 1, 
                               paste0("[", round(min(x$Value), 3), "; ", 
                                      round(max(x$Value), 3), "]"),
                               ifelse(length(unique(round(x$Value, 3))) == 1,
                                      round(min(x$Value), 3), NA)), NA)))
    
    modind <- Reduce("rbind", modind)
    rownames(modind) <- names(fit.list1)
    
    b <- cbind(b, modind)
    
    colnames(b) <- c("CFI.min", "RMSEA.max", "SRMR.max", 
                     "CFI", "RMSEA", "SRMR", "CHI.sq", "Pvalue",
                     "Low.Load", "Neg.Var", "ModInd", "Value")
    
    b <- b[, c("CHI.sq", "Pvalue", "CFI", "RMSEA", "SRMR", "ModInd", "Value")]
    
    df_to_viewer(b)
    return(b)
    
  }
  
  
  
}


# Function for configural models 


globalMI.mi.sep <- function(model, data, ...) {
  r.conf <- lapply(data, function(x)
    lavaan::cfa(model, data = x, ...))
  
  fit.model <- c("chisq.scaled", "df", "cfi.scaled", "rmsea.scaled", "srmr")
  
  r.conf1 <- lapply(r.conf, function(x)
    fitMeasures(x)[fit.model]
  )
  r.conf1 <- as.data.frame(Reduce("rbind", r.conf1))
  
  fit.tab <- data.frame(
    CHI.sq <- ifelse(length(unique(round(r.conf1$chisq.scaled, 2))) > 1, 
                     paste0(
                       "[", round(min(r.conf1$chisq.scaled), 2), "; ", round(max(r.conf1$chisq.scaled), 2), "]"), 
                     round(min(r.conf1$chisq.scaled), 2)),
    df <- unique(r.conf1$df),
    
    CFI <- ifelse(length(unique(round(r.conf1$cfi.scaled, 3))) > 1, 
                  paste0(
                    "[", round(min(r.conf1$cfi.scaled), 3), "; ", round(max(r.conf1$cfi.scaled), 3), "]"), 
                  round(min(r.conf1$cfi.scaled), 3)),
    
    RMSEA <- ifelse(length(unique(round(r.conf1$rmsea.scaled, 3))) > 1, 
                    paste0(
                      "[", round(min(r.conf1$rmsea.scaled), 3), "; ", round(max(r.conf1$rmsea.scaled), 3), "]"), 
                    round(min(r.conf1$rmsea.scaled), 3)),
    SRMR <- ifelse(length(unique(round(r.conf1$srmr, 3))) > 1, 
                   paste0(
                     "[", round(min(r.conf1$srmr), 3), "; ", round(max(r.conf1$srmr), 3), "]"), 
                   round(min(r.conf1$srmr), 3))
    
  )
  
  colnames(fit.tab) <- c("CHI.sq", "df", "CFI", "RMSEA", "SRMR") 
  return(fit.tab)
  
}




