####################################################################################################
# Pedro Carmona-Saez
# GENYO
# pedro.carmona@genyo.es
# December 2015
####################################################################################################
library('HardyWeinberg')
library(meta)
library (metafor)
require(gdata)

####################################################################################################
# Step 1. Calculates HWE in controls 
####################################################################################################

HW = function(data, columns){
  cols = colnames(data)
  colnames(data) = c(colnames(data)[1:(columns+3)], "AA", "AB", "BB")
  data$HW<-round(HWChisqMat(as.matrix(data[,(columns+4):(columns+6)]),cc=0)$pvalvec, digits=4)
  data$adj = round(p.adjust(data$HW, method = "fdr"), digits=4)
  colnames(data) = c(cols, "HW-P.value", "HW-adjusted.P.value")
  return(data)
}

####################################################################################################
# Step 2. Check inter-study heterogeneity 
# Step 3. Meta-analysis: Fixed or random effects model, Forest plot
####################################################################################################

# First, we create a fuctioin to prepare the data frame needed to analyse inter-study heterogeneity
# This function needs a matrix or data frame previously readed and the model

preparemeta = function (data, model, columns) {
  # First, the different columns are calculated, that are the following:
  # event.e: Number of events in experimental group.
  # n.e: Number of observations in experimental group.
  # event.c: Number of events in control group.
  # n.c: Number of observations in control group.
  
  # For this purpose, we first define which columns contain the data:
  AAcases = data[,(columns + 1)]
  Aacases = data[,(columns + 2)]
  aacases = data[,(columns + 3)]
  AActr = data[,(columns + 4)]
  Aactr = data[,(columns + 5)]
  aactr = data[,(columns + 6)]
  
  # The number of events is different for each model
  if (model == "allele") {
    event.e = AAcases*2 + Aacases
    n.e = (AAcases + Aacases + aacases)*2
    event.c = AActr*2 + Aactr
    n.c = (AActr + Aactr + aactr)*2
  }
  
  if (model == "recessive") {
    event.e = AAcases
    n.e = AAcases + Aacases + aacases
    event.c = AActr
    n.c = AActr + Aactr + aactr
  }
  
  if (model == "dominant") {
    event.e = AAcases + Aacases
    n.e = AAcases + Aacases + aacases
    event.c = AActr + Aactr
    n.c = AActr + Aactr + aactr
  }
  
  if (model == "overdominant") {
    event.e = Aacases
    n.e = AAcases + Aacases + aacases
    event.c = Aactr
    n.c = AActr + Aactr + aactr
  }
  
  if (model == "pairw1") {
    event.e = AAcases
    n.e = AAcases + aacases
    event.c = AActr
    n.c = AActr + aactr
  }
  
  if (model == "pairw2") {
    event.e = AAcases
    n.e = AAcases + Aacases
    event.c = AActr
    n.c = AActr + Aactr
  }
  
  if (model == "pairw3") {
    event.e = Aacases
    n.e = Aacases + aacases
    event.c = Aactr
    n.c = Aactr + aactr
  }
  
  # Now, the metabin data frame is created
  metadata = data.frame(data[,1:columns], event.e, n.e, event.c, n.c)
  return(metadata)
  
}

writemeta = function (data, model, columns, mode = 0) {
  
  # Meta-Analysis is done and the outputs prepared
  metadata = preparemeta(data, model, columns)
  meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,sm="OR", method="I", digits=1000000000000000000000000000000000000000000000)
  meta2 = summary(meta1)
  meta3 = capture.output(print.summary.meta(meta2))
  
  metao1 = paste0("Number of combined studies: ", meta2$k)
  metao2 = matrix(0, nrow = 2, ncol = 6,
                  dimnames = list(c(1:2), c("Model", "OR", "95%-CI", "z", "p-value", "adjusted p-value")))
  metao2[1,1] = "Fixed effect"
  metao2[2,1] = "Random effect"
  metao2[1,2] = substr(meta3[4], 22, gregexpr(" ", meta3[4])[[1]][6]-1)
  metao2[2,2] = substr(meta3[5], 22, gregexpr(" ", meta3[5])[[1]][4]-1)
  metao2[1,3] = substr(meta3[4], gregexpr("[", meta3[4], fixed=TRUE)[[1]][1], gregexpr("]", meta3[4], fixed=TRUE)[[1]][1])
  metao2[2,3] = substr(meta3[5], gregexpr("[", meta3[5], fixed=TRUE)[[1]][1], gregexpr("]", meta3[5], fixed=TRUE)[[1]][1])
  metao2[1,4] = round(meta2$fixed$z, digits=4)
  metao2[2,4] = round(meta2$random$z, digits=4)
  metao2[1,5] = round(meta2$fixed$p, digits=10)
  metao2[2,5] = round(meta2$random$p, digits=10)
  if (metao2[1,5] == 0) {
    metao2[1,5] = "< 1e-10"
  }
  if (metao2[2,5] == 0) {
    metao2[2,5] = "< 1e-10"
  }
  if (meta2$fixed$p*7 < 1){
    metao2[1,6] = round(meta2$fixed$p*7, digits=10)
  }
  else {
    metao2[1,6] = "1"
  }
  if (meta2$random$p*7 < 1){
    metao2[2,6] = round(meta2$random$p*7, digits=10)
  }
  else {
    metao2[2,6] = "1"
  }
  
  if (metao2[1,6] == 0) {
    metao2[1,6] = "< 1e-10"
  }
  if (metao2[2,6] == 0) {
    metao2[2,6] = "< 1e-10"
  }
  
  metao3 = "Heterogeneity tests:"
  metanuevo = matrix(nrow=1, ncol=5, dimnames = list(1, c("tau^2", "H", "I^2", "Q", "p-value")),
                     data = c(round(meta2$tau^2, digits=4), round(meta2$H$TE, digits=4), round(meta2$I2$TE, digits=4),
                         round(meta2$Q, digits=4), substr(meta3[12], gregexpr("\\ [^\\ ]*$", meta3[12])[[1]]+1, nchar(meta3[12]))))
  metao4 = paste("tau^2 =", round(meta2$tau^2, digits=4))
  metao5 = paste("H =", round(meta2$H$TE, digits=4))
  metao6 = paste("I^2 =", round(meta2$I2$TE, digits=4))
  
  metao7 = "Test of heterogeneity:"
  metao8 = data.frame(Q = meta2$Q, 
                      "p-value" = substr(meta3[12], gregexpr("\\ [^\\ ]*$", meta3[12])[[1]]+1, nchar(meta3[12])))
  
  metao9 = "Details on meta-analytical method:"
  metao10 = "- Fixed effect estimate method: Inverse variance"
  metao11 = "- Random effect estimate method: DerSimonian-Laird"
  
  if (mode == 0) {
    return(list(metao1, metao2, metao3, metao4, metao5, metao6, metao7, metao8,
                metao9, metao10, metao11, metanuevo))
  }
  
  else {
    return(meta3)
  }
  
}



## This function represents a forest plot for a data with the specified model

forestplot = function(data, model, columns, fixRan) {
  metadata = preparemeta(data, model, columns)
  
  meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,studlab=data[,1],sm="OR", method="I")
  
  if (fixRan == "both"){
    forest(meta1, main = model, showweights=TRUE)
  }
  else if (fixRan == "fixed"){
    forest(meta1, main = model, showweights=TRUE, comb.random=F)
  }
  else {
    forest(meta1, main = model, showweights=TRUE, comb.fixed=F)
  }
}

###########################################################################
## Publication bias: Funnel plot and Egger's linear regression
###########################################################################

funnelplot = function(data, model, columns, funLab) {
  metadata = preparemeta(data, model, columns)
  
  meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,studlab=data[,1],sm="OR", method="I")
  if (funLab){
    funnel(meta1, studlab=T)
  }
  else {
    funnel(meta1, studlab=F)
  }
}

egger = function(data, model, columns, mode=0) {
  AAcases = data[,(columns + 1)]
  Aacases = data[,(columns + 2)]
  aacases = data[,(columns + 3)]
  AActr = data[,(columns + 4)]
  Aactr = data[,(columns + 5)]
  aactr = data[,(columns + 6)]
  
  if (model == "allele") {
    ### calculate log relative risks and corresponding sampling variances
    dat <- escalc(measure="OR", ai=AAcases*2 + Aacases, bi=aacases*2 + Aacases, ci=AActr*2 + Aactr, di=aactr*2 + Aactr, data=data)
    ### fit random-effects model
    res <- rma(yi, vi, data=dat)
  }
  
  if (model == "recessive") {
    dat <- escalc(measure="OR", ai=AAcases, bi=Aacases + aacases, ci=AActr, di=Aactr + aactr, data=data)
    res <- rma(yi, vi, data=dat)
  }
  
  if (model == "dominant") {
    dat <- escalc(measure="OR", ai=AAcases + Aacases, bi=aacases, ci=AActr + Aactr, di=aactr, data=data)
    res <- rma(yi, vi, data=dat)
  }
  
  if (model == "overdominant") {
    dat <- escalc(measure="OR", ai=Aacases, bi=AAcases+aacases, ci=Aactr, di=AActr+aactr, data=data)
    res <- rma(yi, vi, data=dat)
  }
  
  if (model == "pairw1") {
    dat <- escalc(measure="OR", ai=AAcases, bi=aacases, ci=AActr, di=aactr, data=data)
    res <- rma(yi, vi, data=dat)
  }
  
  if (model == "pairw2") {
    dat <- escalc(measure="OR", ai=AAcases, bi=Aacases, ci=AActr, di=Aactr, data=data)
    res <- rma(yi, vi, data=dat)
  }
  
  if (model == "pairw3") {
    dat <- escalc(measure="OR", ai=Aacases, bi=aacases, ci=Aactr, di=aactr, data=data)
    res <- rma(yi, vi, data=dat)
  }
  
  ### classical Egger test
  results = regtest(res, model="lm")
  
  
  if (mode==0) {
    #     t = paste0("t = ", round(results$zval, digits = 4))
    #     df = paste0("df = ", results$dfs)
    p = paste0("P-value = ", round(results$pval, digits=4))
    # resultsf = list(t, df, p)
    # return(resultsf)
    return(p)
  }
  else {
    #     t = round(results$zval, digits = 4)
    #     df = results$dfs
    p = round(results$pval, digits=4)
    # resultsf = data.frame (c("t", "df", "p"), c(t, df, p))
    # return(resultsf)
    return(p)
  }
  
}



###########################################################################
## Grouping studies
###########################################################################

models = c("Allele contrast (A vs. a)" = "allele", "Recessive model (AA vs. Aa+aa)" = "recessive",
           "Dominant model (AA+Aa vs. aa)" = "dominant", "Overdominant (Aa vs. AA + aa)" = "overdominant",
           "pairw1 (AA vs. aa)" = "pairw1",
           "pairw2 (AA vs. Aa)" = "pairw2", "pairw3 (Aa vs. aa)" = "pairw3")

results_grouped = function(data, columns, group) {
  
  if (group == "No subgrouping (results summary)"){
    group = colnames(data)[1]
  }
  # Se prepara el header de la tabla
  header = data.frame("Model"= "", stringsAsFactors = F)
  header[,paste(group)] = ""
  header[,"Number of studies"] = ""
  header[," "] = "OR"
  header[,"Test of association"] = "95% CI"
  header[,"  "] = "p-val"
  header[,"   "] = "Model"
  header[,"Test of heterogeneity"] = "p-val"
  header[,"    "] = "I^2"
  header[,"Publication bias"] = "p-val (Egger's test)"
  
  cols = colnames(data[1:columns]) # Nombres de las columnas adicionales a los datos
  groupfixed = paste0("^", group, "$") # Nombre de la columna con los anclajes añadidos para buscarla en los datos
  colInt = grep(groupfixed, cols) # Número de columna de interés
  groups = levels(factor(data[,colInt])) # Los distintos grupos que hay en esa columna
  ngroups = length(groups) # Número de grupos 
  
  for (model in models) {
    
    # Se calculan los resultados para todos los datos con ese modelo
    metadata = preparemeta(data, model, columns)
    meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,studlab=data[,1],sm="OR", method="I", digits = 10000000000000000000000000000000000000000)
    
    meta2 = summary(meta1)
    meta3 = capture.output(print.summary.meta(meta2))
    
    eggerP = egger(data, model, columns, 1)
    
    dataframe = data.frame("Model"= c(names(models)[models==model], rep("", ngroups)), stringsAsFactors = F)
    dataframe[,paste(group)] = c("Overall", rep("", ngroups))
    dataframe[,"Number of studies"] = c(meta2$k, rep("", ngroups))
    
    # Se comprueba qué modelo se debe usar (fixed o random effect)
    p_heter = as.numeric(substr(meta3[12], gregexpr("\\ [^\\ ]*$", meta3[12])[[1]]+1, nchar(meta3[12])))
    
    if (p_heter > 0.1) {   ## Fixed effect
      dataframe[," "] = c(substr(meta3[4], 22, gregexpr(" ", meta3[4])[[1]][6]-1), rep("", ngroups)) ## OR
      dataframe[,"Test of association"] = c(substr(meta3[4], gregexpr("[", meta3[4], fixed=TRUE)[[1]][1], gregexpr("]", meta3[4], fixed=TRUE)[[1]][1]), rep("", ngroups))  ## 95%-CI
      dataframe[,"  "] = c(round(meta2$fixed$p, digits=10), rep("", ngroups)) ## p-value
      dataframe[,"   "] = c("Fixed", rep("", ngroups))
    }
    
    else { ## Random effect
      
      dataframe[," "] = c(substr(meta3[5], 22, gregexpr(" ", meta3[5])[[1]][4]-1), rep("", ngroups))  ## OR
      dataframe[,"Test of association"] = c(substr(meta3[5], gregexpr("[", meta3[5], fixed=TRUE)[[1]][1], gregexpr("]", meta3[5], fixed=TRUE)[[1]][1]), rep("", ngroups))  ## 95%-CI
      dataframe[,"  "] = c(round(meta2$random$p, digits=10), rep("", ngroups))  ## p-value
      dataframe[,"   "] = c("Random", rep("", ngroups))
      
    }
    
    dataframe[,"Test of heterogeneity"] = c(p_heter, rep("", ngroups))
    dataframe[,"    "] = c(round(meta2$I2$TE, digits=4), rep("", ngroups))
    dataframe[,"Publication bias"] = c(eggerP, rep("", ngroups))
    
    
    # Se calculan los resultados para cada uno de los agrupamientos
    for (x in 1:ngroups) {
      datatocalc = data[data[,colInt] == groups[x],] # Input solo con los estudios que pertenecen a un grupo
      
      # Se comprueba que hay más de un estudio en ese grupo
      if (group == colnames(data)[1]) {next}
      
      
      metadata = preparemeta(datatocalc, model, columns)
      
      meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,studlab=datatocalc[,1],sm="OR", method="I", digits=1000000000000000000000000000000000000000000000000000000)
      
      meta2 = summary(meta1)
      
      
      meta3 = capture.output(print.summary.meta(meta2))
      
      dataframe[x+1,paste(group)] = groups[x]
      dataframe[x+1,"Number of studies"] = meta2$k
      
      if (nrow(datatocalc) < 2) {
        p_heter = "NA"
        dataframe[x+1," "] = substr(meta3[2], 2, gregexpr(" ", meta3[2])[[1]][2]-1)
        dataframe[x+1,"Test of association"] = substr(meta3[2], gregexpr("[", meta3[2], fixed=TRUE)[[1]][1], gregexpr("]", meta3[2], fixed=TRUE)[[1]][1])
        dataframe[x+1,"  "] = round(meta2$fixed$p, digits=10)
        dataframe[x+1,"   "] = "Fixed"
        dataframe[x+1,"Test of heterogeneity"] = "NA"
        dataframe[x+1,"    "] = "NA"
        
      }
      else {
        p_heter = as.numeric(substr(meta3[12], gregexpr("\\ [^\\ ]*$", meta3[12])[[1]]+1, nchar(meta3[12])))
        
        if (p_heter > 0.1) {   ## Fixed effect
          
          dataframe[x+1," "] = substr(meta3[4], 22, gregexpr(" ", meta3[4])[[1]][6]-1)
          dataframe[x+1,"Test of association"] = substr(meta3[4], gregexpr("[", meta3[4], fixed=TRUE)[[1]][1], gregexpr("]", meta3[4], fixed=TRUE)[[1]][1])
          dataframe[x+1,"  "] = round(meta2$fixed$p, digits=10)
          dataframe[x+1,"   "] = "Fixed"
          dataframe[x+1,"    "] = "NA"
          
          
        }
        
        else {
          dataframe[x+1," "] = substr(meta3[5], 22, gregexpr(" ", meta3[5])[[1]][4]-1)
          dataframe[x+1,"Test of association"] = substr(meta3[5], gregexpr("[", meta3[5], fixed=TRUE)[[1]][1], gregexpr("]", meta3[5], fixed=TRUE)[[1]][1])
          dataframe[x+1,"  "] = round(meta2$random$p, digits=10)
          dataframe[x+1,"   "] = "Random"
        }
        dataframe[x+1,"Test of heterogeneity"] = p_heter
        dataframe[x+1,"    "] = round(meta2$I2$TE, digits=4)
        
        
      }
      

      
      
      if (nrow(datatocalc) > 2) {
        eggerP = egger(datatocalc, model, columns, 1)
        
        dataframe[x+1,"Publication bias"] = eggerP[[1]]
      }
      else {
        dataframe[x+1,"Publication bias"] = "NA"
      }
    }
    
    # Se añade el dataframe creado para este modelo al anterior
    header = rbind(header, dataframe)
    
    #Se eliminan las filas vacías
    header = header[header[,4] != "",]
    
  }
  return(header)
}


##################################################
### Robustness - leave-one-out influence analysis
##################################################

robustness = function(data, model, columns, fixRanRob){
  

  metadata = preparemeta(data, model, columns)
  
  meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,studlab=data[,1],sm="OR", method="I")
  
  robustTest = metainf(meta1, pooled = fixRanRob)
  robustTest = data.frame(Study = robustTest$studlab, Estimate = robustTest$TE, Std_Error = robustTest$seTE, P_value = robustTest$p.value)
  robustTest= robustTest[-(nrow(robustTest)-1),]
  
  is.num <- sapply(robustTest, is.numeric)
  robustTest[is.num] <- lapply(robustTest[is.num], round, 3)

  return(robustTest)
}

robustPlot = function(data, model, columns, fixRanRob){
  
  metadata = preparemeta(data, model, columns)
  
  meta1 = metabin(event.e, n.e, event.c, n.c,data=metadata,studlab=data[,1],sm="OR", method="I")
  
  forest(metainf(meta1, pooled = fixRanRob), main = model, showweights=TRUE)
  
}




#####################################
### CODE FOR handsone_to_r REPAIR ###
#####################################


hot_to_r2 = function (...) 
{
  do.call(toR2, ...)
}

toR2 = function(data, changes, params, ...) {
  rClass = params$rClass
  colHeaders = unlist(params$rColHeaders)
  rowHeaders = unlist(params$rRowHeaders)
  rColClasses = unlist(params$rColClasses)[colHeaders]
  
  out = data
  
  # copy/paste may add rows without firing an afterCreateRow event (still needed?)
  # if (length(out) != length(rowHeaders))
  #   changes$event = "afterCreateRow"
  
  # remove spare empty rows; autofill fix (not working)
  # if (!is.null(changes$source) && changes$source == "autofill") {
  #   rm_inds = sapply(out, function(x) all(unlist(x) == "NA"))
  #   rm_inds = suppressWarnings(min(which(diff(rm_inds) == -1)))
  #   if (rm_inds != Inf)
  #     out = out[-(length(out) - rm_inds + 1)]
  # }
  
  # pre-conversion updates; afterCreateCol moved to end of function
  # if (changes$event == "afterCreateRow") {
  #   inds = seq(changes$ind + 1, length.out = changes$ct)
  #   # prevent duplicates
  #   nm = 1
  #   while (nm %in% rowHeaders) {
  #     nm = nm + 1
  #   }
  #   rowHeaders = c(head(rowHeaders, inds - 1), nm,
  #                  tail(rowHeaders, length(rowHeaders) - inds + 1))
  # } else if (changes$event == "afterRemoveRow") {
  #   inds = seq(changes$ind + 1, length.out = changes$ct)
  #   rowHeaders = rowHeaders[-inds]
  if (changes$event == "afterRemoveCol") {
    if (!("matrix" %in% rClass)) {
      inds = seq(changes$ind + 1, 1, length.out = changes$ct)
      rColClasses = rColClasses[-inds]
    }
  }
  
  # convert
  if ("matrix" %in% rClass) {
    nr = length(out)
    out = unlist(out, recursive = FALSE)
    # replace NULL with NA
    out = unlist(lapply(out, function(x) if (is.null(x)) NA else x))
    
    # If there is no data create empty matrix
    if (length(out) == 0) {
      out = matrix(nrow = 0, ncol = length(colHeaders))
    } else {
      out = matrix(out, nrow = nr, byrow = TRUE)
    }
    
    class(out) = params$rColClasses
    
  } else if ("data.frame" %in% rClass) {
    nr = length(out)
    
    out = unlist(out, recursive = FALSE)
    # replace NULL with NA
    out = unlist(lapply(out, function(x) if (is.null(x)) NA else x))
    
    # If there is no data create empty matrix
    if (length(out) == 0) {
      out = matrix(nrow = 0, ncol = length(colHeaders))
    } else {
      out = matrix(out, nrow = nr, byrow = TRUE)
    }
    
    out = colClasses(as.data.frame(out, stringsAsFactors = FALSE),
                     rColClasses, params$columns, ...)
  } else {
    stop("Conversion not implemented: ", rClass)
  }
  
  
  # post-conversion updates
  if (changes$event == "afterCreateRow") {
    # default logical NA in data.frame to FALSE
    if (!("matrix" %in% rClass)) {
      inds_logical = which(rColClasses == "logical")
      for (i in inds_logical)
        out[[i]] = ifelse(is.na(out[[i]]), FALSE, out[[i]])
    }
  }
  
  if (ncol(out) != length(colHeaders))
    colHeaders = genColHeaders(changes, colHeaders)
    # colHeaders = c(colHeaders[1:(length(colHeaders)-6)],
    #                paste0("NewCol", seq(from=length(colHeaders) - 6, to = (ncol(out) - 6))),
    #                colHeaders[(length(colHeaders)-5):(length(colHeaders))])
    # colHeaders = paste0("V",seq(ncol(out)))
  
  
  # if (nrow(out) != length(rowHeaders) && !is.null(rowHeaders))
  #   rowHeaders = genRowHeaders(changes, rowHeaders)
  
  colnames(out) = colHeaders
  # rownames(out) = rowHeaders
  
  if ("data.table" %in% rClass)
    out = as(out, "data.table")
  
  out
}

colClasses <- function(d, colClasses, cols, date_fmt = "%m/%d/%Y", ...) {
  colClasses <- rep(colClasses, len=length(d))
  for(i in seq_along(d))
    d[[i]] = switch(
      colClasses[i],
      Date = as.Date(d[[i]], origin='1970-01-01',
                     format = date_fmt),
      POSIXct = as.POSIXct(d[[i]], origin='1970-01-01',
                           format = date_fmt),
      factor = factor(d[[i]],
                      levels = c(unlist(cols[[i]]$source),
                                 unique(d[[i]][!(d[[i]] %in% unlist(cols[[i]]$source))])),
                      ordered = TRUE),
      json = jsonlite::toJSON(d[[i]]),
      suppressWarnings(as(d[[i]], colClasses[i])))
  d
}

genRowHeaders <- function(changes, rowHeaders) {
  inds = seq(changes$ind + 1, length.out = changes$ct[1])
  
  if (changes$event == "afterCreateRow") {
    # prevent duplicates
    nm = 1
    while (nm %in% rowHeaders) {
      nm = nm + 1
    }
    c(head(rowHeaders, inds - 1), nm,
      tail(rowHeaders, length(rowHeaders) - inds + 1))
  } 
  
  else if (changes$event == "afterRemoveRow") {
    rowHeaders[-inds]
  }
}

genColHeaders <- function(changes, colHeaders) {
  ind_ct = length(which(grepl("V[0-9]{1,}", colHeaders)))
  
  if (changes$event == "afterRemoveCol") {
    colHeaders[-(seq(changes$ind, length = changes$ct) + 1)]
  } else if (changes$event == "afterCreateCol") {
    # create new column names
    new_cols = paste0("V", changes$ct + ind_ct)
    # insert into vector
    inds = seq(changes$ind + 1, 1, length.out = changes$ct)
    c(colHeaders, new_cols)[order(c(seq_along(colHeaders), inds - 0.5))]
  } 
  else {
    stop("More pasted columns than the available ones in the table. Add more columns with ADD button.")
  }
}
