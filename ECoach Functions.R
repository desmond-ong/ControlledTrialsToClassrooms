# Loading Packages --------------------------------------------------------
library (tidyverse)
library(GGally)






# FUnctions for Cleaning  -------------------------------------------------
###Function to generate a table with all observations within a variable and its corresponding count
check_observation <- function (df, column) {
  require (tidyverse)
  check <- df %>% 
    group_by (as.vector(unlist(df[, column]))) %>% 
    count () 
  
  colnames(check)[1] <- column
  return (check)
  
}



recur.collapse <- function(df, n = ncol(df)) { #collapse function
  combine <- function(x,y){
    if(!is.na(x)){
      return(x)
    } else if (!is.na(y)) {
      return(y)
    } else {
      return(NA)
    }
  }
  
  if(n==1){
    
    return(df[,1])
    
  } else {
    
    return(mapply(combine, df[,n], recur.collapse(df, n-1)))
  } 
}


multi_spread <- function(df, id, key, var){
  require(gtools)
  require(tidyverse)
  
  df.list <- lapply(var, 
                    function(x){
                      df.temp <- spread(df[, c(id, key, x)], key, x) # spread 1 variable
                      cname <- mixedsort(unique(as.vector(unlist(df[,key])))) # sort colnames in alphanumeric order
                      df.temp <- df.temp[, c(id, cname)] # rename column names
                      colnames(df.temp)[2:ncol(df.temp)]  <- paste(x, colnames(df.temp[,cname]), sep= "_") #add key to column names
                      return(df.temp) 
                    })
  
  df.output <- df.list[[1]] # initialise
  for(i in 1:(length(var)-1)){ # combine matrices together
    df.output <- full_join(df.output, df.list[[i+1]])
  }
  
  return(as.data.frame(df.output))        
  
}
###Collapsing variables with multiple column into one
collapse_column <- function (df, columns, new.col.name, ignore.conflicts = F, na.conflicts = F, remove.existing = T) {
  
  #Subsetting the dataframe with specified columns
  df.temp <- df [, columns]
  
  #initializing empty vectors
  row.temp <- NULL
  conflicted.rows <- NULL 
  result <- NULL
  
  #checking through data for each row
  for (i in 1:nrow (df.temp)) {
    
    row.temp <- as.vector (unlist (df.temp [i,])) #saving each row as a vector
    row.temp <- row.temp [!duplicated (row.temp)] #removing duplicates from each row 
    row.temp <- row.temp [!is.na(row.temp)] #removing NAs from each row
    
    if (length(row.temp) == 0) { #if nothing
      #then result = NA
      result [i] <- NA
      
    } else if (length(row.temp) == 1) { #if there is one object
      #then save that object into result
      result [i] <- row.temp
      
    } else if (length(row.temp) > 1) { #if there is more than one object that is different
      
      if(na.conflicts == T) {
        result[i] <- NA
      } else {
        #record the position of the row in conflicted.rows
        conflicted.rows <- c(conflicted.rows, i)
        #save the latest object as result
        result [i] <- row.temp [length(row.temp)]
      }
    } else {NULL}
  }
  
  #creating a dataframe with rows that have conflict 
  df.conflicts <- df [conflicted.rows, ]
  
  #creating dataframe with the collapsed column 
  df.final <- df
  
  if (remove.existing == T){
    col.drop.pos <- which (colnames(df.final) %in% columns)
    df.final <- df.final [,-(col.drop.pos)]
  } else {NULL}
  
  
  df.final [, new.col.name] <- result
  
  
  
  
  #Returning results based on if there are conflicts and based on input for ignore.conflicts
  #If there are conflicts 
  if (nrow (df.conflicts) > 0) {
    print ("Warning: There are rows with more than 1 type of entry within selected columns")
    
    if (ignore.conflicts == F) { #If user chose not to ignore conflicts 
      #Return dataframe with conflicted rows
      print ("Warning: Returning dataframe with conflicted observations")
      return (df.conflicts) 
      
    } else { #If user chose to ignore conflicts
      #Return dataframe with input in new column based on the latest column specified
      print ("Warning: Returning dataframe ignoring all conflicts")
      return (df.final)
    }
  } else { #If there are no conflicts
    #Return dataset with new column
    print ("ALL's well that ends well")
    return (df.final)
  }
}

# Functions for visualization  --------------------------------------------
lm_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(position = "jitter") + 
    geom_smooth(method=lm, se = TRUE) +
    theme_bw(base_size = 16)
  p
}

reduce.listwise.deletion <- function(dataframe, target.columns = colnames(dataframe), drop){
  
  check.dataframe <- is.na(dataframe[, target.columns])
  
  for (i in 1:nrow(check.dataframe)){
    
    if (sum(check.dataframe[i, ]) <=  drop){ 
      
      NULL
      
    } else if (sum(check.dataframe[i, ]) >  drop) {
      
      check.dataframe[i, ] <- FALSE
      
    } else {
      
      print("ERROR")
      
    }
    
  }
  
  sum.dataframe <- apply(check.dataframe, 2, sum) # sum each column
  
  return(sum.dataframe)
  
}

cor.with <- function(dataframe, var, with = colnames(dataframe),  group = NULL){
  
  require(tidyverse)
  
  if (length(with) != length(colnames(dataframe))) {
    
    with <- c(var, with) ## adds variable for users
    
  } else {
    
    NULL
    
  }
  
  
  if (length(group) == 0 ) {
    
    result <- cor(dataframe[, with], use = "complete.obs") [ , var]
    
    print(paste("n =", nrow(na.omit(dataframe[, with]))))
    
    return(result)
    
  } else {
    
    df.grp <- as.vector(unlist(unique(dataframe[, group])))
    
    cor.matrix <- c()
    
    while (length(df.grp) > 0) { # for each factor in group
      
      with <- with[with!=group]
      subset.rows <- which(dataframe[, group] == df.grp[1])
      ## subset out group
      temp.df <- dataframe[subset.rows, ]
      
      temp.row <- c() ## initializing
      
      temp.row <- cor(temp.df[, with], use = "complete.obs") [ , var]
      
      
      
      ## add temp.row by row
      cor.matrix <- rbind(cor.matrix, temp.row)
      
      ## closing while loop
      df.grp <- df.grp[-1]
      
    }
    
    ## give row names
    rownames(cor.matrix) <- as.vector(unlist(unique(dataframe[, group])))
    
    ## give col names
    colnames(cor.matrix) <- with
    
    print("MESSAGE: Complete Observations are used for each Group")
    
    return(cor.matrix)
    
    
  }
  
  
  
}

regress.with <- function(dataframe, var, with, control = NULL,  group = NULL, output = c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "n")){
  
  require(tidyverse)
  
  if (length(control) == 0){
    ## formulae
    formulae.str <- paste(var, "~")
    formulae.str <- paste(formulae.str, with)
    
    formulae <- as.formula(formulae.str)  
    
    
  } else {
    
    formulae.str <- paste(var, "~")
    formulae.str <- paste(formulae.str, with)
    
    for (i in 1:length(control)) {
      formulae.str <- paste(formulae.str, "+")
      formulae.str <- paste(formulae.str, control[i])
    }
    
    formulae <- as.formula(formulae.str)  
  }
  
  if (length(group) == 0 ) {
    
    result <- c()
    
    for (i in 1:length(with)) {
      model <- lm(formulae, data = dataframe)
      
      value <- model$coefficients[2]
      result <- c(result, value)
      
      if(length(model$na.action)>0){
        print(paste("WARNING: removed", length(model$na.action), "(", round(100*length(model$na.action)/nrow(dataframe),2), "%)", "observations due to missingness"))
      }
      
    }
    
    names(result) <- with
    
    return(result)
    
  } else if (output[1] == "n") {
    
    df.grp <- as.vector(unlist(unique(dataframe[, group])))
    
    
    cor.matrix <- c()
    
    while (length(df.grp) > 0) { # for each factor in group
      
      subset.rows <- which(dataframe[, group] == df.grp[1])
      ## subset out group
      temp.df <- dataframe[subset.rows, ]
      
      temp.row <- c() ## initializing
      
      for (i in 1:length(with)) {
        model <- lm(formulae, data = temp.df)
        
        model.sum <- summary(model)
        
        value <- nrow(temp.df) - length(model$na.action)
        
        temp.row <- c(temp.row, value)
        
        if(length(model$na.action)>0){
          print(paste("WARNING: removed", length(model$na.action), "(", round(100*length(model$na.action)/nrow(temp.df),2), "%)", "observations due to missingness in", df.grp[1]))
        }
        
      }
      
      ## add temp.row by row
      cor.matrix <- rbind(cor.matrix, temp.row)
      
      ## closing while loop
      df.grp <- df.grp[-1]
      
    }
    
    # give row names
    rownames(cor.matrix) <- as.vector(unlist(unique(dataframe[, group])))
    
    ## give col names
    colnames(cor.matrix) <- with
    
    return(cor.matrix)
    
    
  } else {
    
    df.grp <- as.vector(unlist(unique(dataframe[, group])))
    
    
    cor.matrix <- c()
    
    while (length(df.grp) > 0) { # for each factor in group
      
      subset.rows <- which(dataframe[, group] == df.grp[1])
      ## subset out group
      temp.df <- dataframe[subset.rows, ]
      
      temp.row <- c() ## initializing
      
      for (i in 1:length(with)) {
        model <- lm(formulae, data = temp.df)
        
        model.sum <- summary(model)
        
        value <- model.sum$coefficients[2, output[1]]
        
        temp.row <- c(temp.row, value)
        
        if(length(model$na.action)>0){
          print(paste("WARNING: removed", length(model$na.action), "(", round(100*length(model$na.action)/nrow(temp.df),2), "%)", "observations due to missingness in", df.grp[1]))
        }
        
      }
      
      ## add temp.row by row
      cor.matrix <- rbind(cor.matrix, temp.row)
      
      ## closing while loop
      df.grp <- df.grp[-1]
      
    }
    
    # give row names
    rownames(cor.matrix) <- as.vector(unlist(unique(dataframe[, group])))
    
    ## give col names
    colnames(cor.matrix) <- with
    
    return(cor.matrix)
    
    
  }
  
  
  
}

abstract.regression <- function(form, data) {
  
  locate.element <- function(form){ # unless they are within {}
    
    between <- str_locate_all(form, "[[:space:]]|[[+]]|[[-]]|[[*]]|[[~]]")[[1]][,1]
    
    betw.brack <- str_locate_all(form, "(?<=\\{).+?(?=\\})")[[1]]
    
    if (length(betw.brack > 0)){
      
      ignore <- c()
      
      for(i in 1:nrow(betw.brack)) {
        
        ignore <- c(ignore, betw.brack[i, 1]:betw.brack[i, 2])
        
      }
      
      between <- between[!between %in% ignore]
      
    } else {NULL}
    
    length <- 1:nchar(form)
    text <- length[!length %in% between]
    
    return(unname(text))
    
    
  }
  
  locate.operator <- function(form){ # unless they are within {}
    
    between <- str_locate_all(form, "[[+]]|[[-]]|[[*]]|[[~]]")[[1]][,1]
    
    betw.brack <- str_locate_all(form, "(?<=\\{).+?(?=\\})")[[1]]
    
    if (length(betw.brack > 0)){
      
      ignore <- c()
      
      for(i in 1:nrow(betw.brack)) {
        
        ignore <- c(ignore, betw.brack[i, 1]:betw.brack[i, 2])
        
      }
      
      between <- between[!between %in% ignore]
      
    } else {NULL}
    
    return(unname(between))
    
    
  }
  
  break.formulae <- function(form){
    
    require(stringr)
    
    ## break text
    form <- gsub(" ", "", form)
    
    text <- locate.element(form)
    
    group.text <- list()
    
    for (i in 1:length(text)){
      
      if( i == 1 ) { # store first char
        
        group.text[[1]] <- text[1]
        count <- 1
        
      } else if ( text[i] == text[i-1] + 1 ) {
        
        group.text[[count]] <- c(group.text[[count]], text[i])
        
      } else if ( text[i] != text[i-1] + 1 ) {
        
        group.text[[count + 1]] <- text[i]
        count <- count + 1
        
      }
      
      
    }
    
    
    elements <- c()
    
    for (i in 1:length(group.text)){
      
      elements <- c(elements, substr(form, min(group.text[[i]]), max(group.text[[i]])))
      
    }
    
    return (elements )
    
    
    
  }
  
  simple.operation <- function(expr) {
    
    expr <- gsub(" ", "", expr)
    
    operator <- substr(expr, locate.operator(expr), locate.operator(expr)) 
    
    first <- break.formulae(expr)[1]
    second <- break.formulae(expr)[2]
    
    
    if(operator == "+"){
      
      as.numeric(first) + as.numeric(second)
      
    } else if (operator == "-"){
      
      as.numeric(first) - as.numeric(second)
      
    } else if (operator == "*"){
      
      as.numeric(first) * as.numeric(second)
      
    } else{
      
      NULL
    }
    
    
  }
  
  get.variable <- function(form, data){
    
    template <- break.formulae(form)
    
    # change {} to .
    for (i in 1:length(template)){
      
      betw.brack <- str_locate_all(template[i], "(?<=\\{).+?(?=\\})")[[1]]
      
      # replacing {} with .
      if (length(betw.brack) > 0){ 
        
        expr <- paste0("{", substr(template[i], betw.brack[1,1], betw.brack[1, 2]), "}")
        
        template[i] <- gsub(
          pattern = expr,
          replacement = ".", template[[i]], fixed = T)
        
        
      } else {
        
        NULL
        
      }
      
      
    }
    
    # replacing X with .
    for (i in 1:length(template)){
      
      template[[i]] <- gsub("X", ".", template[[i]] )
      
      
    }
    
    # getting rid of redundant templates
    template <- as.vector(unlist(unique(template)))
    
    # initialising list
    variable.list <- vector(mode = "list", length = length(template))
    
    names(variable.list) <- template
    
    require(gtools)
    
    for (i in 1:length(template)){
      
      variable.list[[i]] <- mixedsort(colnames(data)[grep(names(variable.list)[i], colnames(data))])
      
      
    }
    
    return(as.vector(unlist(variable.list)) )
    
  }
  
  
  get.formulae <- function(form, variables, data){
    
    formulae.list <- list()
    
    for ( i in 1:length(variables)){
      
      betw.brack <- str_locate_all(form, "(?<=\\{).+?(?=\\})")[[1]]
      
      # dealing with {}
      if (length(betw.brack) > 0){
        
        expr <- c() # initialise
        
        for (j in 1:nrow(betw.brack)){ # for each {}
          
          expr <- c(expr, substr(form, betw.brack[j,1], betw.brack[j, 2]))
          
        }
        
        expr <- gsub ("X", i, expr)
        
        for (j in  1:length(expr)){ # for each expression
          
          expr[j] <- simple.operation(expr[j] )
          
          
        } 
        
        
        ### sub expr back to {}
        temp.form <- form
        for (j in 1:nrow(betw.brack)){ # for each {}
          
          temp.expr <- paste0("{", substr(form, betw.brack[j,1], betw.brack[j, 2]), "}")
          temp.form <- gsub(temp.expr, expr[j], temp.form, fixed = T)
          
        }
        
        ### sub X to i
        temp.form <- gsub("X", i, temp.form)
        
        temp.form.elements <- as.vector(unlist(break.formulae(temp.form)))
        
        check <- sum(temp.form.elements %in% colnames(data))
        
        if ( check == length(temp.form.elements)){
          
          formulae.list <- c(formulae.list, as.formula(temp.form))
          
        } else {NULL}
        
        
      } else { # if no {}
        
        ### sub X to i
        temp.form <- form
        
        temp.form <- gsub("X", i, temp.form)
        
        temp.form.elements <- as.vector(unlist(break.formulae(temp.form)))
        
        check <- sum(temp.form.elements %in% colnames(data))
        
        if ( check == length(temp.form.elements)){
          
          formulae.list <- c(formulae.list, as.formula(temp.form))
          
        } else {NULL}
        
      }
      
    }
    
    return(formulae.list)
    
  }
  
  #######
  
  elements <- break.formulae(form)
  elements # get constituent parts
  
  variables <- get.variable(form, data) # variables from data
  
  forms <- get.formulae(form, variables, data) # possible formulaes for modelling
  
  ######
  
  model.list <- list()
  
  for (i in 1:length(forms)){ # for each model
    
    model.list[[i]] <- summary(lm(forms[[i]], data = data))
    
    
  }
  
  for (i in 1:length(forms)){ # for each model
    
    if (i == 1) {
      
      coefs <-  model.list[[i]]$coefficients[2:nrow(model.list[[i]]$coefficients), c("Estimate", "Std. Error", "Pr(>|t|)")]
      
    } else {
      
      coefs <- rbind (coefs, 
                      model.list[[i]]$coefficients[2:nrow(model.list[[i]]$coefficients),c("Estimate", "Std. Error", "Pr(>|t|)")])
      
      
    }
    
  }
  
  print(forms) # print formulaes for checking
  return(coefs)
  
}

extract.df.list <- function(input, parameter, max.int, na.num = length(max.int)){ # give parameter number safer
  
  locate.use <- function(coefs, max.int){
    
    if (is.vector(coefs)){
      
      n <- 1
      
    } else {
      
      n <- nrow(coefs)
    }
    
    
    if (n >= max(max.int)){
      
      return(max.int)
      
    } else{ # if less rows than provided interval
      
      while(n < max(max.int)){
        
        max.int <- max.int[-length(max.int)]
        
      }
      
      return(max.int)
      
    }
    
  }
  
add.na <- function(coefs, na.num){
    
    if (length(coefs) < na.num){
      
      coefs <- c(coefs, rep(NA, na.num - length(coefs) ))
      
    } else {coefs <- coefs}
    
    
  }
  
  output <- NULL
  
  for (i in 1:length(input)) {
    
    if (is.vector(input[[i]])){
      
      temp <- input[[i]][parameter]
      temp <- add.na(temp, na.num)
      
      output <- rbind(output, temp)
      
      
    } else if (i == 1) {
      
      output <- input[[i]][ locate.use(input[[i]], max.int ), parameter  ]
      output <- add.na(output, na.num)
      
    } else {
      
      temp <- input[[i]][ locate.use(input[[i]], max.int ), parameter  ]
      temp <- add.na(temp, na.num)
      
      output <- rbind(output, temp)
      
      
    }
    
  }
  
  return(output)
  
}

add.count <- function (x, rename = NULL) {
  
  require(tidyverse)
  
  count.data <- x %>%
    count()
  
  if (length(rename) == 0) {
    
    NULL
    
  } else {
    
    colnames(count.data)[grep("n", colnames(count.data)) ] <- rename
    
    
  }
  
  y <- full_join(x, count.data)
  return(y)
  
}

add.meta.weight <- function(df, estimate = "estimate", se = "se", group = NULL, random = T, na.rm = T) {
  
  df[, "within.study.variance"] <- NA
  df[, "within.study.variance"] <- df[, se]^2
  df[, "w"] <- NA
  df[, "w"] <- 1/df[, "within.study.variance"]
  
  # calculating weights
  if (random == F){
    
    df[, "weight"] <- NA
    df[,"weight"] <-  df[, "w"]
    
  } else if (random == T & is.null(group)){
    
    q <- c()
    q <- sum( df[, "w"] * (df[, estimate] - mean(df[, estimate], na.rm = na.rm))^2, na.rm = na.rm)
    dfree <- c()
    dfree <- sum(!is.na(df[, se])) - 1
    c <- c()
    c <- sum( df[, "w"], na.rm = na.rm) - sum(( df[, "w"])^2, na.rm = na.rm)/sum( df[, "w"], na.rm = na.rm)
    
    df[,"between.study.variance"] <- NA
    df[,"between.study.variance"] <- (q - dfree)/c
    
    df[,"weight"] <- NA
    df[,"weight"] <- 1/(df[ ,"within.study.variance"] + df[,"between.study.variance"])
    
  } else if (random == T & !is.null(group)){
    
    grp <- unique(df[, group])
    
    for (i in 1:length(grp)) {
      
      grp.row <- which(df[, group] == grp[i])
      
      q <- c()
      q <- sum(df[grp.row, "w"] * (df[grp.row, estimate] - mean(df[grp.row, estimate], na.rm = na.rm))^2, na.rm = na.rm)
      dfree <- c()
      dfree <- sum(!is.na(df[grp.row, se])) - 1
      c <- c()
      c <- sum(df[grp.row,"w"], na.rm = na.rm) - sum((df[grp.row,"w"])^2, na.rm = na.rm)/sum(df[grp.row,"w"], na.rm = na.rm)
      
      df[grp.row,"between.study.variance"] <- NA
      df[grp.row,"between.study.variance"] <- (q - dfree)/c
      
      df[grp.row,"weight"] <- NA
      df[grp.row,"weight"] <- 1/(df[grp.row, "within.study.variance"] + df[grp.row, "between.study.variance"])
      
    }
    
    
  } else {
    
    NULL
  }
  
  if (random == TRUE) {
    
    if (TRUE %in% (df[,"between.study.variance"] < 0)) {
      
      print("WARNING: There are negative between study variances")
      
    } else {
      
      NULL
    } 
  } else {NULL}
  
  df[,"weighted.est"] <- NA
  df[,"weighted.est"] <- df[,"weight"]*df[,estimate]
  
  return(df)
  
  
}

meta.sum <- function(meta.df, group = NULL, na.rm = T) {
  
  if (is.null(group)){
    
    result <- data.frame(
      
      estimate = sum(meta.df$weighted.est,na.rm = na.rm)/sum(meta.df$weight, na.rm = na.rm),
      
      se = sqrt(1/sum(meta.df$weight, na.rm = na.rm))
      
      
    )
    
    result$t <- result$estimate/result$se
    result$p = 2*(1-pnorm(result$t))
    
    
    
    return(result)
    
  } else {
    
    grp <- unique(meta.df[, group])
    
    estimate <- c()
    se <- c()
    
    for (i in 1:length(grp)) {
      
      grp.row <- which(meta.df[, group] == grp[i])
      
      estimate_temp <- sum(meta.df[grp.row, "weighted.est"], na.rm=na.rm)/sum(meta.df[grp.row,"weight"], na.rm=na.rm)
      
      estimate <- c(estimate, estimate_temp)
      
      se_temp <-  sqrt(1/sum(meta.df[grp.row, "weight"], na.rm = na.rm))
      
      se <- c(se, se_temp)
      
      
    }
    
    result <- data.frame(
      
      estimate = estimate,
      
      se = se,
      
      group = grp
      
    )
    
    result$t <- result$estimate/result$se
    result$p = 2*(1-pnorm(result$t))
    
    return(result)
    
    
  }
  
  
  
}

# Functions for reporting

report.p <- function(p){
  
  if (p >= .01 & p <= 1 & p > 0){
    
    return(paste("=", round(p, 3)))
    
  } else if (p < .01 & p >= .001 & p <= 1 & p > 0) {
    
    return(paste("=", round(p, 3)))
    
  } else if (p < .001 & p <= 1 & p > 0){
    
    return("< .001")
    
  } else if (p > 1 & p > 0) {
    
    print ("ERROR: p cannot be greater than 1")
    
  } else if (p < 0) {
    
    print("ERROR: p cannot be less than 0")
  } else {
    
    print("ERROR: invalid input. p has to be a number between 0 and 1")
    
  }
  
}


report <- function(x, dp = 2){
  return(as.vector(unlist(round(x, dp))))
}

report.ci <- function(x, se, cutoff = 1.96, dp = 2){
  return(paste0("[95% CI: ", report(x-cutoff*se, dp), " ", report(x+cutoff*se, dp),"]"   ) )
}


extract_coef <- function(df, var){
  labels <- row.names(df)
  output.vector <- df[, var]
  names(output.vector) <- labels
  return(output.vector)
}

bind_coef <- function(df.old, x.new, names){ # x is vector
  
  # initialise names 
  if(nrow(df.old)==0){
    temp.matrix <- matrix(nrow=1, ncol = length(names))
    df.old <- as.data.frame(temp.matrix)
    colnames(df.old) <- names
    
    new.names <- names(x.new)
    current.row <- nrow(df.old)
  } else {
    new.names <- names(x.new)
    # initialise new row
    df.old[nrow(df.old)+1, ] <- NA 
    current.row <- nrow(df.old)
  }
  
  for(i in 1:length(new.names)){
    if(new.names[i] %in% names){
      df.old[current.row, new.names[i]] <- x.new[i]
    }
  }
  
  return(df.old)
  
}
