## Importing libraries ----

# Identifying system cores
ncores <<- paste0("-j", parallel::detectCores())
Sys.setenv(MAKEFLAGS = ncores)

loadLibrary <- function(lib = 'dplyr', mirror = 'https://cran.ma.imperial.ac.uk/'){
  # Function to load or install libraries
  # lib: Specifies the library name
  # mirror: Specifies the mirror page
  
  if(!require(lib, character.only = TRUE)){
    install.packages(lib, repos = mirror)
    library(lib, character.only = TRUE)
  } else{library(lib, character.only = TRUE)}
  
}

# Loading up the required functions
loadLibrary('dplyr')
loadLibrary('lubridate')
loadLibrary('readr')
loadLibrary('splitstackshape')
loadLibrary('stringr')
loadLibrary('splines')
loadLibrary('caret')

## Main functions ----
VVfitClassificationModels <- function(df_train,
                                      df_test,
                                      targ_var,
                                      train_control = trainControl(method = "cv", number = 5, sampling = "down"),
                                      degree_of_numerical = 1,
                                      models_to_test      = c("earth", "adaboost", "svmLinear3", "bayesglm", "rf", "xgbDART")
                                      ){
  # This function takes as input tow pre-processed training and testing dataframes with one target variable, fits various models and reports their accuracy
  # df_train: training dataset
  # df_test: testing dataset
  # target_var: name of the target variable
  # train_control: the train control
  # degree_of_numerical: this function will creates b-splines out of the numerical columns. This specifies the degree 
  # models_to_test: list of models

  # Variable types
  num_vars  <- names(df_train)[sapply(df_train, is.numeric) & !names(df_train) %in% targ_var]
  cat_vars  <- names(df_train)[sapply(df_train, is.character) | sapply(df_train, is.factor)]
  
  # Formula
  form      <- paste0(paste0(targ_var," ~ "),paste('bs(',num_vars,', degree = ',degree_of_numerical,')', collapse = ' + '), " + ", paste(cat_vars, collapse = ' + '))
  
  # Training all models
  for (i in 1:length(models_to_test)){
    
    print(paste0("Fitting model: ", models_to_test[i]))
    
    try(
      eval(parse(text = paste0(
        
        "model_",models_to_test[i]," <- train(",form,", 
        data      = df_train, 
        method    = '",models_to_test[i],"', 
        trControl = train_control)"
        
        
      )))
    
    )
    
  }
  
  # Reporting on accuracy
  if (!is.null(df_test)){
    
    eval(parse(text = paste0(
      
      "raw_report <- data.frame(actual_values = df_test$",targ_var,")"
      
    )))
    
    
    raw_report$actual_values <- as.character(raw_report$actual_values)
    
    for (i in 1:length(models_to_test)){
      
      if (!exists(paste0("model_",models_to_test[i]))){
        
        next  
        
      }else{
        
        try(
          eval(parse(text = paste0(
            
            
            "
            dummy                  <- as.character(predict(",paste0("model_",models_to_test[i]),",
                                      newdata = df_test, type = 'raw'))
            
            raw_report <- cbind(raw_report, dummy)
            
            names(raw_report)[length(names(raw_report))] <- '",models_to_test[i],"'
            
            "
            
          )))
          
        )
        
      }
      
    }
    
    raw_acc  <- raw_report %>% mutate_all(~(accuracy = sum(. == raw_report$actual_values, na.rm = T)/nrow(raw_report))) %>% select(-1) %>% slice(1)

  }
  
  print(raw_acc)

  return(raw_acc)
  
}

VVfitRegressionModels     <- function(df_train,
                                      df_test,
                                      targ_var,
                                      train_control = trainControl(method = "cv", number = 5),
                                      degree_of_numerical = 1,
                                      log_response        = TRUE,
                                      models_to_test      = c("lm", "lasso", "enet", "svmLinear2", "rf")){
  # This function takes as input tow pre-processed training and testing dataframes with one target variable, fits various models and reports their accuracy
  # df_train: training dataset
  # df_test: testing dataset
  # target_var: name of the target variable
  # train_control: the train control
  # log_response: take the natural log of the response variable or not
  # degree_of_numerical: this function will creates b-splines out of the numerical columns. This specifies the degree 
  # models_to_test: list of models
  
  # variable types
  num_vars  <- names(df_train)[sapply(df_train, is.numeric) & !names(df_train) %in% targ_var]
  cat_vars  <- names(df_train)[sapply(df_train, is.character) | sapply(df_train, is.factor)]
  
  # Formula
  if (log_response){
    
    form      <- paste0(paste0("log(",targ_var,") ~ "),paste('bs(',num_vars,', degree = ',degree_of_numerical,')', collapse = ' + '), " + ", paste(cat_vars, collapse = ' + '))
    
  }else{
    
    form      <- paste0(paste0(targ_var," ~ "),paste('bs(',num_vars,', degree = ',degree_of_numerical,')', collapse = ' + '), " + ", paste(cat_vars, collapse = ' + '))
    
  }
  
  # Training all models
  for (i in 1:length(models_to_test)){
    
    print(paste0("Fitting model: ", models_to_test[i]))
    
    try(
      eval(parse(text = paste0(
        
        "model_",models_to_test[i]," <- train(",form,", 
        data      = df_train, 
        method    = '",models_to_test[i],"', 
        trControl = train_control)"
        
        
      )))
    )
    
  }
  
    # Reporting on MSE
    eval(parse(text = paste0(
      
      "raw_report <- data.frame(actual_values = df_test$",targ_var,")"
      
    )))
    
    for (i in 1:length(models_to_test)){
      
      if (!exists(paste0("model_",models_to_test[i]))){
        
        next  
        
      }else{
        
        try(
          
          if(log_response){
          
            eval(parse(text = paste0(
              
              "
              
              res        <- log(df_train$",targ_var,") - predict(model_",models_to_test[i],", df_train, type = 'raw')
      
              dummy      <- exp(predict(model_",models_to_test[i],", df_test, type = 'raw')) * mean(exp(res)) ## Because: https://stats.stackexchange.com/questions/55692/back-transformation-of-an-mlr-model/58077#58077
      
              raw_report <- cbind(raw_report, dummy)
              
              "
            )))
              
          }else{
            
            eval(parse(text = paste0(
            
              "
              
              dummy      <- predict(model_",models_to_test[i],", df_test, type = 'raw')
      
              raw_report <- cbind(raw_report, dummy)
              
              "
              
            )))
            
          }
          
        )
        
      }
      
    }
    
  names(raw_report)[2:length(names(raw_report))] <- models_to_test
    
  raw_acc  <- raw_report %>% mutate_all(~(mse = mean((. - raw_report$actual_values)^2))) %>% select(-1) %>% slice(1)
  
  print(raw_acc)  
    
  return(raw_acc)
  
}

## Helper/Secondary functions ----
VVCleanLowFreqVars  <-  function(x, thres = 500, set_as_ref_cat = T, 
                                   merge = T, exclPKs = T, default_value = "Not_Speficied_Other_Low_Frequencies"){
  # This function will take care of the records with categories of low frequency by replacing them with a default value
  # thres: is the threshold of frequency, meaning that any category with a frequency below that threshold will be recoded
  # set_as_ref_cat: will set up the new category as reference category
  # exclPKs: will not do anything if the each row is unique
  # merge: will merge the NAs and other together with the low frequencies as well
  # default_value: is the value that the problematic records will be replaced with
  
  if (exclPKs){
    
    if (length(x) != length(unique(x))){
      
      # Start with a character
      x <- as.character(x)
      
      if (merge){
        
        # Array of records that will be merged
        records_to_merge <- c("0", "unknown", "", "-", "others", "other", "n/a")
        
        x[tolower(iconv(x, from = "UTF-8", to = "latin1")) %in% 
            records_to_merge | is.na(x)] <- default_value
      
      }
      
      # Merging small categories
      to_change           <- names(table(x)[table(x) < thres])
      x[x %in% to_change] <- default_value
      
      # From character to factor  
      x                   <- factor(x)
      
      # Setting the "Other" as our reference category
      if (set_as_ref_cat){
        
        if (default_value %in% levels(x)){
          
          # Setting as reference category
          x               <- relevel(factor(x), ref = default_value)
          
        }
        
      }
      
    }
    
  }
  
  return(x)
  
}

VVCleanSingleQuotations <- function(x, lower = T){
  # Single quotations can be a pain as they all look similar but they are not.
  # This function will clean up the annoying single quotations caused by bad formatting
  # lower: will make also everything lowercase
  
  if (lower){
    
    x <- ifelse(is.na(x), NA, trimws(gsub("'","\\'",tolower(iconv(x, from = "UTF-8", to = "latin1")), fixed = T)))
    
  }else{
    
    x <- ifelse(is.na(x), NA, trimws(gsub("'","\\'",iconv(x, from = "UTF-8", to = "latin1"), fixed = T)))
    
  }
  
  return(x)
  
}


VVCleanNonNumericalVars <- function(df, threshold = 500, set_as_ref_cat = T, merge_With_NA = T, lower = F){
  # This function will apply the VVCleanSingleQuotations and VVCleanSmallFreqVars to the whole dataframe
  # This function will also clean columns that have two unique values only and will make them factors
  # threshold: is the threshold of frequency, meaning that any category with a frequency below that threshold will be recoded
  # set_as_ref_cat: will set up the new category as reference category
  # merge: will merge_With_NA the NAs and other together with the low frequencies as well
  # lower: will make everything lowercase
  
  df <- df                                                                                          %>%
        mutate_if(is.factor, as.character)
  
  # Cleans all the categorical variables that are not dates/ids
  to_clean <- names(df)[c(sapply(df, class)) == "character"]
  to_clean <- to_clean[!grepl("id", tolower(to_clean))]
  to_clean <- to_clean[!grepl("date", tolower(to_clean))]
  
  df <- df                                                                                                                        %>% 
        mutate_at(., to_clean, ~VVCleanSingleQuotations(., lower))                                                                %>%
        mutate_at(., to_clean, ~VVCleanLowFreqVars(., thres = threshold, set_as_ref_cat = set_as_ref_cat, merge = merge_With_NA)) %>%
        mutate_if(., ~(length(unique(.)) == 2), as.factor)
  
  return(df)
  
}


VVCleanNames <- function(df){
  # This function cleans the dataframe's column names
  
  names(df)               <- gsub("[[:punct:]]", " ", names(df))
  names(df)               <- gsub("  ", " ", names(df))
  names(df)               <- gsub(" ", "_", names(df))
  names(df)               <- gsub("__", "_", names(df))
  
  return(df)
  
}

VVReduceModelSize <- function(model){
  # This function will reduce the size of a model produced by the caret library for better portability 
  
  model$residuals         <- NULL
  model$weights           <- NULL
  model$fitted.values     <- NULL
  model$prior.weights     <- NULL
  model$na.action         <- NULL
  model$linear.predictors <- NULL
  model$fitted.values     <- NULL
  model$effects           <- NULL
  model$data              <- NULL
  model$model             <- NULL
  model$y                 <- NULL
  model$qr$qr             <- NULL
  model$trainingData      <- NULL
  
  model$finalModel$predicted <- NULL 
  model$finalModel$oob.times <- NULL 
  model$finalModel$y         <- NULL
  model$finalModel$votes     <- NULL
  model$control$indexOut     <- NULL
  model$control$index        <- NULL
  model$trainingData         <- NULL
  
  return(model)
  
}

VVOmitLowVarColumns <- function(df, threshold = 1, exempt_dates_ids = T){
  # This function will subset the dataframe, excluding the columns that don't have enough variability (unique values) and thus will add nothing to the model
  # threshold: This is the threshold that anything below that will be excluded. 
  # exempt_dates_ids: This will exempt columns that include the term "id" or "date"
  # This function exclude the columns that include NAs
  
  
  # Finding columns that have NAs
  na_cols           <- which(sapply(df, function(x) any(is.na(x))))
  
  if (length(na_cols) > 0){
    
    cat("Excluding columns with NAs:", "\n \n", paste0(names(na_cols), collapse = "\n"))
    df                <- df[,-na_cols]
    
  }
  
  
  # Findings the low frequency variables
  not_enough_var    <- names(sapply(df, function(x) length(unique(x))))[sapply(df, function(x) length(unique(x))) <= threshold]
  
  # We dont' process dates and ids
  if(exempt_dates_ids){
    
    not_enough_var  <- not_enough_var[!grepl("id", tolower(not_enough_var))]
    not_enough_var  <- not_enough_var[!grepl("date", tolower(not_enough_var))]
    
  }
  
  if (length(not_enough_var) > 0){
    
    cat("\n","Excluding columns with not enough variability:", "\n \n", paste0(not_enough_var, collapse = "\n"))
    df              <- df[, !names(df) %in% not_enough_var]
    
  }
  
  return(df)
  
}

VVUnnestToBinaryColumns <- function(dfr, threshold_pc = 0.175, threshold_categories = 50, returnNumeric = FALSE, sep = ';'){
  # This function takes a dataframe as an input, identifies which fields have nested values (include the separator) and unnests them, creating new binary fields
  # sep: this is the separator that separates the nested values. It's usually a ; or a , or a |
  # threshold_pc: this is a percentage that will handle exceptions. Any column that the frequency of the separator is below that threshold will not be processed
  # threshold_categories: We usually don't want to add a column for every single value, just the ones that have enough frequencies. Anything below that threshold will be merged into the "Other" ategory
  # returnNumeric: return 1/0 or TRUE/FALSE
  
  lookup                                   <- sapply(dfr, function(y) sum(grepl(paste0(sep),y)))/nrow(dfr)
  to_split                                 <- names(dfr)[lookup >= threshold_pc]
  
  df_to_not_split                          <- dfr[,!names(dfr) %in% to_split]
  df_to_split                              <- dfr[,names(dfr) %in% to_split]
  df_to_split                              <- df_to_split %>% mutate_if(is.factor, as.character)
  
  df_split <- data.frame(id = 1:nrow(df_to_split))
  
  for (col in names(df_to_split)){
    
    vectorised         <- unlist(c(df_to_split[,which(names(df_to_split) == col)]))
    comma_split        <- strsplit(vectorised, split = paste0(sep))
    tags               <- unique(str_trim(unlist(comma_split)))
    dummy_df           <- as.data.frame(Reduce(cbind, lapply(tags, function(i) sapply(comma_split, function(j) +(any(grepl(i, j), na.rm = TRUE))))))
    rownames(dummy_df) <- 1:nrow(dummy_df)
    names(dummy_df)    <- paste0(col,"_",tags)
    dummy_df           <- dummy_df %>% select(-contains("_NA"))
    
    frequencies        <- apply(dummy_df, 2, sum)
    
    if (any(frequencies < threshold_categories)){
      
      big_categores    <- dummy_df[,frequencies >= threshold_categories]
      small_categories <- dummy_df[,frequencies <  threshold_categories]
      
      if (!"integer" %in% class(small_categories)){
        
        Other            <- apply(small_categories, 1, sum)
        dummy_df         <- cbind(big_categores, Other)
        
        names(dummy_df)[length(names(dummy_df))] <- paste0(col,"_Other")
        
      }
      
    }
    
    df_split           <- cbind(df_split, dummy_df)
    
  }
  
  df_split   <- df_split[,colSums(df_split) < nrow(df_split)]
  
  if (returnNumeric == F & nrow(df_split) > 0 & ncol(df_split) > 0){
    
    df_split <- df_split %>% mutate_all(~(
      recode(., 
             `1`         = TRUE,
             `0`         = FALSE,
             .default    = FALSE
      )))
    
  }
  
  to_return  <- cbind(df_to_not_split, df_split)
  
  return(to_return)
  
}

VVAlignTrainTestFactors <- function(training, 
                                  testing,
                                  exempt_dates_ids = T,
                                  default_value = "Not_Speficied_Other_Low_Frequencies"){
  # This function handles the usual case when the categorical variables of the testing dataset has different values
  # compared to the respective variables in the testing dataset. The "disagreeing records" will get the default value
  # default_value: The value that the disagreeing records will get
  # exempt_dates_ids: This will exempt columns that include the term "id" or "date"
  
  training <- data.frame(training)
  testing  <- data.frame(testing)
  
  # Making sure they have the same column names and column order
  df <- testing[,names(testing) %in% names(training)]
  df <- setcolorder(df, names(training)[names(training) %in% names(df)])
  
  # We'll be working with factors
  df <- df %>% mutate_if(is_character, as.factor) %>% select_if(is.factor)
  
  # Usual stuff
  if (exempt_dates_ids){
    
    df <- df[names(df)[!grepl("id|date", tolower(names(df)))]]

  }
  
  training             <- training %>% mutate_at(vars(names(df)), as.factor)  
  ncols                <- length(names(df))
  
  for (col in 1:ncols){
    
    ref <- names(df)[col]
    
    if (any(table(training[,ref]) == 0)) {
      
      # Warning message
      print(paste0("Please Check column ", ref, 
                   " in training, levels: ", names(which(table(training[,ref]) == 0)), " have 0 frequency"))
    }                
    if (class(training[,ref]) == "factor" & any(!levels(df[,col]) %in% levels(training[,ref]))){
      
      # Finding levels that exist in testing but not in training
      to_clean <- levels(df[,col])[!levels(df[,col]) %in% levels(training[,ref])]
      print(paste0("Cleaning ", ref, ": setting ", sum(to_clean %in% df[,col]) , " observations to ", default_value))
      
      # Recoding
      levels(df[,col])[!levels(df[,col]) %in% levels(training[,ref])] <- default_value
      
    }
    
  }
  
  testing[names(df)] <- df
  
  return(testing)
  
}