
### This function is for extracting dataframes from sql server

extractData <- function(time = "201812"){
  
  # library requirement: 'DBI', 'odbc'
  # input:
  #   time: character string indicating the request time
  # return:
  #   A list of four dataframes:
  #   1. strat: MOC_MEMBER_STRAT at the request time
  #   2. history: MOC_MEMBER_HISTORY at the request time
  #   3. hcc: MOC_FINAL_HCCS at the request time
  #   4. strat_next: MOC_MEMBER_STRAT at a year after the request time
  
  ### set up database connection
  con_MOC <- DBI::dbConnect(
    odbc::odbc(), 
    Driver = "SQL Server Native Client 11.0",
    Server = "searepl01", 
    Database = "MOC", 
    trusted_connection="yes"
  )
  
  ### get timestamp of next year
  next_time = as.character( as.numeric(time) + 100 )
  
  ### extract data
  strat <- dbGetQuery(
    con_MOC, 
    paste0('SELECT * FROM MOC_MEMBER_STRAT WHERE RUN_MO = ', time)
  )
  
  history <- dbGetQuery(
    con_MOC, 
    paste0('SELECT * FROM MOC_MEMBER_HISTORY WHERE calendar_YM = ', time)
  )
  
  hcc <- dbGetQuery(
    con_MOC, 
    paste0('SELECT * FROM MOC_FINAL_HCCS WHERE RUN_MO = ', time)
  )
  
  strat_next <- dbGetQuery(
    con_MOC, 
    paste0('SELECT * FROM MOC_MEMBER_STRAT WHERE RUN_MO = ', next_time)
  )
  
  ### combine results
  df_lst <- list('strat' = strat, 
                 'history' = history, 
                 'hcc' = hcc, 
                 'strat_next' = strat_next)
  
  return(df_lst)
  
}



### This function is for cleaning the data and joining the tables

transformData <- function(df_lst, to_drop_strat, to_drop_history){
  
  # library requirement: 'dplyr', 'maditr', 'fastDummies'
  # input:
  #   df_lst: a list of dataframes, returned by function extratData()
  #   to_drop_strat: a array of character strings indicating 
  #                  columns to drop from strat dataframe
  #   to_drop_history: a array of character strings indicating 
  #                    columns to drop from history dataframe
  # return:
  #   df: dataframe containing all information for modeling
  
  ### drop specified columns:
  df_lst$strat <- df_lst$strat[, !names(df_lst$strat) %in% to_drop_strat]
  df_lst$history <- df_lst$history[, !names(df_lst$history) %in% to_drop_history]
  
  ### drop empty columns:
  df_lst$strat <- df_lst$strat[, colSums(is.na(df_lst$strat)) < nrow(df_lst$strat)]
  df_lst$history <- df_lst$history[, colSums(is.na(df_lst$history)) < nrow(df_lst$history)]
  
  ### get next year's outcome
  df_lst$strat_next$IP_IND <- ifelse(df_lst$strat_next$IP_CT == 0, 0, 1)
  df_lst$strat_next <- df_lst$strat_next[,c('MBR_NO', 'IP_IND')]
  
  ### join two year's strat into df
  df <- inner_join(df_lst$strat, df_lst$strat_next, by = 'MBR_NO')
  
  ### join df and history
  df <- inner_join(df, df_lst$history, by = c("MBR_NO" = "Mbr_No"))
  
  ### drop observation with unknown sex
  df <- df[df$sex %in% c('F','M'),]
  
  ### group languages
  df$Language <- ifelse(df$Language %in% c('English','Spanish'), df$Language, 'Other')
  
  ### group hcc into categories
  df_lst$hcc <- df_lst$hcc %>% mutate(
    hcc_cat = case_when(
      HCC %in% 1:6 ~ 'Infectious',
      HCC %in% 8:12 ~ 'Cancer',
      HCC %in% 17:19 ~ 'Diabetes',
      HCC %in% 21:23 ~ 'Metabolic',
      HCC %in% 27:29 ~ 'Liver',
      HCC %in% c(33:35,188) ~ 'Digestive',
      HCC %in% c(39,40,166:173) ~ 'Skeletal',
      HCC %in% 46:48 ~ 'Hematological',
      HCC %in% c(51,52,57:60) ~ 'Mental',
      HCC %in% 54:56 ~ 'Substance',
      HCC %in% 70:80 ~ 'CentralNervous',
      HCC %in% c(82:84,110:115,999) ~ 'Respiratory',
      HCC %in% 85:96 ~ 'Cardiac',
      HCC %in% 99:104 ~ 'Cerebrovascular',
      HCC %in% 106:108 ~ 'Vascular',
      HCC %in% 122:124 ~ 'Ophthalmology',
      HCC %in% 134:138 ~ 'Kidney',
      HCC %in% 157:162 ~ 'Skin',
      HCC %in% c(176,186) ~ 'ImplantTransplant',
      HCC == 189 ~ 'Amputation'
     )
  )
  
  ### turn hcc into wide format
  hcc_wide <- dcast(df_lst$hcc, MEMBER_NO ~ hcc_cat, fun = length)
  
  ### mark the start column of hcc then join df and hcc 
  start <- ncol(df)+1
  df <- left_join(df , hcc_wide, by = c("MBR_NO" = "MEMBER_NO"))
  
  ### fill in the NA's after joining with hcc
  end = ncol(df)
  df[, start:end][is.na(df[, start:end])] <- 0
  
  ### create dummy variables for all categorical variable
  dummy_col <- names(df)[sapply(df, class) == 'character'][-1]
  df <- dummy_cols(df, 
                   select_columns = dummy_col,
                   remove_selected_columns = TRUE)
  
  return(df)  

}



### This function is for BART tuning with grid search and 
### cross validation, with a evaluation metric of user choice
### (bartMachineCV does not have the option of changing metric)

bartCV <- function(X, y, params, k = 5, eval_metric = 'f1', verbose = FALSE){
  
  # library requirement: 'caret', 'dplyr', bartMachine'
  # input: 
  #   X: dataframe, training data
  #   y: vector, training outcome/label
  #   params: a list of hyperparameters: k and m
  #   k: numeric, number of folds
  #   eval_matric: the evaluation metric to maximize
  #                !!!Note: current version only have f1-score defined
  # return:
  #   scores: a vector contain the cv scores in all k folds
  
  ### creating folds 
  folds <- createFolds(y = y, k = k, list = FALSE)
  X$folds <- folds
  y <- data.frame(y = y, folds = folds)
  
  scores <- rep(0, k)
  
  for (i in 1:k){
    X_train_cv <- X %>% filter(folds != i)
    X_test_cv <- X %>% filter(folds == i)
    y_train_cv <- (y %>% filter(folds != i))$y
    y_test_cv <- (y %>% filter(folds == i))$y
    
    if (verbose==TRUE){
      print(sprintf('running at fold %i', i))
    }
    
    # train model
    bart.cv <- bartMachine(X = X_train_cv, 
                           y = y_train_cv,
                           k = params$k,
                           num_trees = params$m,
                           run_in_sample = FALSE,
                           mem_cache_for_speed = FALSE,
                           flush_indices_to_save_RAM = TRUE,
                           verbose = FALSE
    )
    
    # predict on test
    pred <- suppressMessages(predict(bart.cv, new_data = X_test_cv))
    label <- factor(as.numeric(pred >= quantile(pred, 1-mean(y_test_cv==1))), c(1,0))
    
    # calculate score
    TP <- mean(y_test_cv==1 & label==1)
    FP <- mean(y_test_cv==0 & label==1)
    FN <- mean(y_test_cv==1 & label==0)
    
    if (eval_metric == 'f1'){
      scores[i] <- TP / ( TP + (FP+FN)/2 )
    }
    
  }
  
  return(scores)
  
}



### This function is for grid search and cross validation 
### for fine tuning the BART model

bartGridSearch <- function(X, y, param_grid, k = 5, eval_metric = 'f1', verbose = TRUE){
  
  # library requirement: 'caret', 'dplyr', bartMachine'
  # function calls: bartCV
  # input: 
  #   X: dataframe, training data
  #   y: vector, training outcome/label
  #   param_grid: a list of vectors containing choices of candidate hyperparameter k and m
  #   k: numeric, number of folds
  #   eval_matric: the evaluation metric to maximize
  #   verbose: if True, the progress will be printed while executing
  # return:
  #   GS.res: a table containing combinations of parameter values and cv score
  
  ### expand parameter grid
  param_lst <- expand.grid(param_grid)
  n = nrow(param_lst)
  
  ### preallocation
  cv_scores <- rep(0, nrow(param_lst))
  
  if (verbose==TRUE){
    print(sprintf("grid search on a total of %i combinations, %i models", n, k*n))
  }
  
  ### grid search by nested loop
  for (i in 1:nrow(param_lst)){
    params <- list(k = param_lst[i,1], m = param_lst[i,2])
    
    # print progress
    if (verbose==TRUE){
      print(sprintf("cross validation on k = %i, m = %i", params$k, params$m))
    }
    
    # CV
    scores <- bartCV(X, y, params, k = k, eval_metric = eval_metric, verbose = verbose)
    
    # store average score
    cv_scores[i] <- mean(scores)
    
  }
  
  ### combining results
  GS.res <- cbind(param_lst, cv_scores)
  
  print('All done!')
  
  return(GS.res)
  
}



### This function is for the data massaging part in CND

cndMassageData <- function(data, pred.p){
  
  # library requirement: 'dplyr', 'knitr'
  # input:
  #   data: the dataframe that needs massaging, i.e. the training data
  # return:
  #   CND.res: a list of two dataframes:
  #   1. data.new: massaged data
  #   2. race.comparison: a detailed table for race distribuion
  
  ### investigate the racial bias within training data
  race.cnt.overall <- sapply(select(data,contains("race")), sum)
  race.prop.overall <- sapply(select(data,contains("race")), mean)
  race.cnt.IP <- sapply(select(data[data$IP_IND == 1,],contains("race")), sum)
  race.expected.cnt.IP <- round(race.prop.overall * sum(data$IP_IND))
  diff <- race.cnt.IP - race.expected.cnt.IP
  race.comparison <- data.frame(overall.cnt = race.cnt.overall,
                                overall.prop = race.prop.overall,
                                IP.cnt = race.cnt.IP,
                                IP.expected.cnt = race.expected.cnt.IP,
                                diff = diff)
  
  ### get promotion and demotion group
  profiter <- race.comparison %>% filter(diff > 0) %>% select(diff)
  victim <- race.comparison %>% filter(diff < 0) %>% select(diff)
  
  ### label swapping
  # copy the data and attach predicted probability
  data.new <- data
  data.new$pred.p <- pred.p
  
  ### change labels from demotion group from 1 to 0
  # loop through all profiter races
  
  for (i in 1:nrow(profiter)){
    
    # column index for the race
    col_idx <- which(names(data.new) == rownames(profiter)[i]) 
    
    # number of swap needed for this race
    count <- profiter[i,] 
    
    # row index for demotion group
    row_idx <- which(data.new[,col_idx] == 1 & data.new$IP_IND == 1) 
    
    # set a new column and copy predicted probability only for promotion group
    data.new$use.p <- 1
    data.new$use.p[row_idx] <- data.new$pred.p[row_idx]
    
    # row index for demotion group member with top predicted probability
    swap_idx <- sort(data.new$use.p, index.return=TRUE, decreasing=FALSE)[["ix"]][1:count]  
    
    # change label from 1 to 0
    if (length(swap_idx) == count & sum(data.new[swap_idx,]$IP_IND) == count){
      data.new[swap_idx,]$IP_IND = 0
    } else {
      stop('error in CND')
    }
  }
  
  ### change labels from demotion group from 0 to 1
  # loop through all victim races
  
  for (i in 1:nrow(victim)){
    
    # column index for the race
    col_idx <- which(names(data.new) == rownames(victim)[i])
    
    # number of swap needed for this race
    count <- abs(victim[i,])
    
    # row index for promotion group
    row_idx <- which(data.new[,col_idx] == 1 & data.new$IP_IND == 0)
    
    # set a new column and copy predicted probability only for demotion group
    data.new$use.p <- 0 
    data.new$use.p[row_idx] <- data.new$pred.p[row_idx]
    
    # row index for demotion group member with top predicted probability
    swap_idx <- sort(data.new$use.p, index.return=TRUE, decreasing=TRUE)[["ix"]][1:count]
    
    # change label from 0 to 1
    if (length(swap_idx) == count & sum(data.new[swap_idx,]$IP_IND) == 0){
      data.new[swap_idx,]$IP_IND = 1
    } else {
      stop('error in CND')
    }
  }
  
  # drop the pred.p and use.p columns
  data.new <- data.new %>% select(-pred.p, -use.p)
  
  ### count in each race in the corrected
  race.corrected.cnt.IP <- sapply(select(data.new[data.new$IP_IND == 1,],contains("race")), sum)
  race.comparison <- cbind(race.comparison, IP.corrected.cnt = race.corrected.cnt.IP)
  race.comparison <- rbind(race.comparison, Total = colSums(race.comparison))
  
  CND.res <- list('data.new' = data.new, 'race.comparison' = race.comparison)
  
  return(CND.res)
  
}



### this function is for comparing the prediction result before and after 
### applying the CND technique in terms of racial bias 

compareRaceCND <- function(data, pred.before, pred.after){
  
  # library requirement: 'dplyr'
  # input: 
  #   data: the dataframe used for evaluation
  #   pred.before: vector of predicted label before CND
  #   pred.after: vector of predicted label after CND
  
  ### attching predictions to data
  data$before <- pred.before
  data$after <- pred.after
  
  ### actual count, expected count, and predicted count by race
  overall <- sapply(select(data,contains("race")), sum)
  prop <- sapply(select(data,contains("race")), mean)
  actual.cnt <- sapply(select(data[data$IP_IND == 1,],contains("race")), sum)
  expected.cnt <- round(prop * sum(data$IP_IND))
  before <- sapply(select(data[data$before == 1,],contains("race")), sum)
  after <- sapply(select(data[data$after == 1,],contains("race")), sum)
  race.comparison <- data.frame(overall = overall,
                                prop = prop,
                                actual = actual.cnt,
                                expected = expected.cnt,
                                before = before,
                                after = after)
  races <- c(substring(row.names(race.comparison), 6, 50), 'Total')
  race.comparison <- rbind(race.comparison, Total = colSums(race.comparison))
  race.comparison <- cbind(races, race.comparison)
  row.names(race.comparison) <- c()
  
  print(races)
  return(race.comparison)

}


### this function is for estimating confidence intervals for evaluation metrics
metricCI <- function(conf.mat){

  sens_errors <- sqrt(sensitivity(conf.mat) * (1 - sensitivity(conf.mat)) / sum(conf.mat[,1]))
  sensCI <- sensitivity(conf.mat) + c(-1.96, 1.96) * sens_errors
  spec_errors <- sqrt(specificity(conf.mat) * (1 - specificity(conf.mat)) / sum(conf.mat[,1]))
  specCI <- specificity(conf.mat) + c(-1.96, 1.96) * spec_errors
  ppv_errors <- sqrt(posPredValue(conf.mat) * (1 - posPredValue(conf.mat)) / sum(conf.mat[,1]))
  ppvCI <- posPredValue(conf.mat) + c(-1.96, 1.96) * ppv_errors
  npv_errors <- sqrt(negPredValue(conf.mat) * (1 - negPredValue(conf.mat)) / sum(conf.mat[,1]))
  npvCI <- negPredValue(conf.mat) + c(-1.96, 1.96) * npv_errors
  
  res <- data.frame(sensCI, specCI, ppvCI, npvCI, row.names = c('lower','upper'))
  
  return(res)
}
