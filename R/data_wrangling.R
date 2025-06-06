
MetaDataWranglingFunctions <- shinymeta::metaAction({
  
  #' Convert data to wide format. Returns data frame unchanged if already in wide format.
  #'
  #' @param data Data frame to convert.
  #'
  #' @return Data in wide format.
  Long2Wide <- function(data) {
    TempData <- as.data.frame(data)
    if (ncol(TempData)==6 | ncol(TempData)==5){ #long format
      TempData<-TempData[order(TempData$StudyID, TempData$T), ]
      TempData$Arms<- ave(as.numeric(TempData$StudyID),TempData$StudyID,FUN=seq_along)  # create counting variable for number of arms within each study.
      data_wide <- reshape(TempData, timevar = "Arms",idvar = c("Study", "StudyID"), direction = "wide") # reshape
    }
    else {
      data_wide<- TempData
    }
  }
  
  #' Convert data to long format. Returns data frame unchanged if already in long format.
  #'
  #' @param data Data frame to convert.
  #'
  #' @return Data in long format.
  Wide2Long <- function(data) {
    TempData <- as.data.frame(data)
    if (ncol(TempData)==6 | ncol(TempData)==5) {
      data_long <- TempData
    } else {                # wide format
      TempData <- TempData[order(TempData$StudyID), ]
      data_long <- reshape(TempData, idvar = c("Study", "StudyID"), direction = "long", varying = 3:ncol(TempData))
      data_long <- data_long[!is.na(data_long$T), ]
      data_long <- data_long[order(data_long$StudyID, data_long$T), ]
    }
  }
  
  #' Swap treatment and control as necessary when in wide format.
  #'
  #' @param CONBI Type of data. One of: 'continuous' or 'binary'
  #' @param data Data frame in wide format
  #' @param trt Intended reference treatment
  #'
  #' @return Corrected data frame
  SwapTrt <- function(CONBI, data, trt) {
    "# different variables need swapping"
    if (CONBI == 'continuous') {
      list_vars <- c("T", "N", "Mean", "SD")
    } else {
      list_vars <- c("T", "N", "R")
    }
    "# need to check for each study"
    for (i in 1:nrow(data)) {
      "# if the study data needs swapping"
      if (data$T.1[i] != trt) {
        "# complete the swaps for each variable"
        for(var in list_vars) {
          data[i, paste0(var, ".", 1:2)] <- data[i, paste0(var, ".", 2:1)]
        }
      }
    }
    return(data)
  }
  
  #' Remove leading and trailing whitespace and collapse multiple whitespace characters between words.
  #' 
  #' @param data Data frame to clean
  #' @return Cleaned data frame
  CleanData <- function(data) {
    return(dplyr::mutate(data, across(where(is.character), stringr::str_squish)))
  }
  
  #' Rename the columns of a data frame to match the expected letter casing.
  #'
  #' @param data Data frame to fix
  #'
  #' @return Data frame with renamed columns.
  .FixColumnNameCases <- function(data) {
    corrected_names <- unlist(
      sapply(
        names(data),
        function (name) {
          return(.CorrectColumnName(name, meta_pairwise_column_names))
        }
      )
    )
    
    names(data) <- corrected_names
    return(data)
  }
  
  #' Correct a column name to match the expected letter casing.
  #'
  #' @param original_name Column name to fix
  #' @param column_names Named vector where each name is a regular expression to match, and the value is the replacement string.
  #'
  #' @return The corrected column name.
  .CorrectColumnName <- function(original_name, column_names) {
    matches <- unlist(
      sapply(
        column_names$pattern,
        function(pattern) {
          if (length(grep(pattern, original_name)) > 0) {
            column_names$replacement[column_names$pattern == pattern]
          } else {
            NULL
          }
        }
      )
    )
    
    if (length(matches) > 0) {
      return(
        sub(
          names(matches)[1],
          matches[1],
          original_name
        )
      )
    }
    
    return(original_name)
  }
  
  #' Add a new column in the data for study IDs, both for long and wide formats.
  #' 
  #' @param data Data frame in which to search for treatment names
  #' @return Vector of all treatment names
  .AddStudyIds <- function(data) {
    study_names <- unique(data$Study)
    
    "# Add study IDs to data frame"
    "# data$StudyID <- match(data$Study, study_names)"
    data <- cbind(StudyID = match(data$Study, study_names), data)
    
    return(data)
  }
  
  #' Wrangle the uploaded data into a form usable by the internals of the app, both for long and wide formats.
  #' 
  #' @param data Data frame to wrangle
  #' @return Data frame which is usable by the rest of the app
  WrangleUploadData <- function(data) {
    new_df <- data %>%
      .FixColumnNameCases() %>%
      .AddStudyIds()
    
    return(new_df)
  }
  
  #' Find the treatments compared in a given study.
  #'
  #' @param data Data frame in which to find treatments.
  #' @param study_name Name of study for which to find treatments.
  #'
  #' @return Vector of treatments in the given study.
  FindTreatmentsForStudy <- function(data, study_name) {
    rows = data$Study == study_name
    columns = grep("^T(\\.[12])?$", names(data))
    treatments <- unlist(data[rows, columns])
    names(treatments) <- c()
    
    return(treatments)
  }
})
