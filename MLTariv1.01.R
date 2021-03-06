#This code cleans up the data from the "Task and Resource Information" report from MES.

require(xlsx)
require(readxl)
require(purrr)

aggregateTariData <- function(directory){
  #this functions goes through the folder, searches for CSV files and concatenates by rows all the
  #data
  
  output <- data.frame() #initializes an empty data frame
  output_colnames <- c('Container', 'Parent Container', 'Container Status', 'Material', 
                       'Material Desc', 'Material Rev', 'Model', 'Serial #', 'SAP Batch', 
                       'Product Family', 'Original Qty', 'Current Qty', 'Final Confirmed Qty', 
                       'Production Order', 'Production Order Type', 'SWR', 'Work Cell', 'Task List', 
                       'Task List Desc', 'Task List Rev', 'Task Item', 'Data Point Name', 
                       'Data Point Desc', 'Data Point  Value', 'High Limit', 'Low Limit', 'Result', 
                       'Submitter', 'Data Collection Date & Time')
  
  setwd(directory) # sets the working directory to the directory. This way the files can be 
  #accessed by just their respective filenames and not a full workingpath
  
  file_list <- list.files(directory, pattern = "\\.csv") #lists all the files in the directory
  
  directory_count <- 1 #count for going through the files in the directory
  
  while (directory_count < length(file_list) + 1){
    file_name <- file_list[directory_count]
    print(paste0("Running File: ", file_name))
    
    if (grepl("-2", file_name, ignore.case = TRUE)){
      #if the sheet was first downloaded as an excel file it may have two csv files associated to it
      #because two excel tabs were created to fit the data
      #the second tab will not have a header
      data <- read.csv(file_name, header = FALSE, skip = 4, stringsAsFactors = FALSE)
    }
    else{
      #regular csv saved from an excel file
      data <- read.csv(file_name, header = FALSE, skip = 4, stringsAsFactors = FALSE)
    }
    
    output <- rbind(output, data)
    
    directory_count <- directory_count + 1
  }# end while of running through the folder/directory
  
  colnames(output) <- output_colnames
  
  return(output)
  
}# end aggregateTariData


generateTariList <- function(data){
  #'this function goes through the data frame generated by aggregateTariData() and creates a list
  #'the upper elements of the list is the part number. The attributes within the element are the
  #'associated data in tabular form. For example, batch number, drying time, drying temp
  #'barrel temps, screw speeds, etc
  
  
  
  output_list <- list() #initialize an empty list to store the material output data
  
  tari_material_list <- data[!duplicated(data[,"Material"]),"Material"] #list of unique materials
  #column name must be material
  tari_material_list_length <- length(tari_material_list)
  
  #'I will list out a few variables that define different names of task items. These names will be
  #'the generic names for any run, extruder 1 temps, extruder 2 temps, extruder 3 temps,
  #'extruder speeds, extruders pressure, puller speeds.
  #'I will then put these names together for the subelement names
  genericnames <- c("SAP Batch Number", "SWR Number", "Operator ID", "Line", "Start Time",
                    "Drying Temperature (F)", "Drying Time (h)")
  
  extruder1tempnames <- c("Extruder 1 Feedthroat Temp",
                          "Ext 1 Barrel 1 Temp", "Ext 1 Barrel 2 Temp", "Ext 1 Barrel 3 Temp",
                          "Ext 1 Clamp Temp", "Ext 1 Fliter Temp", "Ext 1 Adapter Temp",
                          "Ext 1 Melt Pump Temp", "Ext 1 Melt Pump Clamp Temp")
  extruder2tempnames <- c("Extruder 2 Feedthroat Temp",
                          "Ext 2 Barrel 1 Temp", "Ext 1 Barrel 2 Temp", "Ext 1 Barrel 3 Temp",
                          "Ext 2 Clamp Temp", "Ext 1 Fliter Temp", "Ext 1 Adapter Temp",
                          "Ext 2 Melt Pump Temp", "Ext 1 Melt Pump Clamp Temp")
  extruder3tempnames <- c("Extruder 3 Feedthroat Temp", 
                          "Ext 3 Barrel 1 Temp", "Ext 1 Barrel 2 Temp", "Ext 1 Barrel 3 Temp",
                          "Ext 3 Clamp Temp", "Ext 1 Fliter Temp", "Ext 1 Adapter Temp",
                          "Ext 3 Melt Pump Temp", "Ext 1 Melt Pump Clamp Temp")
  
  generictempnames <- c("Die 1 Temp", "Die 2 Temp")
  
  extruderspeeds <- c("Ext 1 Screw Speed", "Ext 2 Screw Speed", "Ext 3 Screw Speed")
  
  extruderpressure <- c("Ext 1 Barrel Pressure", "Ext 1 Adapter Pressure", "Ext 1 MP Outlet Pressure",
                        "Ext 2 Barrel Pressure", "Ext 2 Adapter Pressure", "Ext 2 MP Outlet Pressure",
                        "Ext 3 Barrel Pressure", "Ext 3 Adapter Pressure", "Ext 3 MP Outlet Pressure")
  
  lastnames <- c("Puller Speed")
  
  subelement_names <- c(genericnames, extruder1tempnames, extruder2tempnames, extruder3tempnames,
                        generictempnames, extruderspeeds, extruderpressure, lastnames)
  
  material_count <- 1 #goes through the materials
  while (material_count < tari_material_list_length + 1){
    #this loops through the data frame and gets all the relevant information for a material
    #it assigns each material as an element to a list and the parameters as sub elements
    
    current_material <- tari_material_list[material_count] #gets the current material
    
    sub_data <- data[(data[,"Material"] == current_material),] #gets all the rows for the material
    
    batch_list <- sub_data[!duplicated(sub_data[,"SAP Batch"]), "SAP Batch"] 
    #gets the unique batch numbers
    batch_list_length <- length(batch_list)
    batch_df <- data.frame(matrix(nrow = length(batch_list), ncol = length(subelement_names)),
                           stringsAsFactors = FALSE)
    #initializes a data frame to store the information for all the batches
    #this data frame will then be assigned to the material element in the material list.
    batch_count <- 1 #this loops through all the batches of a material
    
    while(batch_count < batch_list_length + 1){
      
      current_batch <- batch_list[batch_count] #gets the current batch
      sub_data_batch <- sub_data[(sub_data[,"SAP Batch"] == current_batch),] #get the data for the
      #current batch
      
      sap_batch <- getSAPBatch(sub_data_batch)
      swr_number <- getSWRNumber(sub_data_batch)
      operator_id <- getOperatorID(sub_data_batch)
      line <- getLine(sub_data_batch)
      start_time <- getStartTime(sub_data_batch)
      drying_temperature <- getDryingTemperature(sub_data_batch)
      drying_time <- getDryingTime(sub_data_batch)
      feedthroat_temp <- getFeedThroatTemperature(sub_data_batch)
      barrelzone1_temp <- getBarrelZone1Temperature(sub_data_batch)
      barrelzone2_temp <- getBarrelZone2Temperature(sub_data_batch)
      barrelzone3_temp <- getBarrelZone3Temperature(sub_data_batch)
      adapter_temp <- getAdapterTemperature(sub_data_batch)
      clamp_temp <- getClampTemperature(sub_data_batch)
      die1_temp <- getDie1Temperature(sub_data_batch)
      die2_temp <- getDie2Temperature(sub_data_batch)
      barrel_pressure <- getBarrelPressure(sub_data_batch)
      die_pressure <- getDiePressure(sub_data_batch)
      extruder_speed <- getExtruderSpeed(sub_data_batch)
      puller_speed <- getPullerSpeed(sub_data_batch)
      
      parameter_data_vector <- c(sap_batch, swr_number, operator_id, line, start_time,
                                 drying_temperature, drying_time, feedthroat_temp, barrelzone1_temp,
                                 barrelzone2_temp, barrelzone3_temp, adapter_temp, clamp_temp,
                                 die1_temp, die2_temp, barrel_pressure, die_pressure, extruder_speed,
                                 puller_speed) #assign parameters to a vector
      
      batch_df[batch_count,] <- parameter_data_vector
      #concatenate data to batch_df
      
      batch_count <- batch_count + 1 #update batch_count
    }#end while that goes through the batches
    
    colnames(batch_df) <- subelement_names #naming the parameters
    
    output_list[[current_material]] <- batch_df #double brackets to name and insert
    
    material_count <- material_count + 1 #update batch_count
  }#end while that loops through the materials
  
  return(output_list)
  
}# end generateTariList


generateTariDataFrame <- function(data){
  #this does that same as generateTariList except it makes a dataframe instead of a list.
  
  test <- list() #this list will store three data frames. One is the run parameters, the other is
  #is the time stamps, and the last is the merging of the two
  
  
  df_rowlength <- length(unique(data$'SAP Batch')) #the nrow of the df is the number of unique
  #SAP batches
  
  column_names <- c("Material Number", "SAP Batch Number", "SWR Number", "Operator ID", "Line", 
                    "Start Time", "Drying Temperature (F)", "Drying Time (h)", 
                    "Feedthroat Temperature (F)", "Barrel Zone 1 Temperature (F)", 
                    "Barrel Zone 2 Temperature (F)", "Barrel Zone 3 Temperature (F)", 
                    "Adapter Temperature (F)", "Clamp Temperature (F)", "Die 1 Temperature (F)", 
                    "Die 2 Temperature (F)",
                    "Barrel Pressure (psi)", "Die Pressure (psi)",
                    "Extruder Speed", "Puller Speed")
  
  time_column_names <- c("Material Number", "SAP Batch Number", "SWR Number", "Operator ID", "Line", 
                         "Start Time", "Drying Temperature (F) Time", "Drying Time (h) Time", 
                         "Feedthroat Temperature (F) Time", "Barrel Zone 1 Temperature (F) Time", 
                         "Barrel Zone 2 Temperature (F) Time", "Barrel Zone 3 Temperature (F) Time", 
                         "Adapter Temperature (F) Time", "Clamp Temperature (F) Time", 
                         "Die 1 Temperature (F) Time", 
                         "Die 2 Temperature (F) Time",
                         "Barrel Pressure (psi) Time", "Die Pressure (psi) Time",
                         "Extruder Speed Time", "Puller Speed Time")
  
  submit_column_names <- c("Material Number", "SAP Batch Number", "SWR Number", "Operator ID", "Line", 
                           "Start Time", "Drying Temperature (F) Submitter", "Drying Time (h) Submitter", 
                           "Feedthroat Temperature (F) Submitter", "Barrel Zone 1 Temperature (F) Submitter", 
                           "Barrel Zone 2 Temperature (F) Submitter", "Barrel Zone 3 Temperature (F) Submitter", 
                           "Adapter Temperature (F) Submitter", "Clamp Temperature (F) Submitter", 
                           "Die 1 Temperature (F) Submitter", 
                           "Die 2 Temperature (F) Submitter",
                           "Barrel Pressure (psi) Submitter", "Die Pressure (psi) Submitter",
                           "Extruder Speed Submitter", "Puller Speed Submitter")
  
  df_columnlength <- length(column_names)
  
  output_df <- data.frame(matrix(ncol = df_columnlength, nrow = df_rowlength), stringsAsFactors = F)
  time_df <- data.frame(matrix(ncol = df_columnlength, nrow = df_rowlength), stringsAsFactors = F)
  submit_df <- data.frame(matrix(ncol = df_columnlength, nrow = df_rowlength), stringsAsFactors = F)
  total_df <- data.frame(matrix(ncol = df_columnlength, nrow = (3*df_rowlength)), stringsAsFactors = F)
  #the total_df includes both data
  
  
  #the dataframe is initialized
  names(output_df) <- column_names #name the columns
  names(time_df) <- time_column_names
  names(submit_df) <- submit_column_names
  names(total_df) <- column_names
  
  tari_material_list <- unique(data[,"Material"]) #list of unique materials
  
  tari_material_list_length <- length(tari_material_list)
  material_count <- 1 #goes through the materials
  overall_batch_count <- 1 #keeps track of all the batches that have been analyzed
  
  while (material_count < tari_material_list_length + 1){
    #this loops through the data frame and gets all the relevant information for a material
    
    current_material <- tari_material_list[material_count] #gets the current material
    
    sub_data <- data[(data[,"Material"] == current_material),] #gets all the rows for the material
    
    batch_list <- unique(sub_data[,"SAP Batch"])
    #gets the unique batch numbers
    
    batch_list_length <- length(batch_list)
    #initializes a data frame to store the information for all the batches
    #this data frame will then be assigned to the material element in the material list.
    batch_count <- 1 #this loops through all the batches of a material
    
    while(batch_count < batch_list_length + 1){
      
      current_batch <- batch_list[batch_count] #gets the current batch
      sub_data_batch <- sub_data[(sub_data[,"SAP Batch"] == current_batch),] #get the data for the
      #current batch
      
      sap_batch <- getSAPBatch(sub_data_batch)
      swr_number <- getSWRNumber(sub_data_batch)
      operator_id <- getOperatorID(sub_data_batch)
      line <- getLine(sub_data_batch)
      start_time <- getStartTime(sub_data_batch)
      drying_temperature <- getDryingTemperature(sub_data_batch)$parameter
      drying_time <- getDryingTime(sub_data_batch)$parameter
      feedthroat_temp <- getFeedThroatTemperature(sub_data_batch)$parameter
      barrelzone1_temp <- getBarrelZone1Temperature(sub_data_batch)$parameter
      barrelzone2_temp <- getBarrelZone2Temperature(sub_data_batch)$parameter
      barrelzone3_temp <- getBarrelZone3Temperature(sub_data_batch)$parameter
      adapter_temp <- getAdapterTemperature(sub_data_batch)$parameter
      clamp_temp <- getClampTemperature(sub_data_batch)$parameter
      die1_temp <- getDie1Temperature(sub_data_batch)$parameter
      die2_temp <- getDie2Temperature(sub_data_batch)$parameter
      barrel_pressure <- getBarrelPressure(sub_data_batch)$parameter
      die_pressure <- getDiePressure(sub_data_batch)$parameter
      extruder_speed <- getExtruderSpeed(sub_data_batch)$parameter
      puller_speed <- getPullerSpeed(sub_data_batch)$parameter
      
      parameter_data_vector <- c(current_material, sap_batch, swr_number, operator_id, line, start_time,
                                 drying_temperature, drying_time, feedthroat_temp, barrelzone1_temp,
                                 barrelzone2_temp, barrelzone3_temp, adapter_temp, clamp_temp,
                                 die1_temp, die2_temp, barrel_pressure, die_pressure, extruder_speed,
                                 puller_speed) #assign parameters to a vector
      
      output_df[overall_batch_count, ] <- parameter_data_vector
      
      
      #Time Parameters#
      
      
      drying_temperature_time <- getDryingTemperature(sub_data_batch)$time
      drying_time_time <- getDryingTime(sub_data_batch)$time
      feedthroat_temp_time <- getFeedThroatTemperature(sub_data_batch)$time
      barrelzone1_temp_time <- getBarrelZone1Temperature(sub_data_batch)$time
      barrelzone2_temp_time <- getBarrelZone2Temperature(sub_data_batch)$time
      barrelzone3_temp_time <- getBarrelZone3Temperature(sub_data_batch)$time
      adapter_temp_time <- getAdapterTemperature(sub_data_batch)$time
      clamp_temp_time <- getClampTemperature(sub_data_batch)$time
      die1_temp_time <- getDie1Temperature(sub_data_batch)$time
      die2_temp_time <- getDie2Temperature(sub_data_batch)$time
      barrel_pressure_time <- getBarrelPressure(sub_data_batch)$time
      die_pressure_time <- getDiePressure(sub_data_batch)$time
      extruder_speed_time <- getExtruderSpeed(sub_data_batch)$time
      puller_speed_time <- getPullerSpeed(sub_data_batch)$time
      
      time_data_vector <- c(current_material, sap_batch, swr_number, operator_id, line, start_time,
                            drying_temperature_time, drying_time_time, feedthroat_temp_time, 
                            barrelzone1_temp_time,
                            barrelzone2_temp_time, barrelzone3_temp_time, adapter_temp_time, 
                            clamp_temp_time, die1_temp_time, die2_temp_time, barrel_pressure_time, 
                            die_pressure_time, extruder_speed_time, puller_speed_time)
      
      time_df[overall_batch_count, ] <- time_data_vector
      
      
      #Submitter#
      
      drying_temperature_submit <- getDryingTemperature(sub_data_batch)$submit
      drying_submit_submit <- getDryingTime(sub_data_batch)$submit
      feedthroat_temp_submit <- getFeedThroatTemperature(sub_data_batch)$submit
      barrelzone1_temp_submit <- getBarrelZone1Temperature(sub_data_batch)$submit
      barrelzone2_temp_submit <- getBarrelZone2Temperature(sub_data_batch)$submit
      barrelzone3_temp_submit <- getBarrelZone3Temperature(sub_data_batch)$submit
      adapter_temp_submit <- getAdapterTemperature(sub_data_batch)$submit
      clamp_temp_submit <- getClampTemperature(sub_data_batch)$submit
      die1_temp_submit <- getDie1Temperature(sub_data_batch)$submit
      die2_temp_submit <- getDie2Temperature(sub_data_batch)$submit
      barrel_pressure_submit <- getBarrelPressure(sub_data_batch)$submit
      die_pressure_submit <- getDiePressure(sub_data_batch)$submit
      extruder_speed_submit <- getExtruderSpeed(sub_data_batch)$submit
      puller_speed_submit <- getPullerSpeed(sub_data_batch)$submit
      
      submit_data_vector <- c(current_material, sap_batch, swr_number, operator_id, line, start_time,
                              drying_temperature_submit, drying_submit_submit, feedthroat_temp_submit, 
                              barrelzone1_temp_submit,
                              barrelzone2_temp_submit, barrelzone3_temp_submit, adapter_temp_submit, 
                              clamp_temp_submit, die1_temp_submit, die2_temp_submit, barrel_pressure_submit, 
                              die_pressure_submit, extruder_speed_submit, puller_speed_submit)
      
      submit_df[overall_batch_count, ] <- submit_data_vector
      
      
      
      #total data
      total_df[(overall_batch_count*3),] <- parameter_data_vector
      total_df[(overall_batch_count*3) + 1,] <- time_data_vector
      total_df[(overall_batch_count*3) + 2,] <- submit_data_vector
      
      batch_count <- batch_count + 1 #update batch_count
      overall_batch_count <- overall_batch_count + 1
    }#end while that goes through the batches
    
    material_count <- material_count + 1 #update batch_count
  }#end while that loops through the materials
  
  
  total_df <- total_df[3:nrow(total_df),] #remove first row that is blank due to *2 multiplier
  
  converttime_count <- 6
  while (converttime_count < 21){
    time_df[,converttime_count] <- as.POSIXct(time_df[,converttime_count], format = '%m/%d/%Y %H:%M:%S %p')
    converttime_count <- converttime_count + 1
  }
  
  test$parameters <- output_df
  test$time <- time_df
  test$submit <- submit_df
  test$total <- total_df
  
  
  return(test)
  
  
}# end generateTariDataFrame


getSAPBatch <- function(data){
  #parses the batch data to get the SWR number
  #because the batch number will be in every row, it just grabs the first one
  
  sap_batch <- data[1,"SAP Batch"]
  return(sap_batch)
  
} #end getSAPBatch


getSWRNumber <- function(data){
  #parses the batch data to get the SWR number
  #because the batch number will be in every row, it just grabs the first one
  
  swr_number <- data[1,"SWR"]
  return(swr_number)
  
} #end getSWRNumber


getOperatorID <- function(data){
  #parses the batch data to get the Operator ID
  #because the operator ID will be in every row, it just grabs the first one
  
  operator_id <- data[1,"Submitter"]
  return(operator_id)
  
}#end getOperatorID


getLine <- function(data){
  #parses the batch data to get the line
  #because the line will be in every row, it just grabs the first one; however, it assumes that the 
  #line will be in the first task IT may have offline or other work cells as the first task
  
  line <- data[1,"Work Cell"]
  return(line)
  
}#end getLine


getStartTime <- function(data){
  #parses the batch data to get the start time for the first task
  
  start_time <- data[1,"Data Collection Date & Time"]
  return(start_time)
  
}#end getStartTime


getDryingTemperature <- function(data){
  #parses the batch data to get the drying temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  
  row_index <- grep("Drying Temp Setpoint", data[, "Data Point Name"], ignore.case = TRUE)
  
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getDryingTemperature


getDryingTime <- function(data){
  #parses the batch data to get the drying time by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Drying Time", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, check for another name for the task
    
    row_index <- grep("Verify Resin is Dryed", data[, "Data Point Name"], ignore.case = TRUE)
    
    if (length(row_index) == 0){
      #if still no match was found, return NA
      return (list(parameter = NA, time = NA, submit = NA))
    }
    else if (length(row_index) > 1){
      #if multiple matches, return the first
      row_index <- row_index[1]
    }
    
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getDryingTime


getFeedThroatTemperature <- function(data){
  #parses the batch data to get the feedthroat temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Feedthroat Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
} #end getFeedThroatTemperature


getBarrelZone1Temperature <- function(data){
  #parses the batch data to get the barrel zone 1 temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Barrel 1 Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getBarrelZone1Temperature


getBarrelZone2Temperature <- function(data){
  #parses the batch data to get the barrel zone 2 temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Barrel 2 Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getBarrelZone2Temperature


getBarrelZone3Temperature <- function(data){
  #parses the batch data to get the barrel zone 3 temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Barrel 3 Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getBarrelZone3Temperature


getAdapterTemperature <- function(data){
  #parses the batch data to get the adapter temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Adapter Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
} #end getAdapterTemperature


getClampTemperature <- function(data){
  #parses the batch data to get the clamp temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Extruder Clamp Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getClampTemperature


getDie1Temperature <- function(data){
  #parses the batch data to get the die 1 temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Die 1 Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  
  return(output)
  
}#end getDie1Temperature


getDie2Temperature <- function(data){
  #parses the batch data to get the die 2 temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Die 2 Temp", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  return(output)
  
}#end getDie1Temperature


getBarrelPressure <- function(data){
  #parses the batch data to get the barrel pressure by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Barrel Pressure", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getBarrelPressure


getDiePressure <- function(data){
  #parses the batch data to get the die pressure by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Die Pressure", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  return(output)
  
}#end getDiePressure


getExtruderSpeed <- function(data){
  #parses the batch data to get the extruder speed by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Extruder Speed", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  return(output)
  
}#end getExtruderSpeed


getPullerSpeed <- function(data){
  #parses the batch data to get the puller speed by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Puller Speed", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  return(output)
  
}#end getPullerSpeed





