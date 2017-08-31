
#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

server<-function(input,output,session){
  #' Split up into five sections: 
  #' 1) The environments and the setter and getter methods
  #' 2) The observe functions for the input variables
  #' 3) The single extrusion data set cleaning
  #' 4) The multi layered extrusion data set cleaning
  #' 5) The tapered extrusion data set cleaning
  
  generateNewDF <- function(df, index_name, value){
    #this determines whether the index is max, min, or other. And then it cleans up the df based
    #on that and generates a new clean df
    
    if (length(grep("min", index_name, ignore.case = TRUE)) != 0){
      #if min was found in the index_name
      
      parameter_name <- gsub(" Min", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      new_df <- df[df[,column_index] >= value,]
      return(new_df)
      
    }
    else if(length(grep("max", index_name, ignore.case = TRUE)) != 0){
      #if max was found in the index_name
      
      parameter_name <- gsub(" Max", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      new_df <- df[df[,column_index] <= value,]
      return(new_df)
      
    }
    else{
      #this is not a min or a max, but instead is a select input
      
      parameter_name <- index_name
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      if (value == "All"){
        #if 'All' is selected
        new_df <- df
      }
      else{
        #a specific value was selected
        new_df <- df[df[,column_index] == value,]
      }
      
      return(new_df)
      
    }#end if-else on the types of parameters
    
  }#end generateNewDf
  
  
  #### Single PPS  ####
  
  e1 <- new.env(
    #This environment will store variable of inputs and stack that are used for comparison
    #of the input parameters that have been selected and changed
    #' variables to contain:
    #' sivector - a vector that will store the inputs of the single extrusion parameters
    #' sistack - the stack for the single inputs
  ) #creates a new environment to store instance variables
  
  #the assign will initialize the siidvector
  assign("siidvector", 
         c("PCSPN", "PCSPD", "PCSRN", "PCSRD", "PCSPPSN", 
           "PCSDS_min", "PCSDS_max", "PCSDLL", "PCSTS_min", "PCSTS_max",
           "PCSTLL", "PCSSP", 
           "PCSFT_min", "PCSFT_max", "PCSBZT1_min", "PCSBZT1_max",
           "PCSBZT2_min", "PCSBZT2_max", "PCSBZT3_min", "PCSBZT3_max",
           "PCSCT_min", "PCSCT_max", "PCSAT_min", "PCSAT_max",
           "PCSDT1_min", "PCSDT1_max", "PCSDT2_min", "PCSDT2_max",
           "PCSIDI_min", "PCSIDI_max", "PCSODI_min", "PCSODI_max",
           "PCSWT_min", "PCSWT_max", "PCSOR_min", "PCSOR_max", 
           "PCSCCT_min", "PCSCCT_max", "PCSLength_min", "PCSLength_max",
           "PCSPPD",
           "PCSNEXIV", "PCSAnnealed", "PCSCaliper", "PCSOS",
           "PCSMP", "PCSHT", "PCSSPD", "PCSSLD", "PCSDLN", "PCSULT",
           "PCSVC", "PCSIRD"), 
         envir = e1)
  
  
  ## These are the setter and getter function ##
  
  setSIIDVector <- function(vector){
    #set the vector for the input ids
    
    assign("siidvector", vector, env = e1)
    
  } #end setSIIDVector
  
  getSIIDVector <- function(){
    #returns the IDs of the inputs
    
    if(exists("siidvector", e1)){
      return(get("siidvector", e1))
    }
    else{
      return(NA)
    }
  } #end SIIDVector
  
  setSCBVector <- function(vector){
    #this sets the values for the single input vector
    assign("scbvector", vector, env = e1)
  } #end setSCVector
  
  getSCBVector <- function(vector){
    #this gets the values for the single checkbox vector
    
    if(exists("scbvector", e1)){
      return(get("scbvector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getSCBVector
  
  setOriginalSIVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originalscbvector", vector, env = e1)
  } #end setOriginalSIVector
  
  getOriginalSIVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originalscbvector", e1)){
      return(get("originalscbvector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalSIVector
  
  setSIVector <- function(vector){
    #this sets the values for the single input vector
    assign("sivector", vector, env = e1)
  } #end setSIVector
  
  getSIVector <- function(){
    #returns sivector
    
    if(exists("sivector", e1)){
      return(get("sivector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getSIVector
  
  setSIStack <- function(stack){
    #this sets the stack for the single inputs
    assign("sistack", stack, env = e1)
  } #end setSIStack
  
  getSIStack <- function(){
    #returns sivector
    if(exists("sistack", e1)){
      return(get("sistack", e1))
    }
    else{
      return(NA)
    }
    
  } #end getSIStack
  
  getSIStack.length <- function(){
    #returns the length of the SIStack
    
    if(exists("sistack", e1)){
      #if it exists, return the length
      return(length(getSIStack()))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getSIStack.length
  
  addSIStack <- function(name){
    #this function adds a new parameter to the stack
    #it will only add to the stack if it is a unique name
    
    stack <- get("sistack", e1)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    
    if (length(grep(rep_name, stack)) == 0){
      #the name is not currently in the stack
      stack <- c(stack, name) #add the name to the top of the stack (the right most)
      setSIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is already present
    }
    
  } #end  addSIStack
  
  removeSIStack <- function(name){
    #this function removes the inputs associated with the checkbox name from the stack
    
    print("removeSIStack: We are in the removeSIStack")
    
    stack <- get("sistack", e1)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    
    if (length(grep(rep_name, stack)) != 0){
      #the name is not currently in the stack
      indices <- grep(rep_name, stack)
      
      if (length(indices) == length(stack)){
        #if the length of the indices are equal to the stack
        print("removeSIStack: The length of the indices are equal to the stack")
        stack <- c()
      }
      else if (length(indices) == 1){
        print("removeSIStack: There was only one match in the stack")
        #if there is only one match
        if (indices == 1){
          #if the first matches is 1 or the only match is one
          stack <- stack[c(2:length(stack))] # removes the first element
        }
        else if (indices == length(stack)){
          stack <- stack[c(1:(indices - 1))]
        }
        else{
          #removes the index from the stack
          stack <- stack[c(1:(indices-1), (indices + 1):length(stack))]
        }#end if-else for the vallue of indices
      }
      else if (length(indices) == 2){
        print("removeSIStack: There were multiple matches to the stack")
        #if there are multiple indices
        if (indices[1] == 1){
          #if the first matches is 1 or the only match is one
          
          if (indices[2] == 2){
            stack <- stack[c(3:length(stack))] # removes the first and second element
          }
          else if (indices[2] == length(stack)){
            stack <- stack[c(2:(indices[2] - 1))]
          }
          else{
            stack <- stack[c(2:(indices[2] - 1), (indices[2] + 1):length(stack))] # the indices
          }#end if-else for the value of the second index
          
        }
        else if(indices[1] == (length(stack) - 1)){
          #if the first index is length(stack)-1, then the second index is the length of the stack
          stack <- stack[c(1:(indices[1] - 1))]
        }
        else if (indices[1] == (indices[2] + 1)){
          #if the indices are right next to each other but are not at the ends of the stack
          stack <- stack[c(1:(indices[1]-1), (indices[2] + 1):length(stack))]
        }
        else{
          #removes the indices from the stack
          stack <- stack[c(1:(indices[1]-1), (indices[1] + 1):(indices[2] - 1), (indices[2] + 1):length(stack))]
        }#end if-else for the value of indices
        
      }#end if-else for the length of indices
      else{
        #if it was neither, there was an error and the app shoudl stop
        print("The indices in the removeSIStack were more than 2")
        print(paste0("The indices are: ", indices))
        stop()
      }
      
      
      setSIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is not in the stack
      print("removeSIStack: Nothing was done because the name was not in the stack")
    }
    
  } #end  removeSIStack
  
  setSDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e1)
  }#end setSDFList
  
  getSDFList <- function(){
    #returns the df_list
    if(exists("df_list", e1)){
      #if it exists, return the length
      return((get("df_list", e1)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getSDFList
  
  addSDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e1)){
      #if the list already exists
      
      if(index == getSIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getSDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setSDFList(df_list) #set the new list
        
        single_df_output$data <- new_df #set the data table to this
        
      }
      else if(index > getSIStack.length() + 1){
        #if there was an error in the index and it was too large
        print("The index for the stack was too large and greater than the allowable limit")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getSIStack.length(), "."))
        stopApp()
      }
      else if(index < 1){
        #error in the index
        print("The index for the stack was zero or negative")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getSIStack.length(), "."))
        stopApp()
      }
      else{
        #the index is within the stack which means we means a previous parameter is being changed
        stack <- getSIStack()
        stack_length <- getSIStack.length()
        
        input_values <- getSIVector()
        
        df_list <- getSDFList() #gets latest df_list
        
        if (index == 1){
          #if it is starting from the beginning
          
          new_df <- generateNewDF(single_pps_data, index_name, value) #starts from this initial one
          df_list[[index]] <- new_df #add the new data frame
          
          count <- index + 1
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]]
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            count <- count + 1
            df_list[[count]] <- new_df #add the new data frame
          }#end while creating new DFs
          
          setSDFList(df_list) #set the new list
          single_df_output$data <- new_df #set the data table to this
          
        }
        else{
          
          count <- index
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]] #gets the previous df
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            
            df_list[[count]] <- new_df #add the new data frame
            
            count <- count + 1
          }#end while creating new DFs
          
          setSDFList(df_list) #set the new list
          single_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e1) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewDF(single_pps_data, index_name, value) #starts from this initial one
      df_list <- getSDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setSDFList(df_list) #set the new list
      single_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addSDFSeries
  
  removeSDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeSDFSeries: We are in removeSDFSeries")
    
    if (exists("df_list", e1)){
      #if the list already exists
      
      print("removeSDFSeries: df_list exists")
      
      stack <- getSIStack() #gets the stack
      stack_length <- getSIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setSDFList(NULL) #sets the list to NULL so it can be reinitialized
        single_df_output$data <- single_pps_data
        
        print("removeSDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeSDFSeries: The stack_length was NOT zero")
        
        input_values <- getSIVector() #gets the input_values
        df_list <- getSDFList() #gets latest df_list
        
        current_parameter <- stack[1] #gets the first parameter in the stack
        current_parameter_value <- input_values[current_parameter] #gets the parameters value
        
        new_df <- generateNewDF(single_pps_data, current_parameter, current_parameter_value) #starts from this initial one
        df_list[[1]] <- new_df #add the new data frame
        
        count <- 2
        
        while (count < stack_length + 1){
          
          previous_df <- df_list[[count-1]]
          current_parameter <- stack[count]
          current_parameter_value <- input_values[current_parameter]
          
          new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
          count <- count + 1
          df_list[[count]] <- new_df #add the new data frame
        }#end while creating new DFs
        
        if (count < (length(df_list) + 1)){
          #'because we recreate the df_list from the original df_list and th new df_list will not
          #'have the dataframes from the removed parameters, this new df_list will be shorter than
          #'the original before the parameters were removed. Thus we resize the df_list
          
          while ((length(df_list) + 1) > count){
            #the df_list will continually be resized as the inputs are made null, thus the condition
            #is met when all the element greater than an index of count are removed
            
            df_list[[count]] <- NULL
            
          } #end while for making the elements NULL
          
          
        }#end if for the count compared to the df_list length
        
        setSDFList(df_list) #set the new list
        single_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
        
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
      
    
  } #end removeSDFSeries
  
  resetSI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetSI: Resetting the Input Values")
    print(paste0("resetSI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    
    inputs <- names(isolate(single_inputs()))
    input_ids <- getSIIDVector()
    original_inputs <- getOriginalSIVector()
    print(original_inputs)
    
    input_indices <- grep(grepname, inputs)
    
    print(paste0("resetSI: input_indices is: ", input_indices))
    
    count <- 1
    
    while (count < length(input_indices) + 1){
      #goes through all the input_indices that matches
      
      print("resetSI: In the while loop.")
      
      index <- input_indices[count]
      input_name <- inputs[index]
      id <- input_ids[index]
      value <- original_inputs[[index]]
      
      print(paste0("resetSI: index is: ", index))
      print(paste0("resetSI: input_name is: ", input_name))
      print(paste0("resetSI: id is: ", id))
      print(paste0("resetSI: value is: ", value))
      
      if (length(grep(" min", input_name, ignore.case = TRUE)) > 0){
        print("resetSI: In the min")
        updateNumericInput(session,
                          inputId = id,
                          label = NULL,
                          value = value
                          )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetSI: In the max")
        updateNumericInput(session,
                          inputId = id,
                          label = NULL,
                          value = value
                          )
      }
      else{
        print("resetSI: In the else")
        updateSelectInput(session, 
                        id,
                        label = NULL,
                        choices = c("All",unique(as.character(single_pps_data[,checkbox_name]))),
                        selected =  "All"
                        )
      }#end if-else for grep the input_name
      
      count <- count + 1
    }#end while for length of input_indices
    
    
  }#end resetSI
  
  single_df_output <- reactiveValues(data = single_pps_data) #end reactive for single_df_output
  
  clean_single_pps_data <- reactiveValues(data = NA) #the clean data to be downloaded
  
  observeEvent(single_inputs(),{
    #'This will observe if any of the inputs of the parameters for single extrusion have changed
    #'It does not check to see if the input has been selected, but rather, if the user has changed
    #'the search input.'
    
    print("Input Observed")
    
    if (exists("sivector", e1)){
      #checks to see if the vector has been created yet. This is to prevent the initialization
      #of the program
      
      old_sivector <- getSIVector() #get the old sivector before inputs were changed
      current_sivector <- single_inputs()
      setSIVector(current_sivector) #updates the sivector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_sivector == old_sivector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed
        print("A parameter was changed but the value was changed to what the previous value was")
      }
      else{
        
        index_name <- names(current_sivector[index_differ])
        value <- current_sivector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("The value is null or na")
        }
        else{
          addSIStack(index_name) #adds the name to the stack
          current_sistack <- getSIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          
          stack_index <- grep(updated_index_name, current_sistack) #gets the index in the stack
          
          if (length(stack_index) != 1){
            print("The Stack Index has a length != 0")
            print(paste0("The Stack Index is: ", stack_index))
            stopApp() #terminate the program
          }
          else{
            addSDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addSDFSeries()
      
      setSIVector(single_inputs())
      setSIStack(c()) #creates an empty stack since no parameters have been changed yet
    }
  })#end observeEvent for the user inputs

  observeEvent(show_vars1(),{
    #' this function will observe when a user checks or unchecks an input. If the input is unchecked,
    #' it removes any of the data cleaning it did in the stack and also resets the value of the
    #' input to the initialized value from the start of the session

    print("observeEvent(show_vars1): Checkbox Observed")

    if (exists("scbvector", e1)){
      
      print("observeEvent(show_vars1): The scbvector Exists")

      old_scbvector <- getSCBVector() #get the old scbvector before inputs were changed
      current_scbvector <- show_vars1()
      setSCBVector(current_scbvector) #updates the scbvector with the new inputs

      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_scbvector == old_scbvector))

      if (length(index_differ) == 0){
        #Nothing will be analyzed since the value was changed to TRUE
      }
      else{
        
        print("observeEvent(show_vars1): The Checkbox Index_Differ Worked")

        index_name <- names(current_scbvector[index_differ])
        value <- current_scbvector[index_differ] #gets the value of the new parameter

        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("observeEvent(show_vars1): The checkbox value is null or na")
          stopApp()
        }
        else if (value == FALSE){
          #this ensures that the checkbox was unchecked instead of checked
          
          print("observeEvent(show_vars1): The Value of the Checkbox was FALSE")
          
          current_sistack <- getSIStack() #gets the newstack

          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)

          stack_index <- grep(updated_index_name, current_sistack) #gets the index in the stack

          if (length(stack_index) == 0){
            print("observeEvent(show_vars1): The Input was not in the Stack")
            #the input that was unchecked is not in the stack, so do nothing
          }
          else if (length(stack_index) == 1 || length(stack_index) == 2){
            #the parameter that was unchecked is in the stack
            #it can be of length 1 or 2 depending of the input parameter had a min and max value
            #if it is a selectize input, there will only be one match (length of 1)
            
            print("observeEvent(show_vars1): The Input was in the Stack")
            print(paste0("observeEvent(show_vars1): The Checkboxname is: ", index_name))
            
            resetSI(index_name) #resets the values of the inputs of the checkbox
            removeSIStack(index_name)
            removeSDFSeries() #removes the inputs associated with the checkbox
          }
          else{
            #none of the conditions were met, most likely multiple matches greater than 2
            print("observeEvent(show_vars1): The Stack Index for the Checkboxes ObserveEvent has a length > 2 or negative")
            stopApp()
          } #end if-else for the length of the stack index

        }
        else{
          #if it is not false, do nothing
          print("observeEvent(show_vars1): The Checkbox value was TRUE")
        }#end if-else for the value being na or null

      } #end if-else for the length of index_differ

    }
    else{
      #if it has not been created, it sets the vector
      #this is mainly to initialize data.

      setOriginalSIVector(single_inputs()) #initialize the original values
      setSCBVector(show_vars1()) #initialize the input values that will be changing with the app

    } #end if-else for scbvector existing


  })#end observeEvent for the checkboxes



  # obtain the output of checkbox from functions and make a list to store them----Single Extrusion PPS Data
  show_vars1<-reactive({
    checkboxes <- as.numeric(c(input$PCSPN_d,input$PCSPD_d,input$PCSRN_d,input$PCSRD_d,input$PCSPPSN_d,input$PCSDS_d,input$PCSDLL_d,input$PCSTS_d,input$PCSTLL_d,input$PCSSP_d,input$PCSFT_d,
                 input$PCSBZT1_d,input$PCSBZT2_d,input$PCSBZT3_d,input$PCSCT_d,input$PCSAT_d,input$PCSDT1_d,input$PCSDT2_d,input$PCSIDI_d,input$PCSODI_d,input$PCSWT_d,
                 input$PCSOR_d,input$PCSCCT_d,input$PCSLength_d,input$PCSPPD_d,input$PCSNEXIV_d,input$PCSAnnealed_d,input$PCSCaliper_d,input$PCSOS_d,input$PCSMP_d,input$PCSHT_d,
                 input$PCSSPD_d,input$PCSSLD_d,input$PCSDLN_d,input$PCSULT_d,input$PCSVC_d,input$PCSIRD_d))

    names(checkboxes) <- c("Part Number", "Part Description", "Resin Number", "Resin Description",
                           "PPS Number",
                           "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)", "Tip Land Length (in)", "Screw Print",
                           "Feedthroat Temperature  F",
                           "Barrel Zone 1 Temperature  F", "Barrel Zone 2 Temperature  F",
                           "Barrel Zone 3 Temperature  F","Clamp Temperature  F",
                           "Adapter Temperature  F","Die 1 Temperature  F", "Die 2 Temperature  F",
                           "Inner Diameter (in)", "Outer Diameter (in)",
                           "Wall Thickness (in)", "Out of Roundness (in)",
                           "Concentricity (in)", "Length (in)", "Perpendicularity (in)",
                           "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                           "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                           "Vacuum Calibration", "Irradiated")

    return(checkboxes)

  })
  
  #this variable will store all the inputs of the single extrusions
  single_inputs <- reactive({
    #this variable will store all the inputs of of the single extrusions
    inputs <- c(input$PCSPN, input$PCSPD, input$PCSRN, input$PCSRD, input$PCSPPSN, 
                   input$PCSDS_min, input$PCSDS_max, input$PCSDLL, input$PCSTS_min, input$PCSTS_max,
                   input$PCSTLL, input$PCSSP, 
                   input$PCSFT_min, input$PCSFT_max, input$PCSBZT1_min, input$PCSBZT1_max,
                   input$PCSBZT2_min, input$PCSBZT2_max, input$PCSBZT3_min, input$PCSBZT3_max,
                   input$PCSCT_min, input$PCSCT_max, input$PCSAT_min, input$PCSAT_max,
                   input$PCSDT1_min, input$PCSDT1_max, input$PCSDT2_min, input$PCSDT2_max,
                   input$PCSIDI_min, input$PCSIDI_max, input$PCSODI_min, input$PCSODI_max,
                   input$PCSWT_min, input$PCSWT_max, input$PCSOR_min, input$PCSOR_max, 
                   input$PCSCCT_min, input$PCSCCT_max, input$PCSLength_min, input$PCSLength_max,
                   input$PCSPPD,
                   input$PCSNEXIV, input$PCSAnnealed, input$PCSCaliper, input$PCSOS,
                   input$PCSMP, input$PCSHT, input$PCSSPD, input$PCSSLD, input$PCSDLN, input$PCSULT,
                   input$PCSVC, input$PCSIRD
                   )
    names(inputs) <- c("Part Number", "Part Description", "Resin Number", "Resin Description",
                       "PPS Number",
                       "Die Size (in) Min", "Die Size (in) Max", "Die Land (in) Length", 
                       "Tip Size (in) Min","Tip Size (in) Max", "Tip Land (in) Length", "Screw Print",
                       "Feedthroat Temperature  F Min", "Feedthroat Temperature  F Max",
                       "Barrel Zone 1 Temperature  F Min", "Barrel Zone 1 Temperature  F Max",
                       "Barrel Zone 2 Temperature  F Min", "Barrel Zone 2 Temperature  F Max",
                       "Barrel Zone 3 Temperature  F Min", "Barrel Zone 3 Temperature  F Max",
                       "Clamp Temperature  F Min", "Clamp Temperature  F Max",
                       "Adapter Temperature  F Min", "Adapter Temperature  F Max",
                       "Die 1 Temperature  F Min", "Die 1 Temperature  F Max",
                       "Die 2 Temperature  F Min", "Die 2 Temperature  F Max",
                       "Inner Diameter (in) Min", "Inner Diameter (in) Max", "Outer Diameter (in) Min",
                       "Outer Diameter (in) Max", "Wall Thickness (in) Min", "Wall Thickness (in) Max",
                       "Out of Roudness (in) Min", "Out of Roundness (in) Max", 
                       "Concentricity (in) Min", "Concentricity (in) Max",
                       "Length (in) Min", "Length (in) Max", "Perpendicularity (in", 
                       "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                       "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                       "Vacuum Calibration", "Irradiated")
    return(inputs)
  })
  
  # use all the input values from UI to modify table 1 and show the modified table
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      
      Col_PCS=c() #initialized the variable
      
      col_var1=show_vars1()
      col_var1 = c(1,col_var1) #because the first column is the buttons, this makes sure it is true
      for (i in 1:length(col_var1)){
        #this will go through col_var1 and determine which parameters have been checked
        #only the parameters that have been checked will be displayed on the table
        if (col_var1[i]!=0){
          Col_PCS=c(Col_PCS,i)
        }
      }
      
      data_PCS <- isolate(single_df_output$data) #the data frame is set
      data_PCS <- data_PCS[,Col_PCS] #only get the columns that have been checked
      
      clean_single_pps_data$data <- data_PCS #assign the clean table to the data that is available
      #for downloading
      
      return(data_PCS)
    }
    )
  },
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',
                                        targets = "_all"
                 )
                 ),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE),
  filter = "top",
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE) #end Single Extrusion PPS Data
  
  

  
  
  ###
  
  ### The single shopping cart section ###
  
  ###
  
  
  singleshoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the singl extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end singleshoppingcart
  
  singleshoppingcartparts <- reactiveValues(
    #'this will hold only a list of parts, this way it is easier for users to look at all the parts
    #'when there are two many batches in the shopping cart.
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  )
  
  observeEvent(input$singleadd_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$singleadd_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"singledelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    singleshoppingcart$data <- rbind(singleshoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(singleshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  })
  
  observeEvent(input$singleadd_button,{
    #this observes whether the user clicked a button to add a part to the part only shopping cart
    part <- strsplit(input$singleadd_button, "_")[[1]][2]
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    new_data <- cbind(part, deletepart)
    
    colnames(new_data) <- c("Part", "Delete Part")
    singleshoppingcartparts$data <- rbind(singleshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
    colnames(singleshoppingcartparts$data) <- c("Part", "Delete Part")
  })
  
  
  observeEvent(input$singledelete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$singledelete_part_button, "_")[[1]][2]
    singleshoppingcart$data <- singleshoppingcart$data[singleshoppingcart$data$'Part' != part,]
    singleshoppingcartparts$data <- singleshoppingcartparts$data[singleshoppingcartparts$data$'Part' != part,]
  })
  
  observeEvent(input$singledelete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$singledelete_batch_button, "_")[[1]][2]
    singleshoppingcart$data <- singleshoppingcart$data[singleshoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$singleshoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(singleshoppingcart$data)
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE,
    options=list(pageLength=5)   # make the shopping cart page shorter
  ) #for the shoppingcart
  
  
  output$singleshoppingcartparts <- renderDataTable({
    #'this is a table that only lists the parts for quick viewing
    return(singleshoppingcartparts$data)
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options=list(pageLength=5)   # make the shopping cart page shorter
  ) #for the shoppingcart
  
  
  output$singledownloadSPPSData <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Single PPS Data", '.csv', sep='') },
    content = function(file) {
      write.csv(clean_single_pps_data$data, file)
    }
  )
  
  output$singleshoppingcartpps <- renderDataTable({
    #this is to render a datatable that has all the PPS information of parts that have been saved
    #to the shopping cart
    
    data <- single_pps_data[which(single_pps_data$`Part Number` %in% singleshoppingcart$data$'Part'),]
    return(data)
    
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE
  )
  
  
  
  output$singlecartdownloadpps <- downloadHandler(
    #downlaod the single PPS data from the shopping cart
    filename = function() { paste("Single PPS Shopping Cart Data", '.csv', sep='') },
    content = function(file) {
      write.csv(single_pps_data[which(single_pps_data$`Part Number` %in% singleshoppingcart$data$'Part'),], file)
    }
  )
     
  
  

  #### Multi-Layer PPS ####
  
  generateNewMDF <- function(df, index_name, value){
    #this determines whether the index is max, min, or other. And then it cleans up the df based
    #on that and generates a new clean df
    
    if (length(grep("min", index_name, ignore.case = TRUE)) != 0){
      #if min was found in the index_name
      
      parameter_name <- gsub(" Min", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      middle_df <- df[df[,column_index] >= value,]
      
      part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
      new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      
      return(new_df)
      
    }
    else if(length(grep("max", index_name, ignore.case = TRUE)) != 0){
      #if max was found in the index_name
      
      parameter_name <- gsub(" Max", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      middle_df <- df[df[,column_index] <= value,]
      
      part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
      new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      
      return(new_df)
      
    }
    else{
      #this is not a min or a max, but instead is a select input
      
      parameter_name <- index_name
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      if (value == "All"){
        #if 'All' is selected
        new_df <- df
      }
      else{
        #a specific value was selected
        middle_df <- df[df[,column_index] == value,] #first it searches by the parameters
        part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
        new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      }
      
      return(new_df)
      
    }#end if-else on the types of parameters
    
  }#end generateNewMDf
  
  e2 <- new.env(
    #This environment will store variable of inputs and stack that are used for comparison
    #of the input parameters that have been selected and changed
    #' variables to contain:
    #' mivector - a vector that will store the inputs of the multi extrusion parameters
    #' mistack - the stack for the multi inputs
  ) #creates a new environment to store instance variables
  
  #the assign will initialize the miidvector
  assign("miidvector", 
         c("PCMPN", "PCMPD", "PCMRN", "PCMRD", "PCMPPSN", 
           "PCMDS_min", "PCMDS_max", "PCMDLL", "PCMTS_min", "PCMTS_max",
           "PCMTLL", "PCMSP", 
           "PCMFT_min", "PCMFT_max", "PCMBZT1_min", "PCMBZT1_max",
           "PCMBZT2_min", "PCMBZT2_max", "PCMBZT3_min", "PCMBZT3_max",
           "PCMCT_min", "PCMCT_max", "PCMAT_min", "PCMAT_max",
           "PCMDT1_min", "PCMDT1_max", "PCMDT2_min", "PCMDT2_max",
           "PCMIDI_min", "PCMIDI_max", "PCMODI_min", "PCMODI_max",
           "PCMIWT_min", "PCMIWT_max", "PCMMWT_min", "PCMMWT_max", 
           "PCMOWT_min", "PCMOWT_max", "PCMTWT_min", "PCMTWT_max", 
           "PCMOR_min", "PCMOR_max", 
           "PCMCCT_min", "PCMCCT_max", "PCMLength_min", "PCMLength_max",
           "PCMToLength_min", "PCMToLength_max",
           "PCMPPD",
           "PCMNEXIV", "PCMAnnealed", "PCMCaliper", "PCMOS",
           "PCMMP", "PCMHT", "PCMSPD", "PCMSLD", "PCMDLN", "PCMULT",
           "PCMVC", "PCMIRD"), 
         envir = e2)
  
  
  ## These are the setter and getter function ##
  
  setMIIDVector <- function(vector){
    #set the vector for the input ids
    
    assign("miidvector", vector, env = e2)
    
  } #end setMIIDVector
  
  getMIIDVector <- function(){
    #returns the IDs of the inputs
    
    if(exists("miidvector", e2)){
      return(get("miidvector", e2))
    }
    else{
      return(NA)
    }
  } #end getMIIDVector
  
  setMCBVector <- function(vector){
    #this sets the values for the multi input vector
    assign("mcbvector", vector, env = e2)
  } #end setMCVector
  
  getMCBVector <- function(vector){
    #this gets the values for the multi checkbox vector
    
    if(exists("mcbvector", e2)){
      return(get("mcbvector", e2))
    }
    else{
      return(NA)
    }
    
  } #end getMCBVector
  
  setOriginalMCBVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originalmcbvector", vector, env = e2)
  } #end setOriginalMCBVector
  
  getOriginalMCBVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originalmcbvector", e2)){
      return(get("originalmcbvector", e2))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalMCBVector
  
  setMIVector <- function(vector){
    #this sets the values for the multi input vector
    assign("mivector", vector, env = e2)
  } #end setMIVector
  
  getMIVector <- function(){
    #returns mivector
    
    if(exists("mivector", e2)){
      return(get("mivector", e2))
    }
    else{
      return(NA)
    }
    
  } #end getMIVector
  
  setMIStack <- function(stack){
    #this sets the stack for the multi inputs
    assign("mistack", stack, env = e2)
  } #end setMIStack
  
  getMIStack <- function(){
    #returns mivector
    if(exists("mistack", e2)){
      return(get("mistack", e2))
    }
    else{
      return(NA)
    }
    
  } #end getMIStack
  
  getMIStack.length <- function(){
    #returns the length of the SIStack
    
    if(exists("mistack", e2)){
      #if it exists, return the length
      return(length(getMIStack()))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getMIStack.length
  
  addMIStack <- function(name){
    #this function adds a new parameter to the stack
    #it will only add to the stack if it is a unique name
    
    stack <- get("mistack", e2)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    
    if (length(grep(rep_name, stack)) == 0){
      #the name is not currently in the stack
      stack <- c(stack, name) #add the name to the top of the stack (the right most)
      setMIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is already present
    }
    
  } #end  addMIStack
  
  removeMIStack <- function(name){
    #this function removes the inputs associated with the checkbox name from the stack
    
    print("removeMIStack: We are in the removeMIStack")
    
    stack <- get("mistack", e2)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    
    if (length(grep(rep_name, stack)) != 0){
      #the name is not currently in the stack
      indices <- grep(rep_name, stack)
      
      if (length(indices) == length(stack)){
        #if the length of the indices are equal to the stack
        print("removeMIStack: The length of the indices are equal to the stack")
        stack <- c()
      }
      else if (length(indices) == 1){
        print("removeMIStack: There was only one match in the stack")
        #if there is only one match
        if (indices == 1){
          #if the first matches is 1 or the only match is one
          stack <- stack[c(2:length(stack))] # removes the first element
        }
        else if (indices == length(stack)){
          stack <- stack[c(1:(indices - 1))]
        }
        else{
          #removes the index from the stack
          stack <- stack[c(1:(indices-1), (indices + 1):length(stack))]
        }#end if-else for the vallue of indices
      }
      else if (length(indices) == 2){
        print("removeMIStack: There were multiple matches to the stack")
        #if there are multiple indices
        if (indices[1] == 1){
          #if the first matches is 1 or the only match is one
          
          if (indices[2] == 2){
            stack <- stack[c(3:length(stack))] # removes the first and second element
          }
          else if (indices[2] == length(stack)){
            stack <- stack[c(2:(indices[2] - 1))]
          }
          else{
            stack <- stack[c(2:(indices[2] - 1), (indices[2] + 1):length(stack))] # the indices
          }#end if-else for the value of the second index
          
        }
        else if(indices[1] == (length(stack) - 1)){
          #if the first index is length(stack)-1, then the second index is the length of the stack
          stack <- stack[c(1:(indices[1] - 1))]
        }
        else if (indices[1] == (indices[2] + 1)){
          #if the indices are right next to each other but are not at the ends of the stack
          stack <- stack[c(1:(indices[1]-1), (indices[2] + 1):length(stack))]
        }
        else{
          #removes the indices from the stack
          stack <- stack[c(1:(indices[1]-1), (indices[1] + 1):(indices[2] - 1), (indices[2] + 1):length(stack))]
        }#end if-else for the value of indices
        
      }#end if-else for the length of indices
      else{
        #if it was neither, there was an error and the app shoudl stop
        print("The indices in the removeMIStack were more than 2")
        print(paste0("The indices are: ", indices))
        stop()
      }
      
      
      setMIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is not in the stack
      print("removeMIStack: Nothing was done because the name was not in the stack")
    }
    
  } #end  removeMIStack
  
  setMDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e2)
  }#end setMDFList
  
  getMDFList <- function(){
    #returns the df_list
    if(exists("df_list", e2)){
      #if it exists, return the length
      return((get("df_list", e2)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getMDFList
  
  addMDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e2)){
      #if the list already exists
      
      if(index == getMIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getMDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewMDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setMDFList(df_list) #set the new list
        
        multi_df_output$data <- new_df #set the data table to this
        
      }
      else if(index > getMIStack.length() + 1){
        #if there was an error in the index and it was too large
        print("The index for the stack was too large and greater than the allowable limit")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getMIStack.length(), "."))
        stopApp()
      }
      else if(index < 1){
        #error in the index
        print("The index for the stack was zero or negative")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getMIStack.length(), "."))
        stopApp()
      }
      else{
        #the index is within the stack which means we means a previous parameter is being changed
        stack <- getMIStack()
        stack_length <- getMIStack.length()
        
        input_values <- getMIVector()
        
        df_list <- getMDFList() #gets latest df_list
        
        if (index == 1){
          #if it is starting from the beginning
          
          new_df <- generateNewMDF(multi_pps_data, index_name, value) #starts from this initial one
          df_list[[index]] <- new_df #add the new data frame
          
          count <- index + 1
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]]
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewMDF(previous_df, current_parameter, current_parameter_value)
            count <- count + 1
            df_list[[count]] <- new_df #add the new data frame
          }#end while creating new DFs
          
          setMDFList(df_list) #set the new list
          multi_df_output$data <- new_df #set the data table to this
          
        }
        else{
          
          count <- index
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]] #gets the previous df
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewMDF(previous_df, current_parameter, current_parameter_value)
            
            df_list[[count]] <- new_df #add the new data frame
            
            count <- count + 1
          }#end while creating new DFs
          
          setMDFList(df_list) #set the new list
          multi_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e2) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewMDF(multi_pps_data, index_name, value) #starts from this initial one
      df_list <- getMDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setMDFList(df_list) #set the new list
      multi_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addMDFSeries
  
  removeMDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeMDFSeries: We are in removeMDFSeries")
    
    if (exists("df_list", e2)){
      #if the list already exists
      
      print("removeMDFSeries: df_list exists")
      
      stack <- getMIStack() #gets the stack
      stack_length <- getMIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setMDFList(NULL) #sets the list to NULL so it can be reinitialized
        multi_df_output$data <- multi_pps_data
        
        print("removeMDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeMDFSeries: The stack_length was NOT zero")
        
        input_values <- getMIVector() #gets the input_values
        df_list <- getMDFList() #gets latest df_list
        
        current_parameter <- stack[1] #gets the first parameter in the stack
        current_parameter_value <- input_values[current_parameter] #gets the parameters value
        
        new_df <- generateNewMDF(multi_pps_data, current_parameter, current_parameter_value) #starts from this initial one
        df_list[[1]] <- new_df #add the new data frame
        
        count <- 2
        
        while (count < stack_length + 1){
          
          previous_df <- df_list[[count-1]]
          current_parameter <- stack[count]
          current_parameter_value <- input_values[current_parameter]
          
          new_df <- generateNewMDF(previous_df, current_parameter, current_parameter_value)
          count <- count + 1
          df_list[[count]] <- new_df #add the new data frame
        }#end while creating new DFs
        
        if (count < (length(df_list) + 1)){
          #'because we recreate the df_list from the original df_list and th new df_list will not
          #'have the dataframes from the removed parameters, this new df_list will be shorter than
          #'the original before the parameters were removed. Thus we resize the df_list
          
          while ((length(df_list) + 1) > count){
            #the df_list will continually be resized as the inputs are made null, thus the condition
            #is met when all the element greater than an index of count are removed
            
            df_list[[count]] <- NULL
            
          } #end while for making the elements NULL
          
          
        }#end if for the count compared to the df_list length
        
        
        setMDFList(df_list) #set the new list
        multi_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
      
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
    
    
  } #end removeMDFSeries
  
  resetMI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetMI: Resetting the Input Values")
    print(paste0("resetMI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    
    inputs <- names(isolate(multi_inputs()))
    input_ids <- getMIIDVector()
    original_inputs <- getOriginalMCBVector()
    print(original_inputs)
    
    input_indices <- grep(grepname, inputs)
    
    print(paste0("resetMI: input_indices is: ", input_indices))
    
    count <- 1
    
    while (count < length(input_indices) + 1){
      #goes through all the input_indices that matches
      
      print("resetMI: In the while loop.")
      
      index <- input_indices[count]
      input_name <- inputs[index]
      id <- input_ids[index]
      value <- original_inputs[[index]]
      
      print(paste0("resetMI: index is: ", index))
      print(paste0("resetMI: input_name is: ", input_name))
      print(paste0("resetMI: id is: ", id))
      print(paste0("resetMI: value is: ", value))
      
      if (length(grep(" min", input_name, ignore.case = TRUE)) > 0){
        print("resetMI: In the min")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetMI: In the max")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else{
        print("resetMI: In the else")
        updateSelectInput(session, 
                          id,
                          label = NULL,
                          choices = c("All",unique(as.character(multi_pps_data[,checkbox_name]))),
                          selected =  "All"
        )
      }#end if-else for grep the input_name
      
      count <- count + 1
    }#end while for length of input_indices
    
    
  }#end resetMI
  
  multi_df_output <- reactiveValues(data = multi_pps_data) #end reactive for multi_df_output
  
  clean_multi_pps_data <- reactiveValues(data = NA) #this is the data to be downloaded
  
  observeEvent(multi_inputs(),{
    #'This will observe if any of the inputs of the parameters for multi extrusion have changed
    #'It does not check to see if the input has been selected, but rather, if the user has changed
    #'the search input.'
    
    print("Input Observed")
    
    if (exists("mivector", e2)){
      #checks to see if the vector has been created yet. This is to prevent the initialization
      #of the program
      
      old_mivector <- getMIVector() #get the old mivector before inputs were changed
      current_mivector <- multi_inputs()
      setMIVector(current_mivector) #updates the mivector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_mivector == old_mivector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed
        print("A parameter was changed but the value was changed to what the previous value was")
      }
      else{
        
        index_name <- names(current_mivector[index_differ])
        value <- current_mivector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("The value is null or na")
        }
        else{
          addMIStack(index_name) #adds the name to the stack
          current_mistack <- getMIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          
          stack_index <- grep(updated_index_name, current_mistack) #gets the index in the stack
          
          if (length(stack_index) != 1){
            print("The Stack Index has a length != 0")
            print(paste0("The Stack Index is: ", stack_index))
            stopApp() #terminate the program
          }
          else{
            addMDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addMDFSeries()
      
      print("observeEvent(multi_inputs): The multi_input vector does not exist.")
      
      setMIVector(multi_inputs())
      setMIStack(c()) #creates an empty stack since no parameters have been changed yet
    }
  })#end observeEvent for the user inputs
  
  observeEvent(show_vars2(),{
    #' this function will observe when a user checks or unchecks an input. If the input is unchecked,
    #' it removes any of the data cleaning it did in the stack and also resets the value of the
    #' input to the initialized value from the start of the session
    
    print("observeEvent(show_vars2): Checkbox Observed")
    
    if (exists("mcbvector", e2)){
      
      print("observeEvent(show_vars2): The mcbvector Exists")
      
      old_mcbvector <- getMCBVector() #get the old scbvector before inputs were changed
      current_mcbvector <- show_vars2()
      setMCBVector(current_mcbvector) #updates the scbvector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_mcbvector == old_mcbvector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed since the value was changed to TRUE
      }
      else{
        
        print("observeEvent(show_vars2): The Checkbox Index_Differ Worked")
        
        index_name <- names(current_mcbvector[index_differ])
        value <- current_mcbvector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("observeEvent(show_vars2): The checkbox value is null or na")
          stopApp()
        }
        else if (value == FALSE){
          #this ensures that the checkbox was unchecked instead of checked
          
          print("observeEvent(show_vars2): The Value of the Checkbox was FALSE")
          
          current_mistack <- getMIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          
          stack_index <- grep(updated_index_name, current_mistack) #gets the index in the stack
          
          if (length(stack_index) == 0){
            print("observeEvent(show_vars2): The Input was not in the Stack")
            #the input that was unchecked is not in the stack, so do nothing
          }
          else if (length(stack_index) == 1 || length(stack_index) == 2){
            #the parameter that was unchecked is in the stack
            #it can be of length 1 or 2 depending of the input parameter had a min and max value
            #if it is a selectize input, there will only be one match (length of 1)
            
            print("observeEvent(show_vars2): The Input was in the Stack")
            print(paste0("observeEvent(show_vars2): The Checkboxname is: ", index_name))
            
            resetMI(index_name) #resets the values of the inputs of the checkbox
            removeMIStack(index_name)
            removeMDFSeries() #removes the inputs associated with the checkbox
          }
          else{
            #none of the conditions were met, most likely multiple matches greater than 2
            print("observeEvent(show_vars2): The Stack Index for the Checkboxes ObserveEvent has a length > 2 or negative")
            stopApp()
          } #end if-else for the length of the stack index
          
        }
        else{
          #if it is not false, do nothing
          print("observeEvent(show_vars2): The Checkbox value was TRUE")
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector
      #this is mainly to initialize data.
      
      setOriginalMCBVector(multi_inputs()) #initialize the original values
      setMCBVector(show_vars2()) #initialize the input values that will be changing with the app
      
    } #end if-else for scbvector existing
    
    
  })#end observeEvent for the checkboxes
  

  
  # obtain the output of checkbox from functions and make a list to store them
  #this variable will store all the inputs of the multi-layer extrusions

  show_vars2<-reactive({
    
    checkboxes2 <- as.numeric(c(input$PCMPN_d,input$PCMPD_d,input$PCMRN_d,input$PCMRD_d,
                               input$PCMPPSN_d, input$PCMET_d, input$PCMB_d,
                               input$PCMDS_d,input$PCMDLL_d,input$PCMTS_d,
                               input$PCMTLL_d,input$PCMSP_d,input$PCMFT_d,
                               input$PCMBZT1_d,input$PCMBZT2_d,input$PCMBZT3_d,
                               input$PCMCT_d,input$PCMAT_d,input$PCMDT1_d,input$PCMDT2_d, input$PCMTE_d,
                               input$PCMIDI_d,input$PCMODI_d,input$PCMIWT_d,input$PCMMWT_d,
                               input$PCMOWT_d,input$PCMTWT_d,input$PCMOR_d,input$PCMCCT_d,
                               input$PCMLength_d,input$PCMToLength_d,input$PCMPPD_d,input$PCMNEXIV_d,
                               input$PCMAnnealed_d,input$PCMCaliper_d,input$PCMOS_d,input$PCMMP_d,
                               input$PCMHT_d,input$PCMSPD_d,input$PCMSLD_d,input$PCMDLN_d,
                               input$PCMULT_d,input$PCMVC_d,input$PCMIRD_d))
    
    names(checkboxes2) <- c("Part Number", "Part Description", "Resin Number", "Resin Description",
                           "PPS Number", "Extrusion Type", "Barrel",
                           "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)", "Tip Land Length (in)", "Screw Print",
                           "Feedthroat Temperature  F",
                           "Barrel Zone 1 Temperature  F", "Barrel Zone 2 Temperature  F",
                           "Barrel Zone 3 Temperature  F","Clamp Temperature  F",
                           "Adapter Temperature  F","Die 1 Temperature  F", "Die 2 Temperature  F",
                           "Tapered End",
                           "Inner Diameter (in)", "Outer Diameter (in)",
                           "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                           "Outer Wall Thickness (in)", "Total Wall Thickness (in)",
                           "Out of Roundness (in)",
                           "Concentricity (in)", "Length (in)", "Total Length",
                           "Perpendicularity (in)",
                           "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                           "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                           "Vacuum Calibration", "Irradiated")
    
    return(checkboxes2)
    
    })
  
  #this variable will store all the inputs of the multi extrusions
  multi_inputs <- reactive({
    #this variable will store all the inputs of of the multi extrusions
    inputs2 <- c(input$PCMPN, input$PCMPD, input$PCMRN, input$PCMRD, input$PCMPPSN, 
                input$PCMDS_min, input$PCMDS_max, input$PCMDLL, input$PCMTS_min, input$PCMTS_max,
                input$PCMTLL, input$PCMSP, 
                input$PCMFT_min, input$PCMFT_max, input$PCMBZT1_min, input$PCMBZT1_max,
                input$PCMBZT2_min, input$PCMBZT2_max, input$PCMBZT3_min, input$PCMBZT3_max,
                input$PCMCT_min, input$PCMCT_max, input$PCMAT_min, input$PCMAT_max,
                input$PCMDT1_min, input$PCMDT1_max, input$PCMDT2_min, input$PCMDT2_max,
                input$PCMIDI_min, input$PCMIDI_max, input$PCMODI_min, input$PCMODI_max,
                input$PCMIWT_min, input$PCMIWT_max, 
                input$PCMMWT_min, input$PCMMWT_max,
                input$PCMOWT_min, input$PCMOWT_max,
                input$PCMTWT_min, input$PCMTWT_max,
                input$PCMOR_min, input$PCMOR_max, 
                input$PCMCCT_min, input$PCMCCT_max, input$PCMLength_min, input$PCMLength_max,
                input$PCMPPD,
                input$PCMNEXIV, input$PCMAnnealed, input$PCMCaliper, input$PCMOS,
                input$PCMMP, input$PCMHT, input$PCMSPD, input$PCMSLD, input$PCMDLN, input$PCMULT,
                input$PCMVC, input$PCMIRD)
    
    names(inputs2) <- c("Part Number", "Part Description", "Resin Number", "Resin Description",
                       "PPS Number",
                       "Die Size (in) Min", "Die Size (in) Max", "Die Land (in) Length", 
                       "Tip Size (in) Min","Tip Size (in) Max", "Tip Land (in) Length", "Screw Print",
                       "Feedthroat Temperature  F Min", "Feedthroat Temperature  F Max",
                       "Barrel Zone 1 Temperature  F Min", "Barrel Zone 1 Temperature  F Max",
                       "Barrel Zone 2 Temperature  F Min", "Barrel Zone 2 Temperature  F Max",
                       "Barrel Zone 3 Temperature  F Min", "Barrel Zone 3 Temperature  F Max",
                       "Clamp Temperature  F Min", "Clamp Temperature  F Max",
                       "Adapter Temperature  F Min", "Adapter Temperature  F Max",
                       "Die 1 Temperature  F Min", "Die 1 Temperature  F Max",
                       "Die 2 Temperature  F Min", "Die 2 Temperature  F Max",
                       "Inner Diameter (in) Min", "Inner Diameter (in) Max", "Outer Diameter (in) Min",
                       "Outer Diameter (in) Max", 
                       "Inner Wall Thickness (in) Min", "Inner Wall Thickness (in) Max",
                       "Middle Wall Thickness (in) Min", "Middle Wall Thickness (in) Max",
                       "Outer Wall Thickness (in) Min", "Outer Wall Thickness (in) Max",
                       "Total Wall Thickness (in) Min", "Total Wall Thickness (in) Max",
                       "Out of Roudness (in) Min", "Out of Roundness (in) Max", 
                       "Concentricity (in) Min", "Concentricity (in) Max",
                       "Length (in) Min", "Length (in) Max", "Perpendicularity (in", 
                       "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                       "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                       "Vacuum Calibration", "Irradiated")
    return(inputs2)
  })
  
  
  
  #All Table Filter works are done below, and output a modified talbe. This table will be used by the renderdatatable, and download button
  datasetInput<-reactive({
    col_var2=show_vars2()
    for (i in 1:length(col_var2)){
      if (col_var2[i]!=0){
        Col_PCM=c(Col_PCM,i) # obtain the selected columns' number
      }
    } 
    data_PCM<-multi_pps_data[,Col_PCM]
    data_PCM
  })
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({

      Col_PCM=c()
      col_var2=show_vars2()
      for (i in 1:length(col_var2)){
        if (col_var2[i]!=0){
          Col_PCM=c(Col_PCM,i)
        }
      } 
  
      data_PCM <- multi_df_output$data #the data frame is set
      data_PCM<-data_PCM[,Col_PCM]
      
      clean_multi_pps_data$data <- data_PCM #assign the clean table to the data that is available
      #for downloading
      
      rows <- nrow(data_PCM)
      vectorofbuttons <- c(rep(0, rows))
      row_count <- 1
      
      while(row_count < rows + 1){
        #this creates a vector of html action buttons to add to the table
        vectorofbuttons[row_count] <- as.character(
          actionButton(inputId = paste0("button_", data_PCM[row_count,1]),
                       label = "Add Part",
                       onclick = 'Shiny.onInputChange(\"add_button\",  this.id)')
        )
        row_count <- row_count + 1
      } #end while adding the html stuff
      
      data_PCM$"" <- vectorofbuttons
      data_PCM <- data_PCM[,c(ncol(data_PCM), 1:(ncol(data_PCM)-1))]
      
      return(data_PCM)

    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE))
  },
  filter = "top",
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE)#END Multi Extrusion PPS Data
  
  
  

  ###
  
  ### The multi-layer shopping cart section ###
  
  ###
  
  
  multishoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the multi extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end multishoppingcart
  
  observeEvent(input$multiadd_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$multiadd_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"multidelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- multi_tari_parameter_data$`SAP Batch Number`[multi_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"multidelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    multishoppingcart$data <- rbind(multishoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(multishoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  })
  
  
  observeEvent(input$multidelete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$multidelete_part_button, "_")[[1]][2]
    multishoppingcart$data <- multishoppingcart$data[multishoppingcart$data$'Part' != part,]
  })
  
  observeEvent(input$multidelete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$multidelete_batch_button, "_")[[1]][2]
    multishoppingcart$data <- multishoppingcart$data[multishoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$multishoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(multishoppingcart$data)
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE) #for the shoppingcart
  
  output$downloadMPPSData <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Multi-Layer PPS Data", '.csv', sep='') },
    content = function(file) {
      write.csv(clean_multi_pps_data$data, file)
    }
  )
  
  
  
  #### Tapered PPS ####
  
  e3 <- new.env(
    #This environment will store variable of inputs and stack that are used for comparison
    #of the input parameters that have been selected and changed
    #' variables to contain:
    #' tivector - a vector that will store the inputs of the tapered extrusion parameters
    #' tistack - the stack for the tapered inputs
  ) #creates a new environment to store instance variables
  
  #the assign will initialize the tiidvector
  assign("tiidvector", 
         c("input$PCSPN", "input$PCSPD", "input$PCSRN", "input$PCSRD", "input$PCSPPSN", 
           "input$PCSDS_min", "input$PCSDS_max", "input$PCSDLL", "input$PCSTS_min", "input$PCSTS_max",
           "input$PCSTLL", "input$PCSSP", 
           "input$PCSFT_min", "input$PCSFT_max", "input$PCSBZT1_min", "input$PCSBZT1_max",
           "input$PCSBZT2_min", "input$PCSBZT2_max", "input$PCSBZT3_min", "input$PCSBZT3_max",
           "input$PCSCT_min", "input$PCSCT_max", "input$PCSAT_min", "input$PCSAT_max",
           "input$PCSDT1_min", "input$PCSDT1_max", "input$PCSDT2_min", "input$PCSDT2_max",
           "input$PCTPIDI_min", "input$PCTPIDI_max", "input$PCTPODI_min", "input$PCTPODI_max", "input$PCTPWT_min", 
           "input$PCTPWT_max","input$PCTPOR_min", "input$PCTPOR_max", "input$PCTPCCT_min", "input$PCTPCCT_max",
           "input$PCTDIDI_min", "input$PCTDIDI_max", "input$PCTDODI_min", "input$PCTDODI_max", "input$PCTDWT_min",
           "input$PCTDWT_max", "input$PCTDOR_min", "input$PCTDOR_max", "input$PCTDCCT_min", "input$PCTDCCT_max",
           "input$PCTPLength_min", "input$PCTPLength_max", "input$PCTTLength_min", "input$PCTTLength_max",
           "input$PCTDLength_min", "input$PCTDLength_max", "input$PCTToLength_min", "input$PCTToLength_max",
           "input$PCSPPD",
           "input$PCSNEXIV", "input$PCSAnnealed", "input$PCSCaliper", "input$PCSOS",
           "input$PCSMP", "input$PCSHT", "input$PCSSPD", "input$PCSSLD", "input$PCSDLN", "input$PCSULT",
           "input$PCSVC", "input$PCSIRD"), 
         envir = e3)
  
  
  ## These are the setter and getter function ##
  
  setTIIDVector <- function(vector){
    #set the vector for the input ids
    
    assign("tiidvector", vector, env = e3)
    
  } #end setTIIDVector
  
  getTIIDVector <- function(){
    #returns the IDs of the inputs
    
    if(exists("tiidvector", e3)){
      return(get("tiidvector", e3))
    }
    else{
      return(NA)
    }
  } #end SIIDVector
  
  setTCBVector <- function(vector){
    #this sets the values for the tapered input vector
    assign("tcbvector", vector, env = e3)
  } #end setTCVector
  
  getTCBVector <- function(vector){
    #this gets the values for the tapered checkbox vector
    
    if(exists("tcbvector", e3)){
      return(get("tcbvector", e3))
    }
    else{
      return(NA)
    }
    
  } #end getTCBVector
  
  setOriginalTIVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originaltcbvector", vector, env = e3)
  } #end setOriginalTIVector
  
  getOriginalTIVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originaltcbvector", e3)){
      return(get("originaltcbvector", e3))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalTIVector
  
  setTIVector <- function(vector){
    #this sets the values for the tapered input vector
    assign("tivector", vector, env = e3)
  } #end setTIVector
  
  getTIVector <- function(){
    #returns tivector
    
    if(exists("tivector", e3)){
      return(get("tivector", e3))
    }
    else{
      return(NA)
    }
    
  } #end getTIVector
  
  setTIStack <- function(stack){
    #this sets the stack for the tapered inputs
    assign("tistack", stack, env = e3)
  } #end setTIStack
  
  getTIStack <- function(){
    #returns tivector
    if(exists("tistack", e3)){
      return(get("tistack", e3))
    }
    else{
      return(NA)
    }
    
  } #end getTIStack
  
  getTIStack.length <- function(){
    #returns the length of the SIStack
    
    if(exists("tistack", e3)){
      #if it exists, return the length
      return(length(getTIStack()))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getTIStack.length
  
  addTIStack <- function(name){
    #this function adds a new parameter to the stack
    #it will only add to the stack if it is a unique name
    
    stack <- get("tistack", e3)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    
    if (length(grep(rep_name, stack)) == 0){
      #the name is not currently in the stack
      stack <- c(stack, name) #add the name to the top of the stack (the right most)
      setTIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is already present
    }
    
  } #end  addTIStack
  
  removeTIStack <- function(name){
    #this function removes the inputs associated with the checkbox name from the stack
    
    print("removeTIStack: We are in the removeTIStack")
    
    stack <- get("tistack", e3)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    
    if (length(grep(rep_name, stack)) != 0){
      #the name is not currently in the stack
      indices <- grep(rep_name, stack)
      
      if (length(indices) == length(stack)){
        #if the length of the indices are equal to the stack
        print("removeTIStack: The length of the indices are equal to the stack")
        stack <- c()
      }
      else if (length(indices) == 1){
        print("removeTIStack: There was only one match in the stack")
        #if there is only one match
        if (indices == 1){
          #if the first matches is 1 or the only match is one
          stack <- stack[c(2:length(stack))] # removes the first element
        }
        else if (indices == length(stack)){
          stack <- stack[c(1:(indices - 1))]
        }
        else{
          #removes the index from the stack
          stack <- stack[c(1:(indices-1), (indices + 1):length(stack))]
        }#end if-else for the vallue of indices
      }
      else if (length(indices) == 2){
        print("removeTIStack: There were multiple matches to the stack")
        #if there are multiple indices
        if (indices[1] == 1){
          #if the first matches is 1 or the only match is one
          
          if (indices[2] == 2){
            stack <- stack[c(3:length(stack))] # removes the first and second element
          }
          else if (indices[2] == length(stack)){
            stack <- stack[c(2:(indices[2] - 1))]
          }
          else{
            stack <- stack[c(2:(indices[2] - 1), (indices[2] + 1):length(stack))] # the indices
          }#end if-else for the value of the second index
          
        }
        else if(indices[1] == (length(stack) - 1)){
          #if the first index is length(stack)-1, then the second index is the length of the stack
          stack <- stack[c(1:(indices[1] - 1))]
        }
        else if (indices[1] == (indices[2] + 1)){
          #if the indices are right next to each other but are not at the ends of the stack
          stack <- stack[c(1:(indices[1]-1), (indices[2] + 1):length(stack))]
        }
        else{
          #removes the indices from the stack
          stack <- stack[c(1:(indices[1]-1), (indices[1] + 1):(indices[2] - 1), (indices[2] + 1):length(stack))]
        }#end if-else for the value of indices
        
      }#end if-else for the length of indices
      else{
        #if it was neither, there was an error and the app shoudl stop
        print("The indices in the removeTIStack were more than 2")
        print(paste0("The indices are: ", indices))
        stop()
      }
      
      
      setTIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is not in the stack
      print("removeTIStack: Nothing was done because the name was not in the stack")
    }
    
  } #end  removeTIStack
  
  setTDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e3)
  }#end setTDFList
  
  getTDFList <- function(){
    #returns the df_list
    if(exists("df_list", e3)){
      #if it exists, return the length
      return((get("df_list", e3)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getTDFList
  
  addTDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e3)){
      #if the list already exists
      
      if(index == getTIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getTDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setTDFList(df_list) #set the new list
        
        tapered_df_output$data <- new_df #set the data table to this
        
      }
      else if(index > getTIStack.length() + 1){
        #if there was an error in the index and it was too large
        print("The index for the stack was too large and greater than the allowable limit")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getTIStack.length(), "."))
        stopApp()
      }
      else if(index < 1){
        #error in the index
        print("The index for the stack was zero or negative")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getTIStack.length(), "."))
        stopApp()
      }
      else{
        #the index is within the stack which means we means a previous parameter is being changed
        stack <- getTIStack()
        stack_length <- getTIStack.length()
        
        input_values <- getTIVector()
        
        df_list <- getTDFList() #gets latest df_list
        
        if (index == 1){
          #if it is starting from the beginning
          
          new_df <- generateNewDF(tapered_pps_data, index_name, value) #starts from this initial one
          df_list[[index]] <- new_df #add the new data frame
          
          count <- index + 1
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]]
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            count <- count + 1
            df_list[[count]] <- new_df #add the new data frame
          }#end while creating new DFs
          
          setTDFList(df_list) #set the new list
          tapered_df_output$data <- new_df #set the data table to this
          
        }
        else{
          
          count <- index
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]] #gets the previous df
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            
            df_list[[count]] <- new_df #add the new data frame
            
            count <- count + 1
          }#end while creating new DFs
          
          setTDFList(df_list) #set the new list
          tapered_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e3) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewDF(tapered_pps_data, index_name, value) #starts from this initial one
      df_list <- getTDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setTDFList(df_list) #set the new list
      tapered_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addTDFSeries
  
  removeTDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeTDFSeries: We are in removeTDFSeries")
    
    if (exists("df_list", e3)){
      #if the list already exists
      
      print("removeTDFSeries: df_list exists")
      
      stack <- getTIStack() #gets the stack
      stack_length <- getTIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setTDFList(NULL) #sets the list to NULL so it can be reinitialized
        tapered_df_output$data <- tapered_pps_data
        
        print("removeTDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeTDFSeries: The stack_length was NOT zero")
        
        input_values <- getTIVector() #gets the input_values
        df_list <- getTDFList() #gets latest df_list
        
        current_parameter <- stack[1] #gets the first parameter in the stack
        current_parameter_value <- input_values[current_parameter] #gets the parameters value
        
        new_df <- generateNewDF(tapered_pps_data, current_parameter, current_parameter_value) #starts from this initial one
        df_list[[1]] <- new_df #add the new data frame
        
        count <- 2
        
        while (count < stack_length + 1){
          
          previous_df <- df_list[[count-1]]
          current_parameter <- stack[count]
          current_parameter_value <- input_values[current_parameter]
          
          new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
          count <- count + 1
          df_list[[count]] <- new_df #add the new data frame
        }#end while creating new DFs
        
        if (count < (length(df_list) + 1)){
          #'because we recreate the df_list from the original df_list and th new df_list will not
          #'have the dataframes from the removed parameters, this new df_list will be shorter than
          #'the original before the parameters were removed. Thus we resize the df_list
          
          while ((length(df_list) + 1) > count){
            #the df_list will continually be resized as the inputs are made null, thus the condition
            #is met when all the element greater than an index of count are removed
            
            df_list[[count]] <- NULL
            
          } #end while for making the elements NULL
          
          
        }#end if for the count compared to the df_list length
        
        setTDFList(df_list) #set the new list
        tapered_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
      
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
    
    
  } #end removeTDFSeries
  
  resetTI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetTI: Resetting the Input Values")
    print(paste0("resetTI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    
    inputs <- names(isolate(tapered_inputs()))
    input_ids <- getTIIDVector()
    original_inputs <- getOriginalTIVector()
    print(original_inputs)
    
    input_indices <- grep(grepname, inputs)
    
    print(paste0("resetTI: input_indices is: ", input_indices))
    
    count <- 1
    
    while (count < length(input_indices) + 1){
      #goes through all the input_indices that matches
      
      print("resetTI: In the while loop.")
      
      index <- input_indices[count]
      input_name <- inputs[index]
      id <- input_ids[index]
      value <- original_inputs[[index]]
      
      print(paste0("resetTI: index is: ", index))
      print(paste0("resetTI: input_name is: ", input_name))
      print(paste0("resetTI: id is: ", id))
      print(paste0("resetTI: value is: ", value))
      
      if (length(grep(" min", input_name, ignore.case = TRUE)) > 0){
        print("resetTI: In the min")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetTI: In the max")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else{
        print("resetTI: In the else")
        updateSelectInput(session, 
                          id,
                          label = NULL,
                          choices = c("All",unique(as.character(tapered_pps_data[,checkbox_name]))),
                          selected =  "All"
        )
      }#end if-else for grep the input_name
      
      count <- count + 1
    }#end while for length of input_indices
    
    
  }#end resetTI
  
  tapered_df_output <- reactiveValues(data = tapered_pps_data) #end reactive for tapered_df_output
  
  clean_tapered_pps_data <- reactiveValues(data = NA) #the clean data to be downloaded
  
  observeEvent(tapered_inputs(),{
    #'This will observe if any of the inputs of the parameters for tapered extrusion have changed
    #'It does not check to see if the input has been selected, but rather, if the user has changed
    #'the search input.'
    
    print("Input Observed")
    
    if (exists("tivector", e3)){
      #checks to see if the vector has been created yet. This is to prevent the initialization
      #of the program
      
      old_tivector <- getTIVector() #get the old tivector before inputs were changed
      current_tivector <- tapered_inputs()
      setTIVector(current_tivector) #updates the tivector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_tivector == old_tivector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed
        print("A parameter was changed but the value was changed to what the previous value was")
      }
      else{
        
        index_name <- names(current_tivector[index_differ])
        value <- current_tivector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("The value is null or na")
        }
        else{
          addTIStack(index_name) #adds the name to the stack
          current_tistack <- getTIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          
          stack_index <- grep(updated_index_name, current_tistack) #gets the index in the stack
          
          if (length(stack_index) != 1){
            print("The Stack Index has a length != 0")
            print(paste0("The Stack Index is: ", stack_index))
            stopApp() #terminate the program
          }
          else{
            addTDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addTDFSeries()
      
      setTIVector(tapered_inputs())
      setTIStack(c()) #creates an empty stack since no parameters have been changed yet
    }
  })#end observeEvent for the user inputs
  
  observeEvent(show_vars3(),{
    #' this function will observe when a user checks or unchecks an input. If the input is unchecked,
    #' it removes any of the data cleaning it did in the stack and also resets the value of the
    #' input to the initialized value from the start of the session
    
    print("observeEvent(show_vars3): Checkbox Observed")
    
    if (exists("tcbvector", e3)){
      
      print("observeEvent(show_vars3): The tcbvector Exists")
      
      old_tcbvector <- getTCBVector() #get the old tcbvector before inputs were changed
      current_tcbvector <- show_vars3()
      setTCBVector(current_tcbvector) #updates the tcbvector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_tcbvector == old_tcbvector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed since the value was changed to TRUE
      }
      else{
        
        print("observeEvent(show_vars3): The Checkbox Index_Differ Worked")
        
        index_name <- names(current_tcbvector[index_differ])
        value <- current_tcbvector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("observeEvent(show_vars3): The checkbox value is null or na")
          stopApp()
        }
        else if (value == FALSE){
          #this ensures that the checkbox was unchecked instead of checked
          
          print("observeEvent(show_vars3): The Value of the Checkbox was FALSE")
          
          current_tistack <- getTIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          
          stack_index <- grep(updated_index_name, current_tistack) #gets the index in the stack
          
          if (length(stack_index) == 0){
            print("observeEvent(show_vars3): The Input was not in the Stack")
            #the input that was unchecked is not in the stack, so do nothing
          }
          else if (length(stack_index) == 1 || length(stack_index) == 2){
            #the parameter that was unchecked is in the stack
            #it can be of length 1 or 2 depending of the input parameter had a min and max value
            #if it is a selectize input, there will only be one match (length of 1)
            
            print("observeEvent(show_vars3): The Input was in the Stack")
            print(paste0("observeEvent(show_vars3): The Checkboxname is: ", index_name))
            
            resetTI(index_name) #resets the values of the inputs of the checkbox
            removeTIStack(index_name) #removes the value from the stack
            removeTDFSeries() #removes the inputs associated with the checkbox
          }
          else{
            #none of the conditions were met, most likely multiple matches greater than 2
            print("observeEvent(show_vars3): The Stack Index for the Checkboxes ObserveEvent has a length > 2 or negative")
            stopApp()
          } #end if-else for the length of the stack index
          
        }
        else{
          #if it is not false, do nothing
          print("observeEvent(show_vars3): The Checkbox value was TRUE")
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector
      #this is mainly to initialize data.
      
      setOriginalTIVector(tapered_inputs()) #initialize the original values
      setTCBVector(show_vars3()) #initialize the input values that will be changing with the app
      
    } #end if-else for tcbvector existing
    
    
  })#end observeEvent for the checkboxes
  

  
  
  # obtain the output of checkbox from functions and make a list to store them----multi Extrusion PPS Data

  
  show_vars3<-reactive({
    checkboxes <- as.numeric(c(input$PCTPN_d,input$PCTPD_d,input$PCTRN_d,input$PCTRD_d,
                               input$PCTPPSN_d,input$PCTDS_d,input$PCTDLL_d,input$PCTTS_d,
                               input$PCTTLL_d,input$PCTSP_d,input$PCTFT_d,
                 input$PCTBZT1_d,input$PCTBZT2_d,input$PCTBZT3_d,input$PCTCT_d,input$PCTAT_d,
                 input$PCTDT1_d,input$PCTDT2_d,
                 input$PCTPIDI_d, input$PCTPODI_d, input$PCTPWT_d, input$PCTPOR_d, input$PCTPCCT_d,
                 input$PCTDIDI_d, input$PCTDODI_d, input$PCTDWT_d, input$PCTDOR_d, input$PCTDCCT_d,
                 input$PCTPLength_d, input$PCTTLength_d, input$PCTDLength_d, input$PCTToLength_d,
                 input$PCTPPD_d,
                 input$PCTNEXIV_d,input$PCTAnnealed_d,input$PCTCaliper_d,input$PCTOS_d,
                 input$PCTMP_d,input$PCTHT_d,
                 input$PCTSPD_d,input$PCTSLD_d,input$PCTDLN_d,input$PCTULT_d,input$PCTVC_d,
                 input$PCTIRD_d))
    
    names(checkboxes) <- c("Part Number", "Part Description", "Resin Number", "Resin Description",
                           "PPS Number",
                           "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)", "Tip Land Length (in)", "Screw Print",
                           "Feedthroat Temperature  F",
                           "Barrel Zone 1 Temperature  F", "Barrel Zone 2 Temperature  F",
                           "Barrel Zone 3 Temperature  F","Clamp Temperature  F",
                           "Adapter Temperature  F","Die 1 Temperature  F", "Die 2 Temperature  F",
                           "Proximal Inner Diameter (in)", "Proximal Outer Diameter (in)",
                           "Proximal Wall Thickness (in)", "Proximal Out of Roundness (in)",
                           "Proximal Concentricity (in)",
                           "Distal Inner Diameter (in)", "Distal Outer Diameter (in)",
                           "Distal Wall Thickness (in)", "Distal Out of Roundness (in)",
                           "Distal Concentricity (in)",
                           "Proximal Length (in)", "Transition Length (in)", "Distal Length (in)",
                           "Total Length (in)", "Perpendicularity (in)",
                           "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                           "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                           "Vacuum Calibration", "Irradiated")
    
    return(checkboxes)
    
    })
  
  #this variable will store all the inputs of the tapered extrusions
  tapered_inputs <- reactive({
    #this variable will store all the inputs of of the tapered extrusions
    inputs3 <- c(input$PCTPN, input$PCTPD, input$PCTRN, input$PCTRD, input$PCTPPSN, 
                input$PCTDS_min, input$PCTDS_max, input$PCTDLL, input$PCTTS_min, input$PCTTS_max,
                input$PCTTLL, input$PCTSP, 
                input$PCTFT_min, input$PCTFT_max, input$PCTBZT1_min, input$PCTBZT1_max,
                input$PCTBZT2_min, input$PCTBZT2_max, input$PCTBZT3_min, input$PCTBZT3_max,
                input$PCTCT_min, input$PCTCT_max, input$PCTAT_min, input$PCTAT_max,
                input$PCTDT1_min, input$PCTDT1_max, input$PCTDT2_min, input$PCTDT2_max,
                input$PCTPIDI_min, input$PCTPIDI_max, input$PCTPODI_min, input$PCTPODI_max, input$PCTPWT_min, 
                input$PCTPWT_max,input$PCTPOR_min, input$PCTPOR_max, input$PCTPCCT_min, input$PCTPCCT_max,
                input$PCTDIDI_min, input$PCTDIDI_max, input$PCTDODI_min, input$PCTDODI_max, input$PCTDWT_min,
                input$PCTDWT_max, input$PCTDOR_min, input$PCTDOR_max, input$PCTDCCT_min, input$PCTDCCT_max,
                input$PCTPLength_min, input$PCTPLength_max, input$PCTTLength_min, input$PCTTLength_max,
                input$PCTDLength_min, input$PCTDLength_max, input$PCTToLength_min, input$PCTToLength_max,
                input$PCTPPD,
                input$PCTNEXIV, input$PCTAnnealed, input$PCTCaliper, input$PCTOS,
                input$PCTMP, input$PCTHT, input$PCTSPD, input$PCTSLD, input$PCTDLN, input$PCTULT,
                input$PCTVC, input$PCTIRD
    )
    names(inputs3) <- c("Part Number", "Part Description", "Resin Number", "Resin Description",
                       "PPS Number",
                       "Die Size (in) Min", "Die Size (in) Max", "Die Land (in) Length", 
                       "Tip Size (in) Min","Tip Size (in) Max", "Tip Land (in) Length", "Screw Print",
                       "Feedthroat Temperature  F Min", "Feedthroat Temperature  F Max",
                       "Barrel Zone 1 Temperature  F Min", "Barrel Zone 1 Temperature  F Max",
                       "Barrel Zone 2 Temperature  F Min", "Barrel Zone 2 Temperature  F Max",
                       "Barrel Zone 3 Temperature  F Min", "Barrel Zone 3 Temperature  F Max",
                       "Clamp Temperature  F Min", "Clamp Temperature  F Max",
                       "Adapter Temperature  F Min", "Adapter Temperature  F Max",
                       "Die 1 Temperature  F Min", "Die 1 Temperature  F Max",
                       "Die 2 Temperature  F Min", "Die 2 Temperature  F Max",
                       "Proximal Inner Diameter (in) Min", 
                       "Proximal Inner Diameter (in) Max", 
                       "Proximal Outer Diameter (in) Min",
                       "Proximal Outer Diameter (in) Max",
                       "Proximal Wall Thickness (in) Min", 
                       "Proximal Wall Thickness (in) Max", 
                       "Proximal Out of Roundness (in) Min",
                       "Proximal Out of Roundness (in) Max",
                       "Proximal Concentricity (in) Min",
                       "Proximal Concentricity (in) Max",
                       "Distal Inner Diameter (in) Min", 
                       "Distal Inner Diameter (in) Max",
                       "Distal Outer Diameter (in) Min",
                       "Distal Outer Diameter (in) Max",
                       "Distal Wall Thickness (in) Min", 
                       "Distal Wall Thickness (in) Max", 
                       "Distal Out of Roundness (in) Min",
                       "Distal Out of Roundness (in) Max",
                       "Distal Concentricity (in) Min",
                       "Distal Concentricity (in) Max",
                       "Proximal Length (in) Min", 
                       "Proximal Length (in) Max",
                       "Transition Length (in) Min", 
                       "Transition Length (in) Max", 
                       "Distal Length (in) Min",
                       "Distal Length (in) Max",
                       "Total Length (in) Min",
                       "Total Length (in) Max",
                       "Perpendicularity (in", 
                       "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                       "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                       "Vacuum Calibration", "Irradiated")
    return(inputs3)
  })
  
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable({
      Col_PCT=c()
      col_var3=show_vars3()
      for (i in 1:length(col_var3)){
        if (col_var3[i]!=0){
          Col_PCT=c(Col_PCT,i)
        }
      } 
      data_PCT<-tapered_pps_data[,Col_PCT]
      
      data_PCT <- tapered_df_output$data #the data frame is set
      data_PCT <- data_PCT[,Col_PCT] #only get the columns that have been checked
      
      clean_tapered_pps_data$data <- data_PCT #assign the clean table to the data that is available
      #for downloading
      
      rows <- nrow(data_PCT)
      vectorofbuttons <- c(rep(0, rows))
      row_count <- 1
      
      while(row_count < rows + 1){
        #this creates a vector of html action buttons to add to the table
        vectorofbuttons[row_count] <- as.character(
          actionButton(inputId = paste0("button_", data_PCT[row_count,1]),
                       label = "Add Part",
                       onclick = 'Shiny.onInputChange(\"taperedadd_button\",  this.id)')
        )
        row_count <- row_count + 1
      } #end while adding the html stuff
      
      data_PCT$"" <- vectorofbuttons
      data_PCT <- data_PCT[,c(ncol(data_PCT), 1:(ncol(data_PCT)-1))]
      
      
      return(data_PCT)
    },
    options = list(orderClasses = TRUE,
                   columnDefs = list(list(className = 'dt-center',
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE)
    )
  },
  filter = "top",
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE) #end Tapered Extrusion PPS Data
  
  
  
  
  ###
  
  ### The tapered shopping cart section ###
  
  ###
  
  
  taperedshoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the singl extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end taperedshoppingcart
  
  observeEvent(input$taperedadd_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$taperedadd_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"tapereddelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- tapered_tari_parameter_data$`SAP Batch Number`[tapered_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"tapereddelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    taperedshoppingcart$data <- rbind(taperedshoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(taperedshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  })
  
  
  observeEvent(input$tapereddelete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$tapereddelete_part_button, "_")[[1]][2]
    taperedshoppingcart$data <- taperedshoppingcart$data[taperedshoppingcart$data$'Part' != part,]
  })
  
  observeEvent(input$tapereddelete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$tapereddelete_batch_button, "_")[[1]][2]
    taperedshoppingcart$data <- taperedshoppingcart$data[taperedshoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$taperedshoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(taperedshoppingcart$data)
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE) #for the shoppingcart
  

  output$downloadTPPSData <- downloadHandler(
    #downlaod the data
    filename = function() { paste0("Tapered PPS Data", ".csv") },
    content = function(file) {
      write.csv(clean_tapered_pps_data$data, file)
    }
  )
  
  
  
  
  #### EXTRA ####
  

  output$singleMESparameters <- renderDataTable({
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    data <- single_tari_parameter_data[single_tari_parameter_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$singleMEStime <- renderDataTable({
    #This returns the table of the MES input times based on the SAP batch numbers in the
    #shopping cart
    data <- single_tari_time_data[single_tari_time_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$singleMESsubmitter <- renderDataTable({
    #This returns the table of the MES submitter IDs based on the SAP batch numbers in the
    #shopping cart
    data <- single_tari_submitter_data[single_tari_submitter_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$singleMEStotal <- renderDataTable({
    #This returns the table of all MES inputs based on the SAP batch numbers in the
    #shopping cart
    data <- single_tari_total_data[single_tari_total_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$singlescrapcodes <- renderDataTable({
    #This returns the table of SAP scrap codes based on the SAP batch numbers in the
    #shopping cart
    data <- scrapcodes_data[scrapcodes_data$Order %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  #Testing the appstats data
  # output$nexiv <- renderDataTable({
  #   #This returns the table of the Applied Stats Nexiv Data based on the SAP batch numbers in the
  #   #shopping cart
  #   data <- nexiv[nexiv$`Batch #` %in% singleshoppingcart$data$'SAP Batch',]
  #   return(data)
  # },
  # filter = "top")
  
  # output$laserlinc <- renderDataTable({
  #   #This returns the table of the Applied Stats laserlinc data based on the SAP batch numbers in the
  #   #shopping cart
  #   data <- ll[ll$`Lot Number` %in% singleshoppingcart$data$'SAP Batch',]
  #   return(data)
  # },
  # filter = "top")
  # end Single Extrusion PPS Data Server part and Shopping cart
  
  
  # Error Message---Manually add part number to shopping cart, if there is no such part number in the datadase, then return a error message
  # observeEvent(input$singleMadd_button,{
  #   showModal(modalDialog(
  #     title = "Add Part Number",
  #     "Success",
  #     easyClose = T
  #   ))
  # })
  
  
# Test--SHow The numbe of part number that have been added to shopping cart
  #output$ShoppingCart_Count<-renderText({
   # Count<-count(singleshoppingcartparts$data)
    #Count
  #})
  
  
  
  
  
  
# Add Manually part number input button
  
  observeEvent(input$singleMadd_button,{
    #used by single Mnanually add button in the shopping cart
    #get the part from the text input box
    part <- input$SinglePartNum_input
    
      showModal(modalDialog(
        title = "Add Part Number",
        "Success",
        easyClose = T
      ))
      
      
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"singledelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    singleshoppingcart$data <- rbind(singleshoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(singleshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  })
  
  observeEvent(input$singleMadd_button,{
    #this observes whether the user manually add a part to shopping cart
    part <- input$SinglePartNum_input
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    new_data <- cbind(part, deletepart)
    
    colnames(new_data) <- c("Part", "Delete Part")
    singleshoppingcartparts$data <- rbind(singleshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
    colnames(singleshoppingcartparts$data) <- c("Part", "Delete Part")
    updateTextInput(session,"SinglePartNum_input",value = "") #update input box, reset it to be blank
  })
  
}
  





# Run the application 
shinyApp(ui = ui, server = server)
