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
  
  setOriginalSCBVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originalscbvector", vector, env = e1)
  } #end setOriginalSCBVector
  
  getOriginalSCBVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originalscbvector", e1)){
      return(get("originalscbvector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalSCBVector
  
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
  
  setDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e1)
  }#end setDFList
  
  getDFList <- function(){
    #returns the df_list
    if(exists("df_list", e1)){
      #if it exists, return the length
      return((get("df_list", e1)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getDFList
  
  addDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e1)){
      #if the list already exists
      
      if(index == getSIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setDFList(df_list) #set the new list
        
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
        
        df_list <- getDFList() #gets latest df_list
        
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
          
          setDFList(df_list) #set the new list
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
          
          setDFList(df_list) #set the new list
          single_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e1) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewDF(single_pps_data, index_name, value) #starts from this initial one
      df_list <- getDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setDFList(df_list) #set the new list
      single_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addDFSeries
  
  removeDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeDFSeries: We are in removeDFSeries")
    
    if (exists("df_list", e1)){
      #if the list already exists
      
      print("removeDFSeries: df_list exists")
      
      stack <- getSIStack() #gets the stack
      stack_length <- getSIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setDFList(NULL) #sets the list to NULL so it can be reinitialized
        single_df_output$data <- single_pps_data
        
        print("removeDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeDFSeries: The stack_length was NOT zero")
        
        input_values <- getSIVector() #gets the input_values
        df_list <- getDFList() #gets latest df_list
        
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
        
        print(head(new_df[,1:4]))
        
        setDFList(df_list) #set the new list
        single_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
        
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
      
    
  } #end removeDFSeries
  
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
  
  resetSI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetSI: Resetting the Input Values")
    print(paste0("resetSI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    
    inputs <- names(isolate(single_inputs()))
    input_ids <- getSIIDVector()
    original_inputs <- getOriginalSCBVector()
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
            addDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addDFSeries()
      
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
            removeDFSeries() #removes the inputs associated with the checkbox
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

      setOriginalSCBVector(single_inputs()) #initialize the original values
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
                           "Die Size (in)", "Die Land (in)",
                           "Tip Size (in)", "Tip Land (in)", "Screw Print",
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
  
  #this variable will store all the inputs of of the single extrusions
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
      for (i in 1:length(col_var1)){
        #this will go through col_var1 and determine which parameters have been checked
        #only the parameters that have been checked will be displayed on the table
        if (col_var1[i]!=0){
          Col_PCS=c(Col_PCS,i)
        }
      }
      
      data_PCS <- single_df_output$data #the data frame is set
      data_PCS <- data_PCS[,Col_PCS] #only get the columns that have been checked
      
      
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
     
  
  

  #### Multi-Layer PPS ####
  #Checkbox
  output$PCMPN_s<-renderUI({
    checkboxInput("PCMPN_d","Part Number",value=TRUE)
  })
  output$PCMPD_s<-renderUI({
    checkboxInput("PCMPD_d","Part Description",value=TRUE)
  })
  output$PCMRN_s<-renderUI({
    checkboxInput("PCMRN_d","Resin Number",value=TRUE)
  })
  output$PCMRD_s<-renderUI({
    checkboxInput("PCMRD_d","Resin Description",value=TRUE)
  })
  output$PCMPPSN_s<-renderUI({
    checkboxInput("PCMPPSN_d","PPS Number",value=F)
  })
  #Tooling
  output$PCMDS_s<-renderUI({
    checkboxInput("PCMDS_d","Die Size (in)",value=T)
  })
  output$PCMDLL_s<-renderUI({
    checkboxInput("PCMDLL_d","Die Land Length (in)",value=F)
  })
  output$PCMTS_s<-renderUI({
    checkboxInput("PCMTS_d","Tip Size (in)",value=T)
  })
  output$PCMTLL_s<-renderUI({
    checkboxInput("PCMTLL_d","Tip Land Length (in)",value=F)
  })
  output$PCMSP_s<-renderUI({
    checkboxInput("PCMSP_d","Screw Print",value=F)
  })
  
  
  output$PCMFT_s<-renderUI({
    checkboxInput("PCMFT_d","Feedthroat Temperature F",value=F)
  })
  output$PCMBZT1_s<-renderUI({
    checkboxInput("PCMBZT1_d","Barrel Zone 1 Temperature F",value=F)
  })
  output$PCMBZT2_s<-renderUI({
    checkboxInput("PCMBZT2_d","Barrel Zone 2 Temperature F",value=F)
  })
  output$PCMBZT3_s<-renderUI({
    checkboxInput("PCMBZT3_d","Barrel Zone 3 Temperature F",value=F)
  })
  
  
  output$PCMCT_s<-renderUI({
    checkboxInput("PCMCT_d","Clamp Temperature F",value=F)
  })
  output$PCMAT_s<-renderUI({
    checkboxInput("PCMAT_d","Adapter Temperature F",value=F)
  })
  output$PCMDT1_s<-renderUI({
    checkboxInput("PCMDT1_d","Die 1 Temperature F",value=F)
  })
  output$PCMDT2_s<-renderUI({
    checkboxInput("PCMDT2_d","Die 2 Temperature F",value=F)
  })
  
  output$PCMIDI_s<-renderUI({
    checkboxInput("PCMIDI_d","Inner Diameter (in)",value=TRUE)
  })
  output$PCMODI_s<-renderUI({
    checkboxInput("PCMODI_d","Outer Diameter (in)",value=TRUE)
  })
  
  
  output$PCMIWT_s<-renderUI({
    checkboxInput("PCMIWT_d","Inner Wall Thickness (in)",value=TRUE)
  })
  output$PCMMWT_s<-renderUI({
    checkboxInput("PCMMWT_d","Middle Wall Thickness (in)",value=TRUE)
  })
  output$PCMOWT_s<-renderUI({
    checkboxInput("PCMOWT_d","Outer Wall Thickness (in)",value=TRUE)
  })
  output$PCMTWT_s<-renderUI({
    checkboxInput("PCMTWT_d","Total Wall Thickness (in)",value=TRUE)
  })
  
  
  output$PCMOR_s<-renderUI({
    checkboxInput("PCMOR_d","Out of Roundness (in)",value=F)
  })
  
  
  
  output$PCMCCT_s<-renderUI({
    checkboxInput("PCMCCT_d","Concentricity",value=F)
  })
  output$PCMLength_s<-renderUI({
    checkboxInput("PCMLength_d","Length (in)",value=F)
  })
  output$PCMToLength_s<-renderUI({
    checkboxInput("PCMToLength_d","Total Length (in)",value=T)
  })
  
  output$PCMPPD_s<-renderUI({
    checkboxInput("PCMPPD_d","Perpendicularity (in)",value=F)
  })
  
  
  output$PCMNEXIV_s<-renderUI({
    checkboxInput("PCMNEXIV_d","NEXIV",value=F)
  })
  output$PCMAnnealed_s<-renderUI({
    checkboxInput("PCMAnnealed_d","Annealed",value=F)
  })
  output$PCMCaliper_s<-renderUI({
    checkboxInput("PCMCaliper_d","Caliper",value=F)
  })
  output$PCMOS_s<-renderUI({
    checkboxInput("PCMOS_d","OD Sort",value=F)
  })
  output$PCMMP_s<-renderUI({
    checkboxInput("PCMMP_d","Melt Pump",value=F)
  })
  output$PCMHT_s<-renderUI({
    checkboxInput("PCMHT_d","Hypo Tip",value=F)
  })
  output$PCMSPD_s<-renderUI({
    checkboxInput("PCMSPD_d","Sparker Die",value=F)
  })
  output$PCMSLD_s<-renderUI({
    checkboxInput("PCMSLD_d","Slicking Die",value=F)
  })
  output$PCMDLN_s<-renderUI({
    checkboxInput("PCMDLN_d","Delamination",value=F)
  })
  output$PCMULT_s<-renderUI({
    checkboxInput("PCMULT_d","Ultrasonic",value=F)
  })
  output$PCMVC_s<-renderUI({
    checkboxInput("PCMVC_d","Vacuum Calibration",value=F)
  })
  output$PCMIRD_s<-renderUI({
    checkboxInput("PCMIRD_d","Irradiated",value=F)
  })
  
  
  
  #Search Box
  
  output$PCMPN_input<-renderUI({
    selectizeInput("PCMPN",label = NULL,multiple=TRUE,
                   c("All",unique(as.character(multi_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCMPD_input<-renderUI({
    selectInput("PCMPD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Part Description`))))
  })
  output$PCMRN_input<-renderUI({
    selectInput("PCMRN",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Resin Number`))))
  })
  output$PCMRD_input<-renderUI({
    selectInput("PCMRD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Resin Description`))))
  })
  output$PCMPPSN_input<-renderUI({
    selectInput("PCMPPSN",label = NULL,
                c("All",unique(as.character(multi_pps_data$`PPS Number`))))
  })
  
  #Tooling
  output$PCMDS_min_input<-renderUI({
    numericInput("PCMDS_min",label = NULL,value=PCMDSmin,step=0.001)
  })
  output$PCMDS_max_input<-renderUI({
    numericInput("PCMDS_max",label = NULL,value=PCMDSmax,step=0.001)
  })
  output$PCMDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    selectInput("PCMDLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Die Land Length (in)`))))
  })
  output$PCMTS_min_input<-renderUI({
    numericInput("PCMTS_min",label = NULL,value=PCMTSmin,step=0.001)
  })
  output$PCMTS_max_input<-renderUI({
    numericInput("PCMTS_max",label = NULL,value=PCMTSmax,step=0.001)
  })
  output$PCMTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    selectInput("PCMTLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
  })
  output$PCMSP_input<-renderUI({
    selectInput("PCMSP",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Screw Print`))))
  })
  
  #Attributes_1
  output$PCMFT_min_input<-renderUI({
    numericInput("PCMFT_min",label = NULL,value = PCMFTmin,step=1)
  })
  output$PCMFT_max_input<-renderUI({
    numericInput("PCMFT_max",label = NULL,value=PCMFTmax,step=1)
  })
  output$PCMBZT1_min_input<-renderUI({
    numericInput("PCMBZT1_min",label = NULL,value=PCMBZT1min,step=5)
  })
  output$PCMBZT1_max_input<-renderUI({
    numericInput("PCMBZT1_max",label = NULL,value=PCMBZT1max,step=5)
  })
  output$PCMBZT2_min_input<-renderUI({
    numericInput("PCMBZT2_min",label = NULL,value=PCMBZT2min,step=5)
  })
  output$PCMBZT2_max_input<-renderUI({
    numericInput("PCMBZT2_max",label = NULL,value=PCMBZT2max,step=5)
  })
  output$PCMBZT3_min_input<-renderUI({
    numericInput("PCMBZT3_min",label = NULL,value=PCMBZT3min,step=5)
  })
  output$PCMBZT3_max_input<-renderUI({
    numericInput("PCMBZT3_max",label = NULL,value=PCMBZT3max,step=5)
  })
  
  #Temperatures
  output$PCMCT_min_input<-renderUI({
    numericInput("PCMCT_min",label = NULL,value=PCMCTmin,step=5)
  })
  output$PCMCT_max_input<-renderUI({
    numericInput("PCMCT_max",label = NULL,value=PCMCTmax,step=5)
  })
  output$PCMAT_min_input<-renderUI({
    numericInput("PCMAT_min",label = NULL,value=PCMATmin,step=5)
  })
  output$PCMAT_max_input<-renderUI({
    numericInput("PCMAT_max",label = NULL,value=PCMATmax,step=5)
  })
  output$PCMDT1_min_input<-renderUI({
    numericInput("PCMDT1_min",label = NULL,value=PCMDT1min,step=5)
  })
  output$PCMDT1_max_input<-renderUI({
    numericInput("PCMDT1_max",label = NULL,value=PCMDT1max,step=5)
  })
  output$PCMDT2_min_input<-renderUI({
    numericInput("PCMDT2_min",label = NULL,value=PCMDT2min,step=5)
  })
  output$PCMDT2_max_input<-renderUI({
    numericInput("PCMDT2_max",label = NULL,value=PCMDT2max,step=5)
  })
  #Attributes
  output$PCMIDI_min_input<-renderUI({
    numericInput("PCMIDI_min",label = NULL,value=PCMIDImin,step=0.001)
  })
  output$PCMIDI_max_input<-renderUI({
    numericInput("PCMIDI_max",label = NULL,value=PCMIDImax,step=0.001)
  })
  output$PCMODI_min_input<-renderUI({
    numericInput("PCMODI_min",label = NULL,value=PCMODImin,step=0.001)
  })
  output$PCMODI_max_input<-renderUI({
    numericInput("PCMODI_max",label = NULL,value=PCMODImax,step=0.001)
  })
  
  output$PCMIWT_min_input<-renderUI({
    numericInput("PCMIWT_min",label = NULL,value=PCMIWTmin,step=0.001)
  })
  output$PCMIWT_max_input<-renderUI({
    numericInput("PCMIWT_max",label = NULL,value=PCMIWTmax,step=0.001)
  })
  output$PCMMWT_min_input<-renderUI({
    numericInput("PCMMWT_min",label = NULL,value=PCMMWTmin,step=0.001)
  })
  output$PCMMWT_max_input<-renderUI({
    numericInput("PCMMWT_max",label = NULL,value=PCMMWTmax,step=0.001)
  })
  output$PCMOWT_min_input<-renderUI({
    numericInput("PCMOWT_min",label = NULL,value=PCMOWTmin,step=0.001)
  })
  output$PCMOWT_max_input<-renderUI({
    numericInput("PCMOWT_max",label = NULL,value=PCMOWTmax,step=0.001)
  })
  output$PCMTWT_min_input<-renderUI({
    numericInput("PCMTWT_min",label = NULL,value=PCMTWTmin,step=0.001)
  })
  output$PCMTWT_max_input<-renderUI({
    numericInput("PCMTWT_max",label = NULL,value=PCMTWTmax,step=0.001)
  })
  
  output$PCMOR_min_input<-renderUI({
    numericInput("PCMOR_min",label = NULL,value=PCMORmin,step=0.001)
  })
  output$PCMOR_max_input<-renderUI({
    numericInput("PCMOR_max",label = NULL,value=PCMORmax,step=0.001)
  })
  output$PCMCCT_min_input<-renderUI({
    numericInput("PCMCCT_min",label = NULL,value=PCMCCTmin,step=0.0001)
  })
  output$PCMCCT_max_input<-renderUI({
    numericInput("PCMCCT_max",label = NULL,value=PCMCCTmax,step=0.0001)
  })
  output$PCMLength_min_input<-renderUI({
    numericInput("PCMLength_min",label = NULL,value=PCMLengthmin,step=1)
  })
  output$PCMLength_max_input<-renderUI({
    numericInput("PCMLength_max",label = NULL,value=PCMLengthmax,step=1)
  })
  output$PCMToLength_min_input<-renderUI({
    numericInput("PCMToLength_min",label = NULL,value=PCMToLengthmin,step=1)
  })
  output$PCMToLength_max_input<-renderUI({
    numericInput("PCMToLength_max",label = NULL,value=PCMToLengthmax,step=1)
  })
  output$PCMPPD_input<-renderUI({
    selectInput("PCMPPD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Perpendicularity (in)`))))
  })
  
  #Special_1
  output$PCMNEXIV_input<-renderUI({
    selectInput("PCMNEXIV",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMAnnealed_input<-renderUI({
    selectInput("PCMAnnealed",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMCaliper_input<-renderUI({
    selectInput("PCMCaliper",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMOS_input<-renderUI({
    selectInput("PCMOS",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMMP_input<-renderUI({
    selectInput("PCMMP",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMHT_input<-renderUI({
    selectInput("PCMHT",label = NULL,choices=c("All","yes","NA"))
  })
  #Special_2
  output$PCMSPD_input<-renderUI({
    selectInput("PCMSPD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMSLD_input<-renderUI({
    selectInput("PCMSLD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMDLN_input<-renderUI({
    selectInput("PCMDLN",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMULT_input<-renderUI({
    selectInput("PCMULT",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMVC_input<-renderUI({
    selectInput("PCMVC",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMIRD_input<-renderUI({
    selectInput("PCMIRD",label = NULL,choices=c("All","yes","NA"))
  })
  

  
  #PCM
  Col_PCM=c()
  show_vars2<-reactive({
    as.numeric(c(input$PCMPN_d,input$PCMPD_d,input$PCMRN_d,input$PCMRD_d,input$PCMPPSN_d,input$PCMDS_d,input$PCMDLL_d,input$PCMTS_d,input$PCMTLL_d,input$PCMSP_d,input$PCMFT_d,
                 input$PCMBZT1_d,input$PCMBZT2_d,input$PCMBZT3_d,input$PCMCT_d,input$PCMAT_d,input$PCMDT1_d,input$PCMDT2_d,input$PCMIDI_d,input$PCMODI_d,input$PCMIWT_d,input$PCMMWT_d,
                 input$PCMOWT_d,input$PCMTWT_d,input$PCMOR_d,input$PCMCCT_d,input$PCMLength_d,input$PCMToLength_d,input$PCMPPD_d,input$PCMNEXIV_d,input$PCMAnnealed_d,input$PCMCaliper_d,
                 input$PCMOS_d,input$PCMMP_d,input$PCMHT_d,input$PCMSPD_d,input$PCMSLD_d,input$PCMDLN_d,input$PCMULT_d,input$PCMVC_d,input$PCMIRD_d))})
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({
      
      col_var2=show_vars2()
      for (i in 1:length(col_var2)){
        if (col_var2[i]!=0){
          Col_PCM=c(Col_PCM,i)
        }
      } 
  
      data_PCM<-multi_pps_data[,Col_PCM]
      
      
      
      data_PCM
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
  filter = "top")#END Multi Extrusion PPS Data
  
  
  
  #### Tapered PPS ####
  
  #Checkbox
  output$PCTPN_s<-renderUI({
    checkboxInput("PCTPN_d","Part Number",value=TRUE)
  })
  output$PCTPD_s<-renderUI({
    checkboxInput("PCTPD_d","Part Description",value=TRUE)
  })
  output$PCTRN_s<-renderUI({
    checkboxInput("PCTRN_d","Resin Number",value=TRUE)
  })
  output$PCTRD_s<-renderUI({
    checkboxInput("PCTRD_d","Resin Description",value=TRUE)
  })
  output$PCTPPSN_s<-renderUI({
    checkboxInput("PCTPPSN_d","PPS Number",value=F)
  })
  #Tooling
  output$PCTDS_s<-renderUI({
    checkboxInput("PCTDS_d","Die Size (in)",value=F)
  })
  output$PCTDLL_s<-renderUI({
    checkboxInput("PCTDLL_d","Die Land Length (in)",value=F)
  })
  output$PCTTS_s<-renderUI({
    checkboxInput("PCTTS_d","Tip Size (in)",value=F)
  })
  output$PCTTLL_s<-renderUI({
    checkboxInput("PCTTLL_d","Tip Land Length (in)",value=F)
  })
  output$PCTSP_s<-renderUI({
    checkboxInput("PCTSP_d","Screw Print",value=F)
  })
  
  
  output$PCTFT_s<-renderUI({
    checkboxInput("PCTFT_d","Feedthroat Temperature F",value=F)
  })
  output$PCTBZT1_s<-renderUI({
    checkboxInput("PCTBZT1_d","Barrel Zone 1 Temperature F",value=F)
  })
  output$PCTBZT2_s<-renderUI({
    checkboxInput("PCTBZT2_d","Barrel Zone 2 Temperature F",value=F)
  })
  output$PCTBZT3_s<-renderUI({
    checkboxInput("PCTBZT3_d","Barrel Zone 3 Temperature F",value=F)
  })
  
  
  output$PCTCT_s<-renderUI({
    checkboxInput("PCTCT_d","Clamp Temperature F",value=F)
  })
  output$PCTAT_s<-renderUI({
    checkboxInput("PCTAT_d","Adapter Temperature F",value=F)
  })
  output$PCTDT1_s<-renderUI({
    checkboxInput("PCTDT1_d","Die 1 Temperature F",value=F)
  })
  output$PCTDT2_s<-renderUI({
    checkboxInput("PCTDT2_d","Die 2 Temperature F",value=F)
  })
  
  output$PCTPIDI_s<-renderUI({
    checkboxInput("PCTPIDI_d","Proximal Inner Diameter (in)",value=TRUE)
  })
  output$PCTPODI_s<-renderUI({
    checkboxInput("PCTPODI_d","Proximal Outer Diameter (in)",value=TRUE)
  })
  output$PCTPWT_s<-renderUI({
    checkboxInput("PCTPWT_d","Proximal Wall Thickness (in)",value=TRUE)
  })
  output$PCTPOR_s<-renderUI({
    checkboxInput("PCTPOR_d","Proximal Out of Roundness (in)",value=F)
  })
  output$PCTPCCT_s<-renderUI({
    checkboxInput("PCTPCCT_d","Proximal Concentricity",value=F)
  })
  
  
  output$PCTDIDI_s<-renderUI({
    checkboxInput("PCTDIDI_d","Distal Inner Diameter (in)",value=TRUE)
  })
  output$PCTDODI_s<-renderUI({
    checkboxInput("PCTDODI_d","Distal Outer Diameter (in)",value=TRUE)
  })
  output$PCTDWT_s<-renderUI({
    checkboxInput("PCTDWT_d","Distal Wall Thickness (in)",value=TRUE)
  })
  output$PCTDOR_s<-renderUI({
    checkboxInput("PCTDOR_d","Distal Out of Roundness (in)",value=F)
  })
  output$PCTDCCT_s<-renderUI({
    checkboxInput("PCTDCCT_d","Distal Concentricity",value=F)
  })
  
  
  output$PCTPLength_s<-renderUI({
    checkboxInput("PCTPLength_d","Proximal Length (in)",value=F)
  })
  output$PCTTLength_s<-renderUI({
    checkboxInput("PCTTLength_d","Transition Length (in)",value=F)
  })
  output$PCTDLength_s<-renderUI({
    checkboxInput("PCTDLength_d","Distal Length (in)",value=F)
  })
  output$PCTToLength_s<-renderUI({
    checkboxInput("PCTToLength_d","Total Length (in)",value=F)
  })
  output$PCTPPD_s<-renderUI({
    checkboxInput("PCTPPD_d","Perpendicularity (in)",value=F)
  })
  
  
  output$PCTNEXIV_s<-renderUI({
    checkboxInput("PCTNEXIV_d","NEXIV",value=F)
  })
  output$PCTAnnealed_s<-renderUI({
    checkboxInput("PCTAnnealed_d","Annealed",value=F)
  })
  output$PCTCaliper_s<-renderUI({
    checkboxInput("PCTCaliper_d","Caliper",value=F)
  })
  output$PCTOS_s<-renderUI({
    checkboxInput("PCTOS_d","OD Sort",value=F)
  })
  output$PCTMP_s<-renderUI({
    checkboxInput("PCTMP_d","Melt Pump",value=F)
  })
  output$PCTHT_s<-renderUI({
    checkboxInput("PCTHT_d","Hypo Tip",value=F)
  })
  output$PCTSPD_s<-renderUI({
    checkboxInput("PCTSPD_d","Sparker Die",value=F)
  })
  output$PCTSLD_s<-renderUI({
    checkboxInput("PCTSLD_d","Slicking Die",value=F)
  })
  output$PCTDLN_s<-renderUI({
    checkboxInput("PCTDLN_d","Delamination",value=F)
  })
  output$PCTULT_s<-renderUI({
    checkboxInput("PCTULT_d","Ultrasonic",value=F)
  })
  output$PCTVC_s<-renderUI({
    checkboxInput("PCTVC_d","Vacuum Calibration",value=F)
  })
  output$PCTIRD_s<-renderUI({
    checkboxInput("PCTIRD_d","Irradiated",value=F)
  })
  
  
  
  #Search Box
  
  #Part Resin
  output$PCTPN_input<-renderUI({
    selectizeInput("PCTPN",label = NULL,multiple=TRUE,
                   c("All",unique(as.character(single_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCTPD_input<-renderUI({
    selectInput("PCTPD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Part Description`))))
  })
  output$PCTRN_input<-renderUI({
    selectInput("PCTRN",label = NULL,
                c("All",unique(as.character(single_pps_data$`Resin Number`))))
  })
  output$PCTRD_input<-renderUI({
    selectInput("PCTRD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Resin Description`))))
  })
  output$PCTPPSN_input<-renderUI({
    selectInput("PCTPPSN",label = NULL,
                c("All",unique(as.character(single_pps_data$`PPS Number`))))
  })
  
  #Tooling
  output$PCTDS_min_input<-renderUI({
    numericInput("PCTDS_min",label = NULL,value=PCTDSmin,step=0.001)
  })
  output$PCTDS_max_input<-renderUI({
    numericInput("PCTDS_max",label = NULL,value=PCTDSmax,step=0.001)
  })
  output$PCTDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    selectInput("PCTDLL",label = NULL,c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
  })
  output$PCTTS_min_input<-renderUI({
    numericInput("PCTTS_min",label = NULL,value=PCTTSmin,step=0.001)
  })
  output$PCTTS_max_input<-renderUI({
    numericInput("PCTTS_max",label = NULL,value=PCTTSmax,step=0.001)
  })
  output$PCTTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    selectInput("PCTTLL",label = NULL,c("All",unique(as.character(single_pps_data$`Tip Land Length (in)`))))
  })
  output$PCTSP_input<-renderUI({
    selectInput("PCTSP",label = NULL,
                c("All",unique(as.character(single_pps_data$`Screw Print`))))
  })
  #Attributes_1
  output$PCTFT_min_input<-renderUI({
    numericInput("PCTFT_min",label = NULL,value = PCTFTmin,step=1)
  })
  output$PCTFT_max_input<-renderUI({
    numericInput("PCTFT_max",label = NULL,value=PCTFTmax,step=1)
  })
  output$PCTBZT1_min_input<-renderUI({
    numericInput("PCTBZT1_min",label = NULL,value=PCTBZT1min,step=5)
  })
  output$PCTBZT1_max_input<-renderUI({
    numericInput("PCTBZT1_max",label = NULL,value=PCTBZT1max,step=5)
  })
  output$PCTBZT2_min_input<-renderUI({
    numericInput("PCTBZT2_min",label = NULL,value=PCTBZT2min,step=5)
  })
  output$PCTBZT2_max_input<-renderUI({
    numericInput("PCTBZT2_max",label = NULL,value=PCTBZT2max,step=5)
  })
  output$PCTBZT3_min_input<-renderUI({
    numericInput("PCTBZT3_min",label = NULL,value=PCTBZT3min,step=5)
  })
  output$PCTBZT3_max_input<-renderUI({
    numericInput("PCTBZT3_max",label = NULL,value=PCTBZT3max,step=5)
  })
  
  #Attrobites_2
  output$PCTCT_min_input<-renderUI({
    numericInput("PCTCT_min",label = NULL,value=PCTCTmin,step=5)
  })
  output$PCTCT_max_input<-renderUI({
    numericInput("PCTCT_max",label = NULL,value=PCTCTmax,step=5)
  })
  output$PCTAT_min_input<-renderUI({
    numericInput("PCTAT_min",label = NULL,value=PCTATmin,step=5)
  })
  output$PCTAT_max_input<-renderUI({
    numericInput("PCTAT_max",label = NULL,value=PCTATmax,step=5)
  })
  output$PCTDT1_min_input<-renderUI({
    numericInput("PCTDT1_min",label = NULL,value=PCTDT1min,step=5)
  })
  output$PCTDT1_max_input<-renderUI({
    numericInput("PCTDT1_max",label = NULL,value=PCTDT1max,step=5)
  })
  output$PCTDT2_min_input<-renderUI({
    numericInput("PCTDT2_min",label = NULL,value=PCTDT2min,step=5)
  })
  output$PCTDT2_max_input<-renderUI({
    numericInput("PCTDT2_max",label = NULL,value=PCTDT2max,step=5)
  })
  
  
  
  
  
  #Temps
  output$PCTPIDI_min_input<-renderUI({
    numericInput("PCTPIDI_min",label = NULL,value=PCTPIDImin,step=0.001)
  })
  output$PCTPIDI_max_input<-renderUI({
    numericInput("PCTPIDI_max",label = NULL,value=PCTPIDImax,step=0.001)
  })
  output$PCTPODI_min_input<-renderUI({
    numericInput("PCTPODI_min",label = NULL,value=PCTPODImin,step=0.001)
  })
  output$PCTPODI_max_input<-renderUI({
    numericInput("PCTPODI_max",label = NULL,value=PCTPODImax,step=0.001)
  })
  output$PCTPWT_min_input<-renderUI({
    numericInput("PCTPWT_min",label = NULL,value=PCTPWTmin,step=0.001)
  })
  output$PCTPWT_max_input<-renderUI({
    numericInput("PCTPWT_max",label = NULL,value=PCTPWTmax,step=0.001)
  })
  output$PCTPOR_min_input<-renderUI({
    numericInput("PCTPOR_min",label = NULL,value=PCTPORmin,step=0.001)
  })
  output$PCTPOR_max_input<-renderUI({
    numericInput("PCTPOR_max",label = NULL,value=PCTPORmax,step=0.001)
  })
  output$PCTPCCT_min_input<-renderUI({
    numericInput("PCTPCCT_min",label = NULL,value=PCTPCCTmin,step=0.0001)
  })
  output$PCTPCCT_max_input<-renderUI({
    numericInput("PCTPCCT_max",label = NULL,value=PCTPCCTmax,step=0.0001)
  })
  
  output$PCTDIDI_min_input<-renderUI({
    numericInput("PCTDIDI_min",label = NULL,value=PCTDIDImin,step=0.001)
  })
  output$PCTDIDI_max_input<-renderUI({
    numericInput("PCTDIDI_max",label = NULL,value=PCTDIDImax,step=0.001)
  })
  output$PCTDODI_min_input<-renderUI({
    numericInput("PCTDODI_min",label = NULL,value=PCTDODImin,step=0.001)
  })
  output$PCTDODI_max_input<-renderUI({
    numericInput("PCTDODI_max",label = NULL,value=PCTDODImax,step=0.001)
  })
  output$PCTDWT_min_input<-renderUI({
    numericInput("PCTDWT_min",label = NULL,value=PCTDWTmin,step=0.001)
  })
  output$PCTDWT_max_input<-renderUI({
    numericInput("PCTDWT_max",label = NULL,value=PCTDWTmax,step=0.001)
  })
  output$PCTDOR_min_input<-renderUI({
    numericInput("PCTDOR_min",label = NULL,value=PCTDORmin,step=0.001)
  })
  output$PCTDOR_max_input<-renderUI({
    numericInput("PCTDOR_max",label = NULL,value=PCTDORmax,step=0.001)
  })
  output$PCTDCCT_min_input<-renderUI({
    numericInput("PCTDCCT_min",label = NULL,value=PCTDCCTmin,step=0.0001)
  })
  output$PCTDCCT_max_input<-renderUI({
    numericInput("PCTDCCT_max",label = NULL,value=PCTDCCTmax,step=0.0001)
  })
  
  
  output$PCTPLength_min_input<-renderUI({
    numericInput("PCTPLength_min",label = NULL,value=PCTPLengthmin,step=1)
  })
  output$PCTPLength_max_input<-renderUI({
    numericInput("PCTPLength_max",label = NULL,value=PCTPLengthmax,step=1)
  })
  output$PCTTLength_min_input<-renderUI({
    numericInput("PCTTLength_min",label = NULL,value=PCTTLengthmin,step=1)
  })
  output$PCTTLength_max_input<-renderUI({
    numericInput("PCTTLength_max",label = NULL,value=PCTTLengthmax,step=1)
  })
  output$PCTDLength_min_input<-renderUI({
    numericInput("PCTDLength_min",label = NULL,value=PCTDLengthmin,step=1)
  })
  output$PCTLength_max_input<-renderUI({
    numericInput("PCTDLength_max",label = NULL,value=PCTDLengthmax,step=1)
  })
  output$PCTToLength_min_input<-renderUI({
    numericInput("PCTToLength_min",label = NULL,value=PCTToLengthmin,step=1)
  })
  output$PCTToLength_max_input<-renderUI({
    numericInput("PCTToLength_max",label = NULL,value=PCTToLengthmax,step=1)
  })
  output$PCTPPD_input<-renderUI({
    selectInput("PCTPPD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
  })
  
  #Special_1
  output$PCTNEXIV_input<-renderUI({
    selectInput("PCTNEXIV",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTAnnealed_input<-renderUI({
    selectInput("PCTAnnealed",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTCaliper_input<-renderUI({
    selectInput("PCTCaliper",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTOS_input<-renderUI({
    selectInput("PCTOS",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTMP_input<-renderUI({
    selectInput("PCTMP",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTHT_input<-renderUI({
    selectInput("PCTHT",label = NULL,choices=c("All","yes","NA"))
  })
  #Special_2
  output$PCTSPD_input<-renderUI({
    selectInput("PCTSPD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTSLD_input<-renderUI({
    selectInput("PCTSLD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTDLN_input<-renderUI({
    selectInput("PCTDLN",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTULT_input<-renderUI({
    selectInput("PCTULT",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTVC_input<-renderUI({
    selectInput("PCTVC",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTIRD_input<-renderUI({
    selectInput("PCTIRD",label = NULL,choices=c("All","yes","NA"))
  })
  
  Col_PCT=c()
  show_vars3<-reactive({
    as.numeric(c(input$PCTPN_d,input$PCTPD_d,input$PCTRN_d,input$PCTRD_d,input$PCTPPSN_d,input$PCTDS_d,input$PCTDLL_d,input$PCTTS_d,input$PCTTLL_d,input$PCTSP_d,input$PCTFT_d,
                 input$PCTBZT1_d,input$PCTBZT2_d,input$PCTBZT3_d,input$PCTCT_d,input$PCTAT_d,input$PCTDT1_d,input$PCTDT2_d,
                 input$PCTPIDI_d,input$PCTPODI_d,input$PCTPWT_d,input$PCTPOR_d,input$PCTPCCT_d,
                 input$PCTDIDI_d,input$PCTDODI_d,input$PCTDWT_d,input$PCTDOR_d,input$PCTDCCT_d,
                 input$PCTPLength_d,input$PCTTLength_d,input$PCTDLength_d,input$PCTToLength_d,input$PCTPPD_d,
                 input$PCTNEXIV_d,input$PCTAnnealed_d,input$PCTCaliper_d,input$PCTOS_d,input$PCTMP_d,input$PCTHT_d,
                 input$PCTSPD_d,input$PCTSLD_d,input$PCTDLN_d,input$PCTULT_d,input$PCTVC_d,input$PCTIRD_d))})
  
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable({
      col_var3=show_vars3()
      for (i in 1:length(col_var3)){
        if (col_var3[i]!=0){
          Col_PCT=c(Col_PCT,i)
        }
      } 
      data_PCT<-tapered_pps_data[,Col_PCT]
      
      
      data_PCT
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
  filter = "top") #end Tapered Extrusion PPS Data
  
  
  #### EXTRA ####
  

  output$MESparameters <- renderDataTable({
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    data <- tari_parameter_data[tari_parameter_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$MEStime <- renderDataTable({
    #This returns the table of the MES input times based on the SAP batch numbers in the
    #shopping cart
    data <- tari_time_data[tari_time_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$MESsubmitter <- renderDataTable({
    #This returns the table of the MES submitter IDs based on the SAP batch numbers in the
    #shopping cart
    data <- tari_submitter_data[tari_submitter_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$MEStotal <- renderDataTable({
    #This returns the table of all MES inputs based on the SAP batch numbers in the
    #shopping cart
    data <- tari_total_data[tari_total_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$scrapcodes <- renderDataTable({
    #This returns the table of SAP scrap codes based on the SAP batch numbers in the
    #shopping cart
    data <- scrapcodes_data[scrapcodes_data$Order %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  #Testing the appstats data
  output$nexiv <- renderDataTable({
    #This returns the table of the Applied Stats Nexiv Data based on the SAP batch numbers in the
    #shopping cart
    data <- nexiv[nexiv$`Batch #` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$laserlinc <- renderDataTable({
    #This returns the table of the Applied Stats laserlinc data based on the SAP batch numbers in the
    #shopping cart
    data <- ll[ll$`Lot Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  # end Single Extrusion PPS Data Server part and Shopping cart
  
  
  shoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE))
  
  observeEvent(input$add_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$add_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"delete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- tari_parameter_data$`SAP Batch Number`[tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"delete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    shoppingcart$data <- rbind(shoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(shoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  }
  )
  
  
  observeEvent(input$delete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$delete_part_button, "_")[[1]][2]
    shoppingcart$data <- shoppingcart$data[shoppingcart$data$'Part' != part,]
  })
  
  observeEvent(input$delete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$delete_batch_button, "_")[[1]][2]
    shoppingcart$data <- shoppingcart$data[shoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$shoppingcart <- renderDataTable(
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    shoppingcart$data,
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE) #for the shoppingcart
  

}





# Run the application 
shinyApp(ui = ui, server = server)
