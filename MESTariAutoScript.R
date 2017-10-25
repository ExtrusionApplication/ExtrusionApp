#load the master files
#write master files to a backup location with a date stamp
#analyze the auto download files
#concatenate to the master files
#write the master files to the UI location
#delete files after eight weeks. Keep one file from that period.
#close out the script


#### Loading the Current Master Files ####
ui_folder <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data"

single_parameters <- read.csv(paste0(ui_folder, "/", "Single Tari Parameters.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_parameters <- read.csv(paste0(ui_folder, "/", "Multi Tari Parameters.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_parameters <- read.csv(paste0(ui_folder, "/", "Tapered Tari Parameters.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_submitter <- read.csv(paste0(ui_folder, "/", "Single Tari Submitter.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_submitter <- read.csv(paste0(ui_folder, "/", "Multi Tari Submitter.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_submitter <- read.csv(paste0(ui_folder, "/", "Tapered Tari Submitter.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_time <- read.csv(paste0(ui_folder, "/", "Single Tari Time.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_time <- read.csv(paste0(ui_folder, "/", "Multi Tari Time.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_time <- read.csv(paste0(ui_folder, "/", "Tapered Tari Time.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_total <- read.csv(paste0(ui_folder, "/", "Single Tari Total.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_total <- read.csv(paste0(ui_folder, "/", "Multi Tari Total.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_total <- read.csv(paste0(ui_folder, "/", "Tapered Tari Total.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)


#### Re-writting the Master Files to a Backup ####
backup_folder <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Test/Backup UI Data"

current_date <- Sys.Date() #gets current date

backup_single_parameters_name <- paste0("Backup Single Tari Parameters ", current_date, ".csv")
backup_multi_parameters_name <- paste0("Backup Multi Tari Parameters ", current_date, ".csv")
backup_tapered_parameters_name <- paste0("Backup Tapered Tari Parameters ", current_date, ".csv")

backup_single_submitter_name <- paste0("Backup Single Tari Submitter ", current_date, ".csv")
backup_multi_submitter_name <- paste0("Backup Multi Tari Submitter ", current_date, ".csv")
backup_tapered_submitter_name <- paste0("Backup Tapered Tari Submitter ", current_date, ".csv")

backup_single_time_name <- paste0("Backup Single Tari Time ", current_date, ".csv")
backup_multi_time_name <- paste0("Backup Multi Tari Time ", current_date, ".csv")
backup_tapered_time_name <- paste0("Backup Tapered Tari Time ", current_date, ".csv")

backup_single_total_name <- paste0("Backup Single Tari Total ", current_date, ".csv")
backup_multi_total_name <- paste0("Backup Multi Tari Total ", current_date, ".csv")
backup_tapered_total_name <- paste0("Backup Tapered Tari Total ", current_date, ".csv")


#writing the files

fwrite(single_parameters, paste0(backup_folder, "/", backup_single_parameters_name))
fwrite(multi_parameters, paste0(backup_folder, "/", backup_multi_parameters_name))
fwrite(tapered_parameters, paste0(backup_folder, "/", backup_tapered_parameters_name))

fwrite(single_submitter, paste0(backup_folder, "/", backup_single_submitter_name))
fwrite(multi_submitter, paste0(backup_folder, "/", backup_multi_submitter_name))
fwrite(tapered_submitter, paste0(backup_folder, "/", backup_tapered_submitter_name))

fwrite(single_time, paste0(backup_folder, "/", backup_single_time_name))
fwrite(multi_time, paste0(backup_folder, "/", backup_multi_time_name))
fwrite(tapered_time, paste0(backup_folder, "/", backup_tapered_time_name))

fwrite(single_total, paste0(backup_folder, "/", backup_single_total_name))
fwrite(multi_total, paste0(backup_folder, "/", backup_multi_total_name))
fwrite(tapered_total, paste0(backup_folder, "/", backup_tapered_total_name))



#### Analyzing the Auto Download Files ####
auto_download_data <- read.csv("//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/AutoDownload/MES/CTP - Data Points - Previous Day.csv",
                               header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
colnames(auto_download_data) <- c("Container", "Container Status", "Material", "Material Desc",
                                  "Material Rev", "Model", "SAP Batch", "Current Qty", "Production Order",
                                  "Production Order Type", "SWR", "Data Point  Value", "High Limit",
                                  "Low Limit", "Result", "Parent Container", "Final Confirmed Quantity",
                                  "Data Point Desc", "Sequence", "Duration Delta", "Data Point Name",
                                  "Original Qty", "Task List", "Task List Description",
                                  "Task List Rev", "Task Item", "Work Cell", "Serial #",
                                  "Product Family", "Submitter", "Data Collection Date & Time")







