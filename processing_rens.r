#required packages

require("readr")
require("data.table")

#setting main directory
#Directory path needs to be changes : "C:/Users/____/Documents/GitHub/2023-24a-fai2-adsai-group-team-tourism"
setwd("/Users/rens/Github/Y2BA-Retake--Rens---Stijn-")
#Adding folders

#dir.create("Data")
#dir.create("Output")

#Creating base, data and output directory

base_directory <- "/Users/rens/Github/Y2BA-Retake--Rens---Stijn-"
data_directory <- file.path(base_directory, "Data")
output_directory <- file.path(base_directory, "Data")

#Checking direcory
current_directory <- getwd()
print(current_directory)
cat("This is your current directory:", current_directory, "\n")

#loading the data

text_data <- read.csv(file.path(data_directory, "df_text_2023-10-31.csv"))



#Inspecting collumn names and typs

str(text_data)
summary(text_data)

#EDA & cleaning data

filtered_data <- text_data[apply(text_data, 1, function(row) any(grepl("Tourism", row))), ]
filtered_data <- filtered_data[!apply(filtered_data, 1, function(row) any(grepl("Built Environment", row))), ]

# Define the column numbers to drop
columns_to_drop <- c(74, 75:382, 76, 395:402, 97:104)

# Remove the specified columns by position
filtered_data <- filtered_data[, -columns_to_drop]

#Removing the unnecessary collumns

start <-87
end <- 93
columns_to_drop <- start:end
filtered_data <- filtered_data[, -columns_to_drop]


columns_to_drop <- 74
filtered_data <- filtered_data[, -columns_to_drop]

columns_to_drop <- 41
filtered_data <- filtered_data[, -columns_to_drop]

columns_to_drop <- 83
filtered_data <- filtered_data[, -columns_to_drop]
col_names <- colnames(filtered_data)

# View the updated data frame
View(filtered_data)

#saving the fitered data as filtered_data_text_new.csv
write.csv(filtered_data, file.path(data_directory, "filtered_data_text_new.csv"), row.names = FALSE)