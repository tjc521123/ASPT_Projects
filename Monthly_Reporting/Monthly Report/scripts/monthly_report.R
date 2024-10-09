library(readxl)
library(tidyverse)
library(vtable)
library(data.table)
library(quarto)
library(reportfactory)
library(fs)

# Get month and year to pull data for
year_test  <- readline(prompt = 'Enter the test year: ')
year_test  <- as.numeric(year_test)
month_test <- readline(prompt = 'Enter the test month: ')
month_test <- as.numeric(month_test)

# Create file paths
raw_path   = './data/raw/'
clean_path = './data/clean/'

# Import testing data
test_url           <- 'C:/Users/Caitie Mayo/OneDrive - Architech Sports and Physical Therapy/Athlete Workout Cards/Athlete Workout Cards - Ballantyne/Data/2024 Testing Data - Ballantyne.xlsx'
test_sheet         <- 'MASTER DATA TABLE'

# Create directories
test_raw_data_path <- paste(raw_path, year_test, '_', month_test, sep = '')
output_folder      <- paste('./Reports/', Sys.Date(), sep = '')
if(!dir.exists(test_raw_data_path)) {
  dir.create(path = test_raw_data_path)
}
if(!dir.exists(output_folder)) {
  dir.create(path = output_folder)
}
raw_data <- read_excel(test_url, sheet = test_sheet)
write.csv(raw_data, file = paste(test_raw_data_path, '/raw_testing_', year_test, '_', month_test, sep = ''))

# Perform initial cleaning
# Creates age dummy variables, groups dates by month, consolidates Nordbord to 
# average of Left and Right, and inverts time-based data so the max function
# will correctly find best scores
data <- raw_data %>%
  mutate(YOUNG       = ifelse(raw_data$AGE <= 13, 1, 0),
         OLD         = ifelse(raw_data$AGE> 13 & raw_data$AGE <= 18, 1, 0),
         ADULT       = ifelse(raw_data$AGE > 18, 1, 0),
         DATE_MONTH  = month(raw_data$DATE),
         NORDBORD    = rowMeans(cbind(raw_data$NORDBORD_L, raw_data$NORDBORD_R), na.rm = TRUE),
         PRO_AGILITY = PRO_AGILITY * -1,
         SPLIT_10    = SPLIT_10 * -1,
         SPRINT_20   = SPRINT_20 * -1,
         SHUTTLE_300 = SHUTTLE_300 * -1)


# Get last month's and this month's testing data
data_last    <- data[month(data$DATE) == month_test - 1,]
data_current <- data[month(data$DATE) == month_test,]

# Get athletes who tested last month
athletes <- unique(data_current$NAME)

# Create function list for summary table
func_list <- list(
  Current = ~last(.x, na_rm = TRUE),
  Best = ~max(.x, na.rm = TRUE),
  Best_change = ~(last(.x, na_rm = TRUE) - max(.x, na.rm = TRUE)),
  Best_perc = ~((last(.x, na_rm = TRUE) - max(.x, na.rm = TRUE))/max(.x, na.rm = TRUE)),
  Last = ~nth(.x, -2, na_rm = TRUE),
  Last_change = ~(last(.x, na_rm = TRUE) - nth(.x, -2, na_rm = TRUE)),
  Last_perc = ~((last(.x, na_rm = TRUE) - nth(.x, -2, na_rm = TRUE))/nth(.x, -2, na_rm = TRUE))
)

# Create variable list to exclude for summary table
var_list <- c(
  'NAME',
  'DATE',
  'DATE_MONTH',
  'AGE',
  'CMJ_FLIGHT',
  'SHUTTLE_300',
  'FLEXED_HANG',
  'PULL-UPS',
  'YO-YO',
  'YOUNG',
  'OLD',
  'ADULT',
  'DSI',
  'NORDBORD_DIFF',
  'NORDBORD_L',
  'NORDBORD_R'
)

# Create summary table
summary_data <- data %>%
  group_by(NAME) %>%
  summarise_at(
    .vars = names(data)[!names(data) %in% var_list],
    .funs = func_list
  ) %>% 
  filter(NAME %in% athletes)

# Inverts time-based variables back to positive values
find_min <- c('PRO_AGILITY', 'SPLIT_10', 'SPRINT_20', 'SHUTTLE_300')
for (name in names(summary_data)) {
  for (test in find_min) {
    if(length(grep(test, name)) != 0) {
      summary_data[[name]] <- -1 * summary_data[[name]]
    }
  }
}

for (name in names(data)) {
  if(name %in% find_min) {
    data[[name]] <- -1 * data[[name]]
  }
}

# Replaces Inf values from min/max functions to NA
summary_data[summary_data == -Inf] <- NA
summary_data[summary_data == Inf] <- NA

# Writes data to testing_data.csv
write.csv(data,
          file = paste(clean_path, year_test, '_', month_test, '_testing_data.csv', sep = ''),
          row.names = FALSE)

# Writes summary_data to summary_data.csv
write.csv(summary_data, 
          file = paste(clean_path, year_test, '_', month_test, '_summary_data.csv', sep = ''), 
          row.names = FALSE)

# Renders the Quarto report
wd <- getwd()
setwd('./report_sources/')
output_file_name <- paste(year_test, '_', month_test, '_Monthly Report.html', sep = "")
quarto_render('Monthly_Report_Quarto.qmd',
              output_format = 'html',
              output_file = output_file_name,
              execute_params = list(month = month_test, year = year_test))

# Performs cleanup
setwd(wd)
file_move(path = paste('./report_sources/', output_file_name, sep = ''), 
          new_path = paste(output_folder, '/', output_file_name, sep = ''))
# file_delete(path = './report_sources/Monthly_Report_Quarto_files')
