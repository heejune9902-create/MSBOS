source("scr/libraries.R")

# 환자정보 불러오기 ---------------------------------------------------------------

file_list_pt <- list.files("data/raw", pattern = "MSBOS_multi-center-pt")

pt_raw <- data.table()
for (pt_file in file_list_pt){
  file_path <- file.path("data/raw", pt_file)
  pt_data <- fread(file_path, encoding = "UTF-8", na.strings = "")
  pt_raw <- rbind(pt_raw, pt_data)
}
pt_raw




# 검사처방 불러오기 ---------------------------------------------------------------

file_list <- list.files("data/raw", pattern = "MSBOS_multi-center-검사처방")

lab_raw <- data.table()
for (file in file_list){
  file_path <- file.path("data/raw", file)
  current_data <- fread(file_path, encoding = "UTF-8", na.strings = "")
  lab_raw <- rbind(lab_raw, current_data)
}
lab_raw




# 상병 불러오기 -----------------------------------------------------------------

file_list <- list.files("data/raw", pattern = "MSBOS_multi-center-상병")
file_path <- file.path("data/raw", file_list)

dx_raw_list <- list()
for (i in 1:length(file_path)){
  print(i)
  dx_raw_list[[i]] <- fread(file_path[i], encoding = "UTF-8", na.strings = "")
}
dx_raw_list
dx_raw <- do.call(rbind, c(dx_raw_list, list(use.names = FALSE)))



# msbos_acting 불러오기 -------------------------------------------------------

med_raw <- fread("data/raw/msbos_acting_UTF_8.csv", encoding = "UTF-8", na.strings = "")


# msbos_record ------------------------------------------------------------

surgery_raw <- fread("data/raw/msbos_record_UTF_8.csv", encoding = "UTF-8", na.strings = "")


# 수혈 불러오기-----------------------------------------------------------------


transfusion_order_raw <- fread("data/raw/msbos transfusion_raw.csv", encoding = "UTF-8", na.strings = "")



# ICD9cm ------------------------------------------------------------------

icd9cm <- readxl::read_xlsx("data/new/join_table_icd9cm.xlsx")


# 메모리
rm(list = c("dx_raw_list"))
gc()
