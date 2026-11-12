source(file.path(if (file.exists("scripts/00_setup.R")) "." else "..", "scripts", "00_setup.R"), encoding = "UTF-8")

# 환자 정보

patient_files <- list.files(RAW_DIR, pattern = "MSBOS_multi-center-pt")

patient_raw_df <- data.table()
for (pt_file in patient_files){
  file_path <- file.path(RAW_DIR, pt_file)
  patient_part_df <- fread(file_path, encoding = "UTF-8", na.strings = "")
  patient_raw_df <- rbind(patient_raw_df, patient_part_df)
}
patient_raw_df

# 검사 결과

laboratory_files <- list.files(RAW_DIR, pattern = "MSBOS_multi-center-검사처방")

laboratory_raw_df <- data.table()
for (file in laboratory_files){
  laboratory_source_path <- file.path(RAW_DIR, file)
  laboratory_part_df <- fread(laboratory_source_path, encoding = "UTF-8", na.strings = "")
  laboratory_raw_df <- rbind(laboratory_raw_df, laboratory_part_df)
}
laboratory_raw_df

# 진단 기록

diagnosis_files <- list.files(RAW_DIR, pattern = "MSBOS_multi-center-상병")
diagnosis_source_paths <- file.path(RAW_DIR, diagnosis_files)

diagnosis_parts <- list()
for (i in 1:length(diagnosis_source_paths)){
  diagnosis_parts[[i]] <- fread(diagnosis_source_paths[i], encoding = "UTF-8", na.strings = "")
}
diagnosis_parts
diagnosis_raw_df <- do.call(rbind, c(diagnosis_parts, list(use.names = FALSE)))

# 약물 투여 기록

medication_raw_df <- fread(raw_file("msbos_acting_UTF_8.csv"), encoding = "UTF-8", na.strings = "")

# 수술 기록

surgery_raw_df <- fread(raw_file("msbos_record_UTF_8.csv"), encoding = "UTF-8", na.strings = "")

# 수혈 기록

transfusion_raw_df <- fread(raw_file("msbos transfusion_raw.csv"), encoding = "UTF-8", na.strings = "")

# 수술코드 사전

procedure_dictionary_df <- readxl::read_xlsx(processed_file("join_table_icd9cm.xlsx"))

rm(list = c("diagnosis_parts"))
gc()
