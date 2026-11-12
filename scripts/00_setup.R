# 공통 경로와 패키지
.source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
if (length(.source_files) == 0L) stop("source()로 설정 파일을 실행하세요.")
PROJECT_ROOT <- dirname(dirname(normalizePath(tail(.source_files, 1L)[[1L]], winslash = "/")))
DATA_ROOT <- normalizePath(Sys.getenv("MSBOS_DATA_ROOT", unset = file.path(PROJECT_ROOT, "data")), winslash = "/", mustWork = FALSE)
RAW_DIR <- file.path(DATA_ROOT, "raw")
PROCESSED_DIR <- file.path(DATA_ROOT, "processed")
OUTPUT_DIR <- normalizePath(Sys.getenv("MSBOS_OUTPUT_ROOT", unset = file.path(PROJECT_ROOT, "outputs")), winslash = "/", mustWork = FALSE)

raw_file <- function(...) file.path(RAW_DIR, ...)
processed_file <- function(...) file.path(PROCESSED_DIR, ...)
output_file <- function(...) file.path(OUTPUT_DIR, ...)

required_packages <- c("data.table", "dplyr", "tidyr", "stringr", "lubridate", "readxl", "moonBook", "comorbidity", "skimr", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("패키지를 설치하세요: ", paste(missing_packages, collapse = ", "))
invisible(lapply(required_packages, function(package) suppressPackageStartupMessages(library(package, character.only = TRUE))))
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
rm(.source_files)
