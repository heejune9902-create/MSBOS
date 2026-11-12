# source("scr/1. load_data.R")

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}


# Preprocessing -------------------------------------------------------------------------------------

## pt ---------------------------------------------------------------------------------------------------

col_pt <- colnames(pt_raw)
colnames(pt_raw) <- sub("\\#.*", "", col_pt)

pt0 <- pt_raw %>%
  rename(pt_id = "환자번호",
         birth_date = "생년월일",
         sex = "성별") %>%
  mutate(birth_date = paste(birth_date, "-1", sep = "") %>% ymd)


## Surgery-----------------------------------------------------------------------------------------------

surgery0 <- surgery_raw %>%
         rename(pt_id = "환자번호.1", 
         op_date = "수술일자.최초1...2", 
         visit_date = "진료일시.5", 
         visit_cat_cd = "내원구분코드.6", 
         visit_cat_nm = "내원구분명.7", 
         dx_cd = "상병코드.8", 
         dx_nm = "상병명.9", 
         op_cd = "수술코드.10", 
         op_nm = "수술명.11", 
         dx_clinical_cd = "상병임상용어ID.12", 
         dx_clinical_nm = "상병임상용어명.13",
         tx_op_clinical_cd = "처치수술임상용어ID.14",
         tx_op_clinical_nm = "처치수술임상용어명.15",
         incision_dt = "절개일시.16",
         closure_dt = "수술봉합일시.17",
         op_start_time = "수술시작시간.18",
         op_finish_time = "수술종료시간.19",
         op_time_min = "수술시간.분..20",
         main_surgery = "주수술여부.21",
         severity = "중증도여부.22",
         surgeon_id = "집도의사ID.23",
         surgeon_nm = "집도의사명.24",
         surgeon_dept_cd = "집도의사부서코드.25",
         surgeon_dept_nm = "집도의사부서명.26",
         surgeon_dept_detail_cd = "집도의사분과코드.27",
         surgeon_dept_detail_nm = "집도의사분과명.28",
         anesthesia_cd = "마취종류코드.29", 
         anesthesia_nm = "마취종류명.30")

merged_surgery <- merge(surgery0, pt0, by = "pt_id", all.x = T)
joined_surgery <- left_join(surgery0, pt0, by = "pt_id")

surgery0 <- joined_surgery %>%
  dplyr::select(pt_id, op_cd, op_nm, tx_op_clinical_cd, tx_op_clinical_nm, incision_dt, closure_dt, main_surgery, surgeon_id, surgeon_dept_cd, surgeon_dept_detail_cd, anesthesia_cd, anesthesia_nm, birth_date, sex) %>%
  mutate_at(vars(pt_id, op_cd, tx_op_clinical_cd, main_surgery, surgeon_id, surgeon_dept_cd, surgeon_dept_detail_cd, anesthesia_cd, sex), 
            funs(factor)) %>%
  mutate(birth_date = as.POSIXct(paste(joined_surgery$birth_date, "-01", sep = ""), format = "%Y-%m-%d")) %>%
  filter(incision_dt > ymd_hms("2019-01-01 00:00:00") & incision_dt <= ymd_hms("2023-12-31 23:59:59"))

## Transfusion------------------------------------------------------------------------------------------------

transfusion_order1 <- transfusion_order_raw %>%
  rename(pt_id = "환자번호.1", 
         ordr_cd = "처방코드.4", 
         ordr_nm = "처방명.5", 
         ordr_date = "처방일자.6", 
         visit_dt = "진료일시.8", 
         ward_cd = "병동코드.9", 
         dept_cd = "진료과코드.10", 
         dr_id_ordr = "처방의사ID.11",
         dr_nm_ordr = "처방의사명.12", 
         dr_id_faculty = "지정의사ID.13", 
         dr_nm_faculty = "지정의사명.14", 
         dr_id_resident = "주치의사ID.15", 
         dr_nm_resident = "주치의사명.16", 
         ordr_quan = "의사지시수량.17", 
         out_quan = "혈액은행불출수량.18", 
         return_quan = "혈액은행반납수량.19", 
         disuse_quan = "혈액은행폐기수량.20", 
         use_quan = "사용수량.21", 
         auto_transfusion = "자가수혈여부.22", 
         radiation = "방사선처리여부.23", 
         filtered = "필터적용구분코드.24", 
         acting_num = "액팅횟수.25", 
         Rh = "RH혈액형구분내용.26", 
         indication_cd = "적응증종류코드.27", 
         indication_nm = "적응증종류명.28") %>%
  mutate_at(vars(pt_id, ordr_cd, ward_cd, dept_cd, dr_id_ordr, dr_id_faculty, dr_id_resident, auto_transfusion, radiation, filtered, Rh, indication_cd), 
            funs(factor)) %>%
  mutate_at(vars(tidyr::ends_with("_quan")), 
            funs(as.numeric))
## Lab----------------------------------------------------------------------------------------------------------

lab_raw %>% colnames()

lab1 <- lab_raw %>%
  dplyr::select(-2, -3) %>%
  rename(pt_id = "환자번호#1", 
         receive_dt = "접수일시#4",
         lab_cd = "검사코드#5", 
         lab_nm = "검사명#6", 
         value = "검사결과수치값#7",
         unit_nm = "검사결과단위명#8") %>%
  distinct() %>%
  mutate_at(vars(pt_id, lab_cd), 
            funs(factor(.))) %>%
  filter(receive_dt > lubridate::ymd_hms("2018-11-30 23:59:59")) # 19년 1월 1일 수술한 환자의 1달 전 데이터까지 커버 위함

# Lab code별로 unit은 1개씩임# lab1 %>%

#   gcd, unit_nm) %>%
#   tally()

## Dx---------------------------------------------------------------------------------------------------------
head(dx_raw)
colnames(dx_raw)[1] <- "환자번호#1"

dx1 <- dx_raw %>%
  #dplyr::select(-"var#2") %>%
  rename(pt_id = "환자번호#1", 
         visit_dt = "진료일시#4", 
         dx_cd = "상병코드#5", 
         dx_nm = "상병명#6") %>%
            mutate_at(vars(pt_id, dx_cd), 
            list(~factor(.))) %>%
              mutate(dx_cd = str_sub(dx_cd, 1, 5))

## Med----------------------------------------------------------------------------------------------------------


coumarin_deriatives = c("TJIWARF", "TWAR-C", "TWAR05", "TWAR1", "TWAR2", "TWAR5", "TWARF2", "TWARF5", "TWARFA", "TWARFA1", "TWARFA5", "TWRF1", "TWRF5", "WARF", "WARF5")
direct_thrombin_inhibitors = c("DABI11", "DABI15", "TDABI11", "TDABI15", "XARGAT", "XLEPIR")
heparins = c("TXENO40", "TXENOXP", "XDALT10", "XDALT2", "XDALT7", "XDALTP", "XENOX", "XENOX4", "XENOX6", "XENOX8", "XHEPA", "XHEPA1", "XHEPA10", "XHEPA4", "XHEPA57", "XHEPA7")
direct_factor_Xa_inhibitors = c("APIXA", "APIXA5", "EDOX15", "EDOX3", "EDOX6", "RIVA2.5", "RIVAR15", "RIVAR20", "RIVARX", "T185057", "TAPI2.5", "TAPI5", "TAPIX2", "TAPIX5", "TATLAS", "TDU-TK", "TNRXARE", "TRIVA10", "TRIVA2", "TRIVARX")
miscellaneous_anticoagulants = c("SULO", "SULO2", "TIIT027", "TIIT314", "TSULD", "TSULD-1", "TXARIX", "XATHB", "XATHBSK", "XATHGC", "XFONDA", "XNAFA", "XNAFA5")
platelet_aggregation_inhibitors = c("ASP", "ASP5", "ASPC", "ASPCLO", "ASPE", "ASPM", "ASPP", "CILGIN2", "CILGINK", "CILO", "CILO1", "CILOSR", "CLOPI", "CLOPID", "DIPY", "DIPY75", "PRASU", "SARPO", "SARPSR3", "TADINOX", "TANAGPV", "TANP100", "TASAE", "TASAPV", "TASPCOM", "TASTRIX", "TBIOCIL", "TBRIL", "TBRIL60", "TCGINK", "TCILO1", "TCILOCR", "TCILOSR", "TCLOCOM", "TCLOE", "TCLOPI", "TDAVICI", "TDPR300", "TFLU", "THCP", "TICAG", "TICL", "TIIT057", "TIIT135", "TIIT148", "TIIT149", "TIIT157", "TIIT159", "TIIT167", "TIIT228", "TIIT233", "TIIT234", "TIIT255", "TIIT256", "TIIT300", "TIIT344", "TIIT345", "TNRASA", "TPLAVIT", "TPLAVIX", "TSAFOOD", "TSARP10", "TSARP30", "TSPG100", "TSPG300", "TTICA60", "TTICA90", "TTICAE", "XABCI", "XABCI5", "XASP", "XNAFA", "XNAFA5", "XTIRF")
thrombolytic_agents = c("XDFBR", "XTENE", "XTPA", "XTPA2", "XURK10", "XURK2", "XURK50")

med_raw %>% colnames()

med1 <- med_raw %>%
  dplyr::select(-"V1") %>%
  rename(pt_id = "환자번호.1", 
         ordr_cd = "처방코드.4", 
         ordr_nm = "처방명.5", 
         acting_time = "액팅예정일시.6", 
         acting_type_cd = "액팅종류코드.7", 
         acting_type_nm = "액팅종류.8") %>%
  filter(acting_type_cd %in% c("A", "C", "E", "I", "J", "K", "Q")) %>%
  mutate(med_cat = case_when(ordr_cd %in% coumarin_deriatives ~ "coumarin_deriatives", 
                             ordr_cd %in% direct_thrombin_inhibitors ~ "direct_thrombin_inhibitors", 
                             ordr_cd %in% heparins ~ "heparins", 
                             ordr_cd %in% direct_factor_Xa_inhibitors ~ "direct_factor_Xa_inhibitors", 
                             ordr_cd %in% miscellaneous_anticoagulants ~ "miscellaneous_anticoagulants", 
                             ordr_cd %in% platelet_aggregation_inhibitors ~ "platelet_aggregation_inhibitors", 
                             ordr_cd %in% thrombolytic_agents ~ "thrombolytic_agents")) %>%
  mutate_at(vars(pt_id, ordr_cd, med_cat), 
            funs(factor(.))) %>%
  dplyr::select(pt_id, med_cat, acting_time)




# ICD9cm ------------------------------------------------------------------

# icd9cm <- icd9cm %>%
#   rename(procedure_cd = `PROCEDURE CODE`, 
#          long_description = `LONG DESCRIPTION`, 
#          short_description = `SHORT DESCRIPTION`) %>%
#   mutate(procedure_cd = factor(procedure_cd))

