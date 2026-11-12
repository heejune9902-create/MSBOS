# source("scr/2. preprocessing(columns).R")
load("data/new/train_test (v2024-05-26).RData")
# surgery data analysis ---------------------------------------------------
surgery1 <- surgery0 %>%
  left_join(pt0) %>%
  mutate(age = age(birth_date, incision_dt)) %>%
  filter(age >= 18) %>%
  mutate_at(vars(surgeon_dept_cd), 
            funs(ifelse(. == "", NA, .)))

surgery0$pt_id %>% unique() %>% length()
surgery0 %>%
  left_join(surgery0 %>% # op_id 부여 
              dplyr::select(pt_id, incision_dt, closure_dt) %>%
              distinct() %>%
              mutate(op_id = 1:n())) %>%
  arrange(desc(op_id)) %>%
  filter(row_number() == 1) %>% pull(op_id)

surgery1 <- surgery1 %>% 
  left_join(surgery1 %>% # op_id 부여 
              dplyr::select(pt_id, incision_dt, closure_dt) %>%
              distinct() %>%
              mutate(op_id = 1:n())) %>%
  filter(!is.na(op_nm))

surgery1$pt_id %>% unique() %>% length()
surgery1$op_id %>% unique() %>% length()


# 중복 제거하기 위한 vector 생성: 하나의 surgery id 에서, 두개 이상의 진료과가 있는경우 제외. 
rmv_op_id_cd1 <- surgery1 %>%
  dplyr::select(op_id, surgeon_dept_cd) %>%
  distinct() %>%
  mutate(value = 1) %>%
  ungroup() %>%  
  pivot_wider(names_from = surgeon_dept_cd, values_from = value, values_fill = 0) %>%
  rowwise() %>%
  mutate(sum = sum(c_across(!op_id))) %>%
  filter(sum != 1) %>%
  pull(op_id)
  
length(rmv_op_id_cd1)

surgery2 <- surgery1 %>%
  filter(!op_id %in% rmv_op_id_cd1) %>%
  filter(main_surgery == "Y")

# 두개 이상 진료과 Joint OP
length(rmv_op_id_cd1)
surgery1$pt_id %>% unique() %>% length() - surgery2$pt_id %>% unique() %>% length()

rmv_op_id_cd2 <- surgery2 %>%
  group_by(op_id) %>%
  tally() %>%
  filter(n >= 2) %>%
  pull(op_id)


surgery3 <- surgery2 %>%
  filter(!op_id %in% rmv_op_id_cd2)

# Main Surgery가 2개 이상인 경우 제외
length(rmv_op_id_cd2)
surgery2$pt_id %>% unique() %>% length() - surgery3$pt_id %>% unique() %>% length()

# 단일수술
surgery3$pt_id %>% unique() %>% length()
surgery3$op_id %>% unique() %>% length()


matching_op_tf_df1 <- surgery3 %>%
  mutate(incision_date = date(incision_dt), 
         closure_date = date(closure_dt)) %>%
  dplyr::select(pt_id, op_id, incision_date, closure_date)

transfusion_order2 <- transfusion_order1 %>%
  left_join(matching_op_tf_df1) %>%
  filter(ordr_date == incision_date) %>%
  dplyr::select(-incision_date, -closure_date)

# transfusion_order2 %>%
#    group_by(ordr_cd, ordr_nm) %>%
#    tally() %>%
#    writexl::write_xlsx("data/new/transfusion_category_for_descriptive_study.xlsx")

blood_cat_df <- readxl::read_xlsx("data/new/transfusion_category_for_descriptive_study (handled).xlsx")

transfusion_order3 <- transfusion_order2 %>%
  left_join(blood_cat_df, by = c("ordr_cd", "ordr_nm")) %>%
  filter(blood_cat == "RBC")



transfusion_order4 <- transfusion_order3 %>%
  dplyr::select(pt_id, op_id, ordr_quan, out_quan, return_quan, disuse_quan, use_quan, acting_num) %>%
  group_by(pt_id, op_id) %>%
  summarise(ordr_quan = sum(ordr_quan), 
            out_quan = sum(out_quan), 
            return_quan = sum(return_quan),
            disuse_quan = sum(disuse_quan), 
            use_quan = sum(use_quan), .groups = "drop")



# op+transfusion ----------------------------------------------------------
surgery3
transfusion_order4

surgery4 <- left_join(surgery3, transfusion_order4) %>%
  mutate_at(vars(ordr_quan, out_quan, return_quan, disuse_quan, use_quan), 
            funs(ifelse(is.na(.), 0, .))) %>%
  mutate(procedure_cd = str_replace_all(op_cd, "\\.", ""), 
         procedure_cd = str_replace_all(procedure_cd, "[:alpha:]", "")) %>%
  mutate(surgeon_dept_cd = as.character(surgeon_dept_cd)) %>%
  mutate(surgeon_dept_cd = case_when(surgeon_dept_cd %in% c("PED", "AN", "BR", "DER") ~ "etc", 
                                     TRUE ~ surgeon_dept_cd)) %>%
  mutate(surgeon_dept_cd = factor(surgeon_dept_cd)) %>%
  mutate(anesthesia_cd = case_when(anesthesia_cd %in% c(2, 3, 6) ~ 2 %>% as.character, 
                                   anesthesia_cd %in% 4 ~ 9 %>% as.character, 
                                   TRUE ~ anesthesia_cd %>% as.character))

surgery4
# join_table_icd9cm <- surgery5 %>%
#   dplyr::select(op_cd, op_nm, procedure_cd) %>%
#   left_join(icd9cm) %>%
#   distinct()

join_table_icd9cm <- surgery4 %>%
  dplyr::select(op_cd, op_nm, procedure_cd) %>%
  left_join(icd9cm) %>%
  distinct()
  
# writexl::write_xlsx(join_table_icd9cm, "data/new/join_table_icd9cm.xlsx")
join_table_icd9cm <- readxl::read_xlsx("data/new/join_table_icd9cm_handled.xlsx", col_types = c("text", "text", "text"))


# ICD9cm 기준으로 합치기 ---------------------------------------------------------
surgery5 <- surgery4 %>%
  dplyr::select(-procedure_cd, -op_nm) %>%
  left_join(join_table_icd9cm) %>%
  left_join(icd9cm, by = c("procedure_cd_fin" = "procedure_cd"))

num_op <- nrow(surgery5)
num_op
# 수혈 오더가 안난 수술 수 (비율)
num_non_tf <- surgery5 %>%
  filter(ordr_quan == 0) %>%
  nrow()
num_non_tf # 수 
num_non_tf/num_op * 100 # 비율

# 수혈 오더가 난 수술 수 (비율)
surgery6 <- surgery5 %>% 
  filter(ordr_quan != 0)

surgery6$pt_id %>% unique() %>% length() # 수술일자 적혈구 수혈 unq_pt
num_tf <- surgery5 %>%
  filter(ordr_quan != 0) %>%
  nrow()
num_tf # 수 
num_tf/num_op * 100 # 비율

surgery5
colnames(surgery5)
qplot(surgery5$ordr_quan)

# 실제 수혈 시행된 수술 수 (비율)
num_tf_act <- surgery5 %>%
  filter(!is.na(use_quan)) %>%
  filter(use_quan != 0) %>%
  nrow()

num_tf_act # 수수
num_tf_act/num_op * 100 # 비율

(1-(num_tf_act/num_tf)) * 100 # 이 7%의 수술은 도대체 뭔지 살펴볼 가치가 있음

target_procedure <- surgery5 %>%
  group_by(procedure_cd_fin) %>%
  tally() %>%
  # filter(n>=50) %>%
  pull(procedure_cd_fin)

dept_join_table <- surgery5 %>%
  group_by(procedure_cd_fin, surgeon_dept_cd) %>%
  tally() %>%
  ungroup() %>%
  arrange(procedure_cd_fin, surgeon_dept_cd, desc(n)) %>%
  group_by(procedure_cd_fin) %>%
  filter(row_number() == 1) %>%
  rename(surgeon_dept_cd2 = surgeon_dept_cd) %>%
 dplyr::select(-n)

surgery7 <- surgery5 %>%
  filter(procedure_cd_fin %in% target_procedure) %>%
  mutate(transfused_yn = ifelse(use_quan == 0, 0, 1)) # use_quan이 0 이면 사용안함 (0), 0이 아니면 사용함 (1)

surgery7$pt_id %>% unique() %>% length()
nrow(surgery6)

table2_out <- surgery7 %>%
  group_by(procedure_cd_fin, long_description) %>%
  summarise(n = n(), 
            # ordr_quan
            sum_ordr_quan = sum(ordr_quan),
            mean_ordr_quan = mean(ordr_quan), 
            sd_ordr_quan = sd(ordr_quan), 
            sigma_ordr_quan = sqrt(sd_ordr_quan^2/n), 
            lci_ordr_quan = mean_ordr_quan-1.96*sigma_ordr_quan,
            uci_ordr_quan = mean_ordr_quan+1.96*sigma_ordr_quan,
            text_ordr_quan = paste(round(mean_ordr_quan, 3), " [", round(lci_ordr_quan, 3), "-", round(uci_ordr_quan, 3), "]", sep = ""), 
            # use_quan
            sum_use_quan = sum(use_quan),
            mean_use_quan = mean(use_quan), 
            sd_use_quan = sd(use_quan), 
            sigma_use_quan = sqrt(sd_use_quan^2/n), 
            lci_use_quan = mean_use_quan-1.96*sigma_use_quan,
            uci_use_quan = mean_use_quan+1.96*sigma_use_quan,
            text_use_quan = paste(round(mean_use_quan, 3), " [", round(lci_use_quan, 3), "-", round(uci_use_quan, 3), "]", sep = ""), 
            delta_ordr_use_quan = mean_ordr_quan-mean_use_quan,
            delta_ordr_use_quan = round(delta_ordr_use_quan, 3),
            msbos = round(mean_use_quan, 0),
            num_transfused_case = sum(transfused_yn), 
            percent_transfused_case = (num_transfused_case/n*100) %>% round(1), 
            text_transfused_surgeries = paste(num_transfused_case, " (", percent_transfused_case, ")", sep = "")) %>%
  filter(mean_use_quan > 0) %>%
  left_join(dept_join_table) %>%
  dplyr::select(surgeon_dept_cd2, procedure_cd_fin, long_description, n, text_transfused_surgeries, sum_ordr_quan, text_ordr_quan, sum_use_quan, text_use_quan, delta_ordr_use_quan, msbos)

# writexl::write_xlsx(table2_out, "output/table2_out.xlsx")




# dx data ----------------------------------------------------------------------

dx1
surgery5 %>%
  dplyr::select(pt_id, op_id, incision_dt)

dx1 %>% head()
data10 <- data.frame(id = sample(1:10, size = 250, replace = T), 
                     code = sample_diag(n = 250, version = "ICD10_2011"), 
                     stringsAsFactors = FALSE)

a <- surgery5$pt_id %>% head(20)


dx2 <- left_join(surgery5 %>% dplyr::select(pt_id, op_id, incision_dt), 
                 dx1) %>%
  filter(incision_dt > visit_dt)

dx2 <- dx2 %>% group_by(pt_id) %>% 
  mutate(prev_op_id_incision_dt = incision_dt[match(op_id - 1, op_id)]) %>%
  ungroup() %>% 
  filter(is.na(prev_op_id_incision_dt) | visit_dt > prev_op_id_incision_dt)

dx3 <- comorbidity(x = dx2, id = "op_id", code = "dx_cd", map = "charlson_icd10_quan", assign0 = FALSE)
dx3 <- dx3 %>% mutate(mld = ifelse(msld == 1, 0, mld),
               canc = ifelse(metacanc == 1, 0, canc),
               diab = ifelse(diabwc == 1, 0, diab))
dx3 %>% filter((mld == 1 & msld == 1) | (canc ==1 & metacanc == 1) | (diab == 1 & diabwc == 1))


total1 <- surgery7 %>%
  left_join(dx3)


# lab_data ---------------------------------------------------------------------
lab1

temp <- left_join(surgery5 %>% dplyr::select(pt_id, op_id, incision_dt), 
                  lab1) %>%
  mutate(gap_time = difftime(incision_dt, receive_dt, units = "days") %>% as.numeric) 

lab2 <- left_join(surgery5 %>% dplyr::select(pt_id, op_id, incision_dt), 
                  lab1) %>%
  mutate(gap_time = difftime(incision_dt, receive_dt, units = "days") %>% as.numeric) %>%
  filter(gap_time >= 0 & gap_time <= 31) %>% 
  dplyr::select(-incision_dt, -receive_dt, -unit_nm) %>%
  mutate(lab = case_when(lab_cd == "BL2013" ~ "Hb", 
                         lab_cd == "BL2016" ~ "Plt", 
                         lab_cd == "BL211101" ~ "PT_sec",
                         lab_cd == "BL211102" ~ "PT_pct", 
                         lab_cd == "BL211103" ~ "PT_inr", 
                         lab_cd == "BL2112" ~ "aPTT", 
                         lab_cd == "BL2113" ~ "fibrinogen")) %>%
  filter(lab %in% c("Hb", "Plt", "PT_inr", "aPTT")) %>%
  arrange(op_id, lab, gap_time) %>%
  group_by(op_id, lab) %>%
  filter(row_number() == 1) %>% ungroup()



lab3 <- lab2 %>%
  dplyr::select(op_id, lab, value) %>%
  pivot_wider(names_from = lab, values_from = value) %>%
  ungroup()

skimr::skim(lab3)

total2 <- total1 %>%
  left_join(lab3) %>%
  # 수술 30일 이내 Hb, Plt, PT, aPTT 검사를 한번도 시행하지 않은경우는 제외
  filter(!(is.na(Hb)& is.na(PT_inr) & is.na (Plt) & is.na(aPTT))) %>%
  ungroup() %>%
  # procedure cd (ICD9cm 기준) 결측인 것 제외
  filter(!is.na(procedure_cd_fin.y))


# med_data ---------------------------------------------------------------------
med1

# 약물은 3일 이내 투여했는지 여부로 봄
med2 <- left_join(surgery5 %>%dplyr::select(pt_id, op_id, incision_dt), 
                  med1) %>%
  mutate(gap_time = difftime(incision_dt, acting_time, units = "days") %>% as.numeric) %>%
  filter(gap_time >= 0 & gap_time <= 3) %>%
  arrange(op_id, med_cat, gap_time) %>%
  group_by(op_id, med_cat) %>%
  filter(row_number() == 1) %>% 
  ungroup()

med3 <- med2 %>%
  mutate(value = 1) %>%
  dplyr::select(op_id, med_cat, value) %>%
  pivot_wider(names_from = med_cat, values_from = value, values_fill = 0)

total3 <- total2 %>%
  left_join(med3) %>%
  ungroup() %>%
  mutate_at(vars(mi:aids, heparins:direct_thrombin_inhibitors), 
            funs(ifelse(is.na(.), 0, .)))

# surgeon's experience ---------------------------------------------------------


# table3 -----------------------------------------------------------------------

table3_out <- surgery7 %>%
  group_by(surgeon_dept_cd) %>%
  summarise(num_total_surgery = n(), 
            sum_ordr = sum(ordr_quan), 
            sum_out = sum(out_quan), 
            sum_use = sum(use_quan), 
            sum_return = sum(return_quan),
            sum_disuse = sum(disuse_quan), 
            mean_tf_per_surgery = mean(use_quan) %>% round(2), # 수술 하나 당 몇개 썼는지
            return_rate = (sum_return/sum_out*100) %>% round(2), 
            num_transfused_case = sum(transfused_yn), 
            percent_transfused_case = (num_transfused_case/num_total_surgery*100) %>% round(1), 
            text_transfused_surgeries = paste(num_transfused_case, " (", percent_transfused_case, ")", sep = "")) %>% 
  ungroup() # 불출대비 반납 

# total
surgery7 %>%
  summarise(num_total_surgery = n(), 
            sum_ordr = sum(ordr_quan), 
            sum_out = sum(out_quan), 
            sum_use = sum(use_quan), 
            sum_return = sum(return_quan),
            sum_disuse = sum(disuse_quan), 
            mean_tf_per_surgery = mean(use_quan) %>% round(2), # 수술 하나 당 몇개 썼는지
            return_rate = (sum_return/sum_out*100) %>% round(2), 
            num_transfused_case = sum(transfused_yn), 
            percent_transfused_case = (num_transfused_case/num_total_surgery*100) %>% round(1), 
            text_transfused_surgeries = paste(num_transfused_case, " (", percent_transfused_case, ")", sep = ""))#불출대비 반납 

# writexl::write_xlsx(table3_out, "output/table3_out.xlsx")


# table1 -----------------------------------------------------------------------

total3
table1_out <- mytable(~surgeon_dept_cd+anesthesia_cd+sex+age+mi + chf + pvd+ cevd+ dementia+ cpd+ rheumd+ pud+mld+diab+diabwc+hp+rend+ canc+msld+metacanc+aids+aPTT+Hb+Plt+PT_inr+heparins+direct_factor_Xa_inhibitors+coumarin_deriatives+miscellaneous_anticoagulants+direct_thrombin_inhibitors, data = total3)
table1_out
# mycsv(a, file = "output/table1.csv")



# 전체 처방된 RBC unit 수
surgery7 %>%
  summarise(sum_ordr_quan = sum(ordr_quan))
# 전체 불출된 RBC unit 수
surgery7 %>%
  summarise(sum_out_quan = sum(out_quan))

# 전체 반납된 RBC unit 수
surgery7 %>%
  summarise(sum_return_quan = sum(return_quan))

## 반납율 (불출대비)
round(1465/134708 * 100, 2)

# 전체 사용된 RBC unit 수
surgery7 %>%
  summarise(sum_use_quan = sum(use_quan))

#부서별 반납율


# 데이터 성격
total3
total3$pt_id %>% unique() %>% length()
total3$op_id %>% unique() %>% length()

# 30회 이상 수행된 수술
frequent_op_cd <- total3 %>%
  group_by(procedure_cd_fin.y) %>%
  tally() %>%
  filter(n >= 30) %>% 
  pull(procedure_cd_fin.y) %>%
  ungroup()
  unique()

total4 <- total3 %>%
  filter(procedure_cd_fin %in% frequent_op_cd)

# MSBOS가 1 이상인 수술만 포함

msbos_op_cd <- table2_out %>%
  filter(msbos >= 1) %>% # MSBOS 1 이상
  pull(procedure_cd_fin)

total5 <- total4 %>%
  filter(procedure_cd_fin %in% msbos_op_cd)

join_table_surgery <- total5 %>%
  group_by(procedure_cd_fin, long_description) %>%
  summarise(n = n(), 
            # use_quan
            sum_use_quan = sum(use_quan),
            mean_use_quan = mean(use_quan), 
            sd_use_quan = sd(use_quan), 
            sigma_use_quan = sqrt(sd_use_quan^2/n), 
            lci95_use_quan = mean_use_quan-1.96*sigma_use_quan,
            uci95_use_quan = mean_use_quan+1.96*sigma_use_quan,
            lci98_use_quan = mean_use_quan-2.326*sigma_use_quan,
            uci98_use_quan = mean_use_quan+2.326*sigma_use_quan, 
            lci99_use_quan = mean_use_quan-2.576*sigma_use_quan,
            uci99_use_quan = mean_use_quan+2.576*sigma_use_quan, 
            num_transfused_case = sum(transfused_yn), 
            percent_transfused_case = (num_transfused_case/n*100) %>% round(1), 
            msbos = round(mean_use_quan, 0)) %>% 
  ungroup()

left_join(total5, join_table_surgery)


# 아래 아웃라이어 제거방식은 부적절: 낮은숫자 (1~2등)에 걸려있는애들 다 날아감 --------------------------
# # 각 수술별 95% 신뢰구간 이상의 아웃라이어 제거 시 -------------------------------------------
# # outlier removal option1, n = 221910
# outlier_option1 <- left_join(total5, join_table_surgery) %>%
#   filter(use_quan <= uci95_use_quan)
# nrow(outlier_option1)/nrow(total5) *100
# qplot(outlier_option1$use_quan)
# 
# 
# # 각 수술별 98% 신뢰구간 이상의 아웃라이어 제거 시 -------------------------------------------
# # outlier removal. n = 222219
# outlier_option2 <- left_join(total5, join_table_surgery) %>%
#   filter(use_quan <= uci98_use_quan) 
# nrow(outlier_option2)/nrow(total5)*100
# qplot(outlier_option2$use_quan)
# 
# # 각 수술별 99% 신뢰구간 이상의 아웃라이어 제거 시 -------------------------------------------
# outlier_option3 <- left_join(total5, join_table_surgery) %>%
#   filter(use_quan <= uci99_use_quan)
# nrow(outlier_option3)/nrow(total5)*100
# 
# # 10% 미만인것 제외 X
# # outlier_option1_2 <- outlier_option1 %>%
# #   filter(percent_transfused_case >= 10)
# # 
# 
# outlier_option1$pt_id %>% unique() %>% length()
# outlier_option1$op_id %>% unique() %>% length()


total6 <- total5 %>%
  dplyr::select(pt_id, op_id, 
         # train text 나누기
         incision_dt, 
         # Input
         procedure_cd_fin, surgeon_dept_cd, anesthesia_cd, sex, age, 
         
         mi, chf, pvd, 
         cevd, dementia, cpd, rheumd, pud, mld, diab, diabwc, 
         hp, rend, canc, msld, metacanc, aids, Hb, PT_inr, 
         Plt, aPTT, heparins, miscellaneous_anticoagulants, direct_factor_Xa_inhibitors, 
         coumarin_deriatives, direct_thrombin_inhibitors, 
         # Output
         use_quan) %>%
  mutate(anesthesia_cd = ifelse(anesthesia_cd == "0", "9", anesthesia_cd)) %>%
  mutate_at(vars(surgeon_dept_cd, anesthesia_cd, sex,  mi, chf, pvd, 
                 cevd, dementia, cpd, rheumd, pud, mld, diab, diabwc, 
                 hp, rend, canc, msld, metacanc, aids,
                 heparins, direct_factor_Xa_inhibitors, coumarin_deriatives, miscellaneous_anticoagulants, direct_thrombin_inhibitors), 
            funs(factor))

total6$pt_id %>% unique() %>% length()
total6$op_id %>% unique() %>% length()

# lab missing 제거 
skim(total6)

total7 <- total6 %>%
  na.omit() %>%
  mutate(procedure_cd_fin = factor(procedure_cd_fin))

total7$pt_id %>% unique() %>% length()
total7$op_id %>% unique() %>% length()
total7$procedure_cd_fin %>% unique() %>% length()

set.seed(1)

total8 <- total7 %>% 
  distinct() 
nrow(total8)
n = nrow(total8)


total9 <- total8 %>%
  arrange(incision_dt) %>%
  mutate(row_no = 1:n()) %>%
  mutate(train_test = ifelse(row_no <= 0.7*n, "train", "test")) %>%
  dplyr::select(-row_no) %>%
  left_join(table2_out %>% dplyr::select(procedure_cd_fin, msbos))

mytable(~., data = total9)
skimr::skim(total9)
length(unique(total9$op_id)) # 검증 

# tidy dataset 생성 ---------------------------------------------------------

total10 <- total9 %>%
  # dplyr::select(-pt_id, -op_id, -incision_dt) %>% #학습에 불필요한 데이터 제거
  # mutate(procedure_cd_fin = paste("procedure_", procedure_cd_fin, sep = ""), 
  #        value = factor("1", levels = c("0", "1"))) %>%
  # pivot_wider(names_from = procedure_cd_fin, 
  #             values_from = value, 
  #             values_fill = factor("0", levels = c("0", "1"))) %>%
  dplyr::select(-pt_id, -op_id, -incision_dt) %>%
  dplyr::select(train_test, use_quan, dplyr::everything())


# outlier 제거 여부 결정 --------------------------------------------------------
#코드 추가 여부 고민중

train_df <- total10 %>%
  filter(train_test == "train")

test_df <- total10 %>%
  filter(train_test == "test")
# save(list = c("total10", "train_df", "test_df"), file = "data/new/train_test (v2024-10-29).RData")
