# MSBOS

수술 관련 적혈구 사용량을 예측하고 기관별 모델과 통합 모델의 성능을 비교한다.

## 실행 순서

1. 프로젝트 루트에서 실행한다. 경로는 `project_paths.py`와 `scripts/00_setup.R`에서 설정하며 `MSBOS_DATA_ROOT`, `MSBOS_OUTPUT_ROOT`로 변경할 수 있다.
2. R 전처리는 같은 세션에서 `scripts/01_load_data.R`, `02_standardize_columns.R`, `03_build_cohort.R` 순서로 실행한다. 각 스크립트가 공통 설정을 불러온다.
3. 모델 학습과 평가는 `notebooks/05_train_evaluate_models.ipynb`를 위에서 아래로 실행한다.
4. 기관 통합 작업은 `notebooks/04_harmonize_institutions.ipynb`에서 별도로 실행한다.
