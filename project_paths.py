"""프로젝트 공통 경로 설정."""
import os
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
DATA_ROOT = Path(os.environ.get("MSBOS_DATA_ROOT", PROJECT_ROOT / "data")).expanduser().resolve()
RAW_DIR = DATA_ROOT / "raw"
PROCESSED_DIR = DATA_ROOT / "processed"
OUTPUT_DIR = Path(os.environ.get("MSBOS_OUTPUT_ROOT", PROJECT_ROOT / "outputs")).expanduser().resolve()
