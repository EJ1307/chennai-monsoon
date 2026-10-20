# Downloads the three raw files the analysis starts from (about 44 MB in all).
# Run from the project folder:  Rscript prepare/download_data.R
# Files that are already there are left alone.

options(timeout = 900)

files <- c(
  "data/raw/chennai_daily_1991_2023.csv" =
    "https://data.opencity.in/dataset/256d1a20-adf5-4e3a-ae18-27664339117a/resource/3086f865-a04c-431e-815d-105ae658871f/download/e5c275eb-a4f2-4412-9677-73654e8f5f4d.csv",
  "data/raw-tn/tamilnadu_daily_1991_2023.csv" =
    "https://data.opencity.in/dataset/256d1a20-adf5-4e3a-ae18-27664339117a/resource/fd176db8-caca-42e5-b5a0-9dc78a3d73a9/download/4804c971-4f38-40b5-b203-aba08e19b937.csv",
  "data/raw-openmeteo/era5_hourly_nungambakkam_1990_2023.json" =
    "https://archive-api.open-meteo.com/v1/archive?latitude=13.0674&longitude=80.2376&start_date=1990-12-31&end_date=2023-12-31&hourly=precipitation&timezone=GMT&models=era5"
)

for (f in names(files)) {
  if (file.exists(f)) {
    cat("have  ", f, "\n")
    next
  }
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  download.file(files[[f]], f, mode = "wb", quiet = TRUE)
  cat("got   ", f, sprintf("(%.1f MB)", file.size(f) / 1e6), "\n")
}
