library(readr)
library(corrr)
library(dplyr)
library(ggplot2)
library(SummarizedActigraphy)
library(purrr)
library(mgcv)

options(digits.secs = 3)
fnames = list.files(path = here::here("mats"), full.names = TRUE, pattern = "[.]mat")
gt3x = list.files(path = here::here("gt3x"), full.names = TRUE, pattern = "[.]gt3x")

df = tibble::tibble(
  mat_file = fnames,
  id = sub("RAW[.]mat", "", basename(mat_file)))
df = df %>% 
  mutate(
    gt3x_file = here::here("gt3x", paste0(id, ".gt3x.gz")),
    qc_file = here::here("qc", paste0(id, "_read.gt3x.txt")),
    qc_file2 = here::here("qc", paste0(id, "_AGread.txt"))
  )

df = df %>% 
  mutate(csv_file = here::here("open_measures", paste0(id, "_MIMS.csv.gz")),
         ac_file = here::here("csv", paste0(id, "60sec.csv.gz")))
rm(fnames)


df = df %>% 
  filter(grepl("^[0-9]", basename(gt3x_file)))

df = df %>% 
  filter(file.exists(gt3x_file))
df = df %>% 
  filter(file.exists(csv_file) & file.exists(ac_file))
df = df %>% 
  filter(file.exists(qc_file))

ac_file = df$ac_file[1]
csv_file = df$csv_file[1]

fname = here::here("results", "comparison_data.rds")
rerun = FALSE


if (!file.exists(fname) || rerun) {
  
  pb <- progress_estimated(length(df$csv_file))
  x = df$csv_file
  names(x) = df$id
  data = purrr::map2_dfr(x, df$ac_file, function(csv_file, ac_file) {
    pb$tick()$print()
    
    acc_df = read_csv(csv_file, progress = FALSE,
                      col_types = cols(
                        HEADER_TIME_STAMP = col_datetime(format = ""),
                        MIMS_UNIT = col_double(),
                        mean_r = col_double(),
                        MAD = col_double(),
                        MEDAD = col_double(),
                        SD = col_double(),
                        AI = col_double()
                      ))
    acc_df = acc_df %>% 
      select(HEADER_TIME_STAMP, MIMS_UNIT, AI, MAD)
    suppressWarnings({
      format = extract_acc_header(ac_file)$format
    })
    ac_df = read_csv(ac_file, progress = FALSE, 
                     skip = 10,
                     col_types = cols(
                       timestamp = col_datetime(format = ""),
                       vectormagnitude = col_double(),
                       .default = col_character()
                     ))
    ac_df = ac_df %>% 
      select(HEADER_TIME_STAMP = timestamp, vectormagnitude)
    acc_df = full_join(acc_df, ac_df, by = "HEADER_TIME_STAMP")
  }, .id = "id")
  
  readr::write_rds(data, fname)
}

data = readr::read_rds(fname)


data = data %>% 
  mutate(MIMS_UNIT = ifelse(MIMS_UNIT < 0, 0, MIMS_UNIT),
         MIMS_UNIT = round(MIMS_UNIT, 1),
         vectormagnitude = round(vectormagnitude, 1)) 

# data %>% 
#   ggplot(aes(x = MIMS_UNIT, y = vectormagnitude)) + 
#   geom_point(alpha = 0.01) + 
#   geom_smooth(se = FALSE)

# Sample based on ID
# Sample based on Subject
# Throw how high movers 
   # based on max value (# > 10000)
   # based on avg movement over good data
# throw out low movers


pred_df = data.frame(vectormagnitude = c(100, 1853, 1952, 2690))

mod = gam(MIMS_UNIT ~ s(vectormagnitude, bs = "cr"), data = data)

predict(mod, newdata = pred_df)

mod_over0 = gam(MIMS_UNIT ~ s(vectormagnitude, bs = "cr"), 
                data = data %>% 
                  filter(vectormagnitude > 0))
predict(mod_over0, newdata = pred_df)



ai_mod = gam(AI ~ s(vectormagnitude, bs = "cr"), data = data)

predict(ai_mod, newdata = pred_df)

ai_mod_over0 = gam(AI ~ s(vectormagnitude, bs = "cr"), 
                data = data %>% 
                  filter(vectormagnitude > 0))
predict(ai_mod_over0, newdata = pred_df)


data = data %>% 
  mutate(mvpa = vectormagnitude >= 1853)


bivar = data %>% 
  select(MIMS_UNIT, vectormagnitude) %>% 
  group_by(MIMS_UNIT, vectormagnitude) %>% 
  summarise(n = n())
