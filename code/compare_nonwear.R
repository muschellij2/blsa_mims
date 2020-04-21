library(dplyr)
library(readr)
library(here)
library(tidyr)
library(lme4)
library(ggplot2)
source(here::here("code/helper_functions.R"))

setwd(here::here())

mims = list.files(pattern = "_MIMS.csv.gz",
                  path = "mims", full.names = TRUE)

df = tibble(
  mims = mims, 
  csv = file.path("csv", 
                  sub("_MIMS", "_nonwear", basename(mims))),
  outfile = file.path("compare", basename(csv))
)
df = df %>% 
  filter(file.exists(csv))
ifile = 1
quants = seq(0, 5, by = 0.1)
thresh_names = sprintf("thresh_%0.2f", quants)

# bad one
df = df %>% 
  filter(!grepl("112617WaTAS1D46140292", mims))

if (!all(file.exists(df$outfile))) {
  for (ifile in seq(nrow(df))) {
    
    idf = df[ifile,]
    outfile = idf$outfile
    if (!file.exists(outfile)) {
      mims = read_csv(idf$mims)
      mims_value = mims$MIMS_UNIT
      threshes = sapply(quants, function(x) WearNonWear(mims$MIMS_UNIT > x))
      colnames(threshes) = thresh_names
      stopifnot(!any(is.na(threshes)))
      threshes = threshes > 0
      threshes = as.data.frame(threshes)
      mims = cbind(mims, threshes)
      
      csv = read_csv(idf$csv) 
      csv = csv %>% 
        select(timestamp, vectormagnitude, wear) %>% 
        rename(HEADER_TIME_STAMP = timestamp,
               csv_wear = wear)
      stopifnot(!any(is.na(csv$csv_wear)))
      
      sd = setdiff(mims$HEADER_TIME_STAMP, csv$HEADER_TIME_STAMP)
      stopifnot(length(sd) == 0)
      
      sd = setdiff(csv$HEADER_TIME_STAMP, mims$HEADER_TIME_STAMP)
      stopifnot(length(sd) == 0)      
      
      out = full_join(mims, csv, by = "HEADER_TIME_STAMP")
      bad = out %>% 
        select(-HEADER_TIME_STAMP, -MIMS_UNIT, 
               -vectormagnitude)
      readr::write_csv(out, outfile)
    }
  }
}

files = df$outfile
names(files) = files
out = purrr::map_df(files, readr::read_csv, .id = "file")

example_subject = sample(unique(df$outfile), size = 1)
dd = out %>% 
  filter(file %in% example_subject) %>% 
  select(HEADER_TIME_STAMP, MIMS_UNIT, vectormagnitude) %>% 
  gather(unit, value, MIMS_UNIT, vectormagnitude) 
dd %>% 
  ggplot(aes(x = HEADER_TIME_STAMP, y =  value)) + 
  geom_line() +
  facet_wrap(~ unit, scales = "free_y", ncol = 1)

reg10 = out %>% 
  lm(log10(vectormagnitude+1) ~ log10(MIMS_UNIT+1), data = .)
summary(reg10)

mixed10 = out %>% 
  lmer(log10(vectormagnitude+1) ~ log10(MIMS_UNIT+1) + (1|file), data = .)
summary(mixed10)

reg = out %>% 
  lm(vectormagnitude ~ MIMS_UNIT, data = .)
summary(reg)

mixed = out %>% 
  lmer(vectormagnitude ~ MIMS_UNIT + (1|file), data = .)
summary(mixed)

corrs = out %>% 
  group_by(file) %>% 
  summarise(
    cor_log10 = cor(log10(MIMS_UNIT+1), log10(vectormagnitude + 1)),
    spear_cor_log10 = cor(log10(MIMS_UNIT+1), log10(vectormagnitude + 1),
                          method = "spearman"),
    cor = cor(MIMS_UNIT, vectormagnitude),
    spear_cor = cor(MIMS_UNIT, vectormagnitude, 
                    method = "spearman"))

g = out %>% 
  ggplot(aes(x = MIMS_UNIT, y = vectormagnitude)) + 
  geom_hex() +
  geom_smooth(method = "lm", se = FALSE)
png("results/scatterplot_MIMS.png", res= 300, units = "in", height=5,
    width = 6)
print(g)
dev.off()


lcorrs = corrs %>% 
  gather(cor_type, value, cor, spear_cor, cor_log10, spear_cor_log10) %>% 
  mutate(
    logged = ifelse(grepl("log10", cor_type), "logged", "raw"),   
    cor_type = case_when(
    cor_type == "cor" ~ "Pearson",
    cor_type == "spear_cor" ~ "Spearman",
    cor_type == "cor_log10" ~ "Pearson",
    cor_type == "spear_cor_log10" ~ "Spearman"    
  ))
lcorrs %>% 
  group_by(logged, cor_type) %>% 
  summarise(mean(value))
g = lcorrs %>%
  ggplot(aes(x = value)) + 
  facet_wrap(logged ~cor_type) + 
  geom_histogram()
png("results/histogram_correlation.png", res= 300, units = "in", height=5,
    width = 7)
print(g)
dev.off()


long = out %>% 
  select(-vectormagnitude, -MIMS_UNIT) %>% 
  gather(threshold, value, starts_with("thresh")) 
ind = which(is.na(long$value))
any(is.na(long$value))
tab = long %>% 
  group_by(file, threshold) %>% 
  summarise(tp = sum(value & csv_wear),
            fp = sum(!value & csv_wear),
            fn = sum(value & !csv_wear),
            tn = sum(!value & !csv_wear),
            n = n())
stopifnot(!any(is.na(tab)))
tab = tab %>% 
  mutate(threshold = as.numeric(sub("thresh_", "", threshold)),
         accuracy = (tp + fp + 0.01)/(tp + fp + fn + fp + 0.01),
         sensitivity = (tp + 0.1) / (tp + 0.01 + fp),
         specificity = (tn + 0.01) / (tn + 0.01 + fn),
         dice = (2 * tp + 0.01) / (2*tp + 0.01 + fn + fp),
         youden = sensitivity + specificity - 1)
stopifnot(!any(is.na(tab)))
sums = tab %>% 
  ungroup() %>% 
  select(threshold, accuracy, sensitivity, 
         specificity, dice, youden) %>% 
  group_by(threshold) 
sums %>%
  summarise_all(.funs = list(~mean(.))) %>% 
  filter(threshold == 0) %>% 
  as.data.frame()


png("results/accuracy_wear.png", res= 300, units = "in", height=5,
    width = 6)
sums %>% 
  select(threshold, accuracy) %>% 
  filter(threshold < 2) %>% 
  ggplot(aes(x = factor(threshold), y = accuracy)) + 
  geom_boxplot() + coord_flip()
dev.off()

sums %>% 
  ggplot(aes(x = factor(threshold), y = sensitivity)) + 
  geom_boxplot() + coord_flip()


