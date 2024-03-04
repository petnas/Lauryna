
library(dplyr)
library(openxlsx)

HNF1B_simple <- read_excel("C:/Coding/studentai/Lauryna/HNF1B_simple.xlsx")

HNF1B_simple <- HNF1B_simple %>%
  mutate(age_pre = ifelse(`Age (y)...5` == '<1', 1, `Age (y)...5`)) %>%
  mutate(age_post = ifelse(`Age (y)...18` == '<1', 1, `Age (y)...18`)) %>%
  transform(age_pre = as.numeric(age_pre)) %>%
  transform(age_post = as.numeric(age_post)) %>%
  mutate(age_pre = ifelse(age_pre == 0, NA, age_pre)) %>%
  mutate(age_post = ifelse(age_post == 0, NA, age_post))

HNF1B_simple_exp <- HNF1B_simple

extract_element_end <- function(string, end_position) {
  split_string <- strsplit(string, "[\\.]")
  element <- rev(split_string[[1]])[end_position]
  return(element)
}

HNF1B_simple_exp$baltymo_kiekis <- lapply(HNF1B_simple_exp$HNF1B.mutation..localization..nucleotide.and.amino.acid.change., extract_element_end, end_position=1)
HNF1B_simple_exp$regionas <- sapply(HNF1B_simple_exp$HNF1B.mutation..localization..nucleotide.and.amino.acid.change., extract_element_end, end_position=2)
HNF1B_simple_exp$funkcija <- sapply(HNF1B_simple_exp$HNF1B.mutation..localization..nucleotide.and.amino.acid.change., extract_element_end, end_position=3)


HNF1B_type<-HNF1B_simple_exp %>%
  mutate(mutacijos_tipas = case_when(
    grepl('deletion', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'deletion',
    grepl('missense', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'missense',
    grepl('nonsense', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'nonsense',
    grepl('splice', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'splice',
    grepl('del', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'deletion',
    grepl('stop', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'nonsense',
    grepl('duplication', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'duplication',
    grepl('insertion', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'insertion',
    grepl('frameshift', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ 'frameshift',
    )
  ) %>%
    mutate(heterozygous = case_when(
      grepl('heterozygous', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ TRUE,
      grepl('het', HNF1B.mutation..localization..nucleotide.and.amino.acid.change., ignore.case = TRUE) ~ TRUE)
      )

HNF1B_type[is.na(HNF1B_type)] <- ""

write.xlsx(HNF1B_type, 'HNF1B_curated.xlsx')
