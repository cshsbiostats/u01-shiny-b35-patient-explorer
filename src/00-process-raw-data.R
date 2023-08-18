library(tidyverse)
library(readxl)
library(ggsankey)

sheets <- readxl::excel_sheets('data/B35data_Main_AE_QOL 8-25-2020.xlsx')

sheets <- sheets[2:length(sheets)]

data <- map(sheets, \(x) {
  
  readxl::read_excel(
    here::here('data/B35data_Main_AE_QOL 8-25-2020.xlsx'),
    sheet = x,
    col_types = 'text'
  )
  
}) |> set_names(sheets)



qol <- data$`QOL (long fmt)` |> 
  left_join(data$Main, by = join_by(PATIENTID))

ae <- local({
  
  ae <- data$`AE (long fmt)` |> 
    left_join(data$Main, by = join_by(PATIENTID)) |> 
    janitor::clean_names()
  
  ae <- ae |> 
    select(patientid, trt, ae, ncycle, ae_grade) |> 
    filter(!is.na(ae)) |> 
    mutate(ncycle = as.numeric(ncycle)) |> 
    pivot_wider(
      names_from = ae,
      values_from = ae_grade,
      values_fn = max,
      values_fill = '0-1'
    ) |> 
    complete(nesting(patientid, trt), ncycle) |> 
    group_by(patientid, trt) |> 
    arrange(ncycle, .by_group = TRUE) |> 
    ungroup()
  
  ae <- ae |> 
    mutate(
      across(4:ncol(ae), \(x) {
        
        x <- ifelse(is.na(x), '0-1', x)
        
        factor(x, levels = c('0-1', '2', '3', '4', '5'))
        
      })
    )
  
  ae
  
})

temp <- ae |> 
  filter(trt == 'Anastrozole') |> 
  select(patientid, trt, ncycle, ARTHRL) |> 
  pivot_wider(names_from = ncycle, values_from = ARTHRL) |> 
  make_long(3:16)

library(ggsankey)
library(dplyr)
library(ggplot2)

df <- mtcars %>%
  make_long(cyl, vs, am, gear, carb)

ggplot(temp, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey()



