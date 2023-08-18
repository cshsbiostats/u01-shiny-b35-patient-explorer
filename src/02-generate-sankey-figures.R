library(tidyverse)
library(ggsankey)
library(patchwork)

data <- read_csv(here::here('data/u01_b35_data.csv'))

make_sankey_diagram <- \(data, trt, ae, cycle_limit = 10) {
  
  data <- data |> 
    filter(ncycle <= !!cycle_limit)
  
  data <- data |> 
    filter(trt == !!trt & ae == !!ae)
  
  total_patients <- data |> 
    filter(ncycle == 1) |> 
    pull(patientid) |> 
    unique() |> 
    length()
  
  data <- data |> 
    mutate(
      ncycle = ncycle * 6,
      ncycle = glue::glue('{ncycle} M')
    )
  
  data <- data |> 
    pivot_wider(names_from = ncycle, values_from = ae_grade,
                values_fill = 'Off Treatment')
  
  plot_data <- data |> make_long(4:13)
  
  grade_colors <- c(
    "0-1" = "#1F77B4FF",
    "2" = "#2CA02CFF",
    "3" = "#BCBD22FF",
    "4" = "#FF7F0EFF",
    "5" = "#D62728FF",
    "Off Treatment" = "#7F7F7FFF"
  )
  
  plot <- ggplot(plot_data,
         aes(
           x = x,
           next_x = next_x,
           node = node,
           next_node = next_node,
           fill = factor(node)
         )) +
    geom_sankey(flow.alpha = .3) +
    theme_sankey(base_size = 20) +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          axis.text.x = element_text(face = 'bold'),
          axis.title = element_blank()) +
    scale_color_manual(
      values = grade_colors
    ) + 
    scale_fill_manual(
      values = grade_colors
    ) + 
    labs(title = ae, subtitle = glue::glue('{trt}, n = {total_patients}'))
  
  plot
  
}

make_sankey_diagram(data, trt = 'Tamoxifen', ae = 'Arthralgia (joint pain)')
