calculate_ti <- function(x) {
  
  x <- sort(x, decreasing = T)
  ti <- NULL
  
  for (i in 1:length(x)) {
    ti[i] <- x[i] * prod((x[1:i-1] + 1)^-1)
  }
  
  return(sum(ti))
}

make_patient_ae_sankey_results <- \(data, selected_treatment, selected_ae, selected_timeframe, selected_grade) {
  
  
  # selected_treatment <- 'Anastrozole'
  # selected_ae <- 'Arthralgia (joint pain)'
  # selected_timeframe <- c(6, 30)
  # selected_grade <- '3'
  
  response_order <- c(
    '0-1',
    '2',
    '3',
    '4',
    '5',
    'Off Treatment'
  )
  
  data <- local({
    data <- data |>
      mutate(ncycle = ncycle * 6) |> 
      mutate(ncycle_label = glue::glue('{ncycle} M'))
    
    data <- data |>
      filter(ncycle <= 60)
    
    data <- data |>
      filter(trt == selected_treatment) |>
      group_by(patientid) |>
      filter(
        any(
          ncycle == selected_timeframe[1] &
            ae == selected_ae &
            ae_grade == selected_grade
        )
      ) |>
      ungroup() |>
      filter(
        ncycle >= selected_timeframe[1] &
          ncycle <= selected_timeframe[2]
      ) |>
      filter(ae == selected_ae)
    
  })
  
  if(nrow(data) == 0) {
    return(FALSE)
  }
  
  ti_hist <- local({
    plot_data <- data %>%
      filter(ae_grade != 'Off Treatment') %>%
      group_by(patientid) %>%
      mutate(grade = case_when(ae_grade == '0-1' ~ '0',
                               TRUE ~ ae_grade),
             grade = as.numeric(grade))  %>%
      summarise(ti = calculate_ti(grade)) |> 
      ungroup()
    
    stats <- plot_data |> 
      skimr::skim(ti) |> 
      as_tibble() |> 
      mutate(across(numeric.mean:numeric.p100, \(x) scales::label_number(.01)(x)))

    plot <- ggplot(plot_data, aes(x = ti)) +
      geom_histogram() +
      theme_bw(base_size = 15) +
      scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, .5)) +
      labs(
        x = 'Toxicity Index (TI)',
        y = 'n',
        caption = glue::glue(
          'Mean (SD): {stats$numeric.mean} ({stats$numeric.sd})\nMedian [IQR]: {stats$numeric.p50} [{stats$numeric.p25}, {stats$numeric.p75}]'
        )
      )
    
    plot
    
  })

  grade_duration <- local({
    
    plot_data <- data %>%
      filter(
        ae_grade != 'Off Treatment'
      ) %>%
      group_by(ae_grade, ncycle) %>%
      count() %>%
      mutate(
        ncycle = as_factor(ncycle),
        ae_grade = paste0('Grade ', ae_grade)
      ) %>%
      ungroup() |> 
      mutate(
        label = glue::glue('{ae_grade} - {ncycle} M')
      )
    
    ggplot(plot_data, aes(x = n, y = reorder(label, n))) + 
      geom_col() + 
      theme_bw(base_size = 15) + 
      labs(x = 'n', y = NULL)
  })
  
  
  ##### need error handling on whether there is any data
  
  total_patients <- data |>
    pull(patientid) |>
    unique() |>
    length()
  
  sankey <- local({
    
    plot_data <- data |>
      select(-ncycle) |>
      pivot_wider(names_from = ncycle_label, values_from = ae_grade,
                  values_fill = 'Off Treatment')
    
    plot_data <- plot_data |>
      make_long(4:ncol(plot_data))
    
    
    cbf_1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    
    grade_colors <- c(
      "0-1" = "#999999",
      "2" = "#E69F00",
      "3" = "#56B4E9",
      "4" = "#009E73",
      "5" = "#D55E00",
      "Off Treatment" = "#000000"
    )
    
    plot <- ggplot(
      plot_data,
      aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = factor(node)
      )
    ) +
      geom_sankey(flow.alpha = .3) +
      theme_sankey(base_size = 20) +
      theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.title = element_blank()
      ) +
      scale_color_manual(values = grade_colors) +
      scale_fill_manual(values = grade_colors) +
      labs(
        title = selected_ae,
        subtitle = glue::glue('{selected_treatment}, n = {total_patients}')
      )
    
    plot
    
  })
  
  summary_description <- local({
    res <- data |>
      filter(ncycle == selected_timeframe[2]) |>
      count(ae_grade) |>
      mutate(ae_grade = factor(ae_grade, levels = response_order)) |>
      arrange(ae_grade) |> 
      complete(ae_grade, fill = list(n = 0)) |> 
      mutate(n = case_when(ae_grade == 'Off Treatment' ~ total_patients - sum(n),
                           .default = n)) |> 
      mutate(
        prop = n / sum(n),
        prop = scales::percent_format()(prop)
      )
    
    res_ae_grade <- res |> 
      slice(1:5)
    
    res_off_treatment <- res |> 
      slice(6)

    summary_res_ae_grade <- glue::glue_data(res_ae_grade, 'A total of {n} ({prop}) of the patients responded "{ae_grade}". ') |>
      glue::glue_collapse(sep = '<br>')
    
    summary_res_off_treatment <- glue::glue_data(res_off_treatment, 'Finally, a total of {n} ({prop}) of the patients were OFF the trial".')

    summary_description <- glue::glue(
      'Among a total of {total_patients} patients who responded "{selected_grade}" for "{selected_ae}" at {selected_timeframe[1]} M.',
      'By the {selected_timeframe[2]} M timepoint',

      '{summary_res_ae_grade}',
      
      '{summary_res_off_treatment}',
      .sep = '<br><br>'
    )
  })
  
  tibble::lst(
    sankey,
    summary_description,
    ti_hist,
    grade_duration
  )
  
}

