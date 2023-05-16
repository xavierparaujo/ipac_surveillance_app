#### Home UI ####

cdi_page <<- tabItem(
  tabName = "cdi", 
  div(class = "fixed-sidebar",
    checkboxGroupInput(
      'cdi_facilities', 
      'Facilities', 
      choices = FACILITIES, 
      selected = FACILITIES
    ),
    checkboxGroupInput(
      'cdi_classifications', 
      'Classifications', 
      choices = c('HA-Your', 'HA-Another'), 
      selected = c('HA-Your', 'HA-Another')
    )
  ),
  div(class = "scrollable-content",
    box( 
      title = "CDI Stacked Trigger Plot", 
      status = "primary", 
      solidHeader = FALSE,
      collapsible = TRUE,
      width = 12,
      plotlyOutput('cdi_trigger_plt')
    ),
    box( 
      title = "CDI Rates Plot", 
      status = "primary", 
      solidHeader = FALSE,
      collapsible = TRUE,
      width = 12,
      plotlyOutput('cdi_rates_plt')
    ),
    box( 
      title = "CDI Bioburden Plot", 
      status = "primary", 
      solidHeader = FALSE,
      collapsible = TRUE,
      width = 12,
      plotlyOutput('cdi_bioburden_plt')
    )
  )       
)


#### Home Server ####
cdi_trigger_server <<- function(cdi_acquired_df){
  
  cdi_trigger_df = cdi_acquired_df %>%
    group_by(fiscal_period, classification) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_period, classification, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(classification != 'CA' & grepl('2021|2022', fiscal_period))
  
  average = cdi_trigger_df %>%
    summarise(Average = round(mean(cases), 2))
  
  trigger = cdi_trigger_df %>%
    summarise(Trigger = round(2*sd(cases) + average$Average, 2))
  
  cdi_trigger_plt = cdi_trigger_df %>%
    ggplot(aes(x = fiscal_period, y = cases, fill = classification)) +
    geom_bar(stat = 'identity') +
    geom_hline(aes(yintercept = average$Average), color = "#a7c957", linetype = "dashed") +
    annotate("text", x = '2021-02', y = average$Average, label = glue('Average: {average$Average}'), hjust = 0, vjust = -1) +
    geom_hline(aes(yintercept = trigger$Trigger), color = "#bc4749", linetype = "dashed") +
    annotate("text", x = '2021-02', y = trigger$Trigger, label = glue('Trigger: {trigger$Trigger}'), hjust = 0, vjust = -1) +
    labs(x = "Fiscal Period", y = "Cases", fill = "Classification") +
    scale_fill_manual(values = c("#6a994e", "#386641")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(cdi_trigger_plt) %>% layout(legend = list(orientation = "h", y = -0.3)))
  
}


#### Home Server ####
cdi_rates_server <<- function(cdi_acquired_df, cdi_pt_days_df){
  
  cdi_rates_df = cdi_acquired_df %>%
    group_by(fiscal_quarter) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_quarter, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(grepl('2021|2022', fiscal_quarter)) %>%
    left_join(cdi_pt_days_df) %>%
    mutate(rate = round(1000*(cases/pt_days), 2))
  
  cdi_rates_plt = cdi_rates_df %>%
    ggplot(aes(x = fiscal_quarter, y = rate)) +
    geom_line(group = 1, color = "#386641") +
    geom_hline(aes(yintercept = 3), color = "#a7c957", linetype = "dashed") +
    annotate("text", x = '2021-Q1', y = 3, label = 'CDI Rate Target: 3.0', hjust = 0, vjust = -1) +
    labs(x = "Fiscal Quarter", y = "Rate") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(cdi_rates_plt))
  
}

cdi_bioburden_server <<- function(cdi_collected_df){
  
  cdi_bioburden_df = cdi_collected_df %>%
    group_by(fiscal_period, classification) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_period, classification, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(grepl('2021|2022', fiscal_period))
  
  cdi_bioburden_plt = cdi_bioburden_df %>%
    ggplot(aes(x = fiscal_period, y = cases, fill = classification)) +
    geom_bar(stat = 'identity') +
    labs(x = "Fiscal Period", y = "Cases", fill = "Classification") +
    scale_fill_manual(values = c("#6a994e", "#386641", "#fabc3c")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(cdi_bioburden_plt) %>% layout(legend = list(orientation = "h", y = -0.3)))
  
}
