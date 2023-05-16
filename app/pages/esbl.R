#### Home UI ####

esbl_page <<- tabItem(
  tabName = "esbl", 
  div(class = "fixed-sidebar",
      checkboxGroupInput(
        'esbl_facilities', 
        'Facilities', 
        choices = FACILITIES, 
        selected = FACILITIES
      ),
      checkboxGroupInput(
        'esbl_classifications', 
        'Classifications', 
        choices = c('HA-Your', 'HA-Another'), 
        selected = c('HA-Your', 'HA-Another')
      )
  ),
  div(class = "scrollable-content",
      box( 
        title = "ESBL Stacked Trigger Plot", 
        status = "primary", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput('esbl_trigger_plt')
      ),
      box( 
        title = "ESBL Rates Plot", 
        status = "primary", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput('esbl_rates_plt')
      ),
      box( 
        title = "ESBL Bioburden Plot", 
        status = "primary", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput('esbl_bioburden_plt')
      )
  )       
)


#### Home Server ####
esbl_trigger_server <<- function(esbl_acquired_df){
  
  esbl_trigger_df = esbl_acquired_df %>%
    group_by(fiscal_period, classification) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_period, classification, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(classification != 'CA' & grepl('2021|2022', fiscal_period))
  
  average = esbl_trigger_df %>%
    summarise(Average = round(mean(cases), 2))
  
  trigger = esbl_trigger_df %>%
    summarise(Trigger = round(2*sd(cases) + average$Average, 2))
  
  esbl_trigger_plt = esbl_trigger_df %>%
    ggplot(aes(x = fiscal_period, y = cases, fill = classification)) +
    geom_bar(stat = 'identity') +
    geom_hline(aes(yintercept = average$Average), color = "#a7c957", linetype = "dashed") +
    annotate("text", x = '2021-02', y = average$Average, label = glue('Average: {average$Average}'), hjust = 0, vjust = -1) +
    geom_hline(aes(yintercept = trigger$Trigger), color = "#bc4749", linetype = "dashed") +
    annotate("text", x = '2021-02', y = trigger$Trigger, label = glue('Trigger: {trigger$Trigger}'), hjust = 0, vjust = -1) +
    labs(x = "Fiscal Period", y = "Cases", fill = "Classification") +
    scale_fill_manual(values = c("#f487b6", "#cc59d2")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(esbl_trigger_plt) %>% layout(legend = list(orientation = "h", y = -0.3)))
  
}


#### Home Server ####
esbl_rates_server <<- function(esbl_acquired_df, esbl_pt_days_df){
  
  esbl_rates_df = esbl_acquired_df %>%
    group_by(fiscal_quarter) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_quarter, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(grepl('2021|2022', fiscal_quarter)) %>%
    left_join(esbl_pt_days_df) %>%
    mutate(rate = round(1000*(cases/pt_days), 2))
  
  esbl_rates_plt = esbl_rates_df %>%
    ggplot(aes(x = fiscal_quarter, y = rate)) +
    geom_line(group = 1, color = "#cc59d2") +
    geom_hline(aes(yintercept = 3), color = "#a7c957", linetype = "dashed") +
    annotate("text", x = '2021-Q1', y = 3, label = 'MRSA Rate Target: 3.0', hjust = 0, vjust = -1) +
    labs(x = "Fiscal Quarter", y = "Rate") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(esbl_rates_plt))
  
}

esbl_bioburden_server <<- function(esbl_collected_df){
  
  esbl_bioburden_df = esbl_collected_df %>%
    group_by(fiscal_period, classification) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_period, classification, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(grepl('2021|2022', fiscal_period))
  
  esbl_bioburden_plt = esbl_bioburden_df %>%
    ggplot(aes(x = fiscal_period, y = cases, fill = classification)) +
    geom_bar(stat = 'identity') +
    labs(x = "Fiscal Period", y = "Cases", fill = "Classification") +
    scale_fill_manual(values = c("#f487b6", "#cc59d2", "#fabc3c")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(esbl_bioburden_plt) %>% layout(legend = list(orientation = "h", y = -0.3)))
  
}
