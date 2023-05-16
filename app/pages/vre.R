#### Home UI ####

vre_page <<- tabItem(
  tabName = "vre", 
  div(class = "fixed-sidebar",
      checkboxGroupInput(
        'vre_facilities', 
        'Facilities', 
        choices = FACILITIES, 
        selected = FACILITIES
      ),
      checkboxGroupInput(
        'vre_classifications', 
        'Classifications', 
        choices = c('HA-Your', 'HA-Another'), 
        selected = c('HA-Your', 'HA-Another')
      )
  ),
  div(class = "scrollable-content",
      box( 
        title = "VRE Stacked Trigger Plot", 
        status = "primary", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput('vre_trigger_plt')
      ),
      box( 
        title = "VRE Rates Plot", 
        status = "primary", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput('vre_rates_plt')
      ),
      box( 
        title = "VRE Bioburden Plot", 
        status = "primary", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput('vre_bioburden_plt')
      )
  )       
)


#### Home Server ####
vre_trigger_server <<- function(vre_acquired_df){
  
  vre_trigger_df = vre_acquired_df %>%
    group_by(fiscal_period, classification) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_period, classification, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(classification != 'CA' & grepl('2021|2022', fiscal_period))
  
  average = vre_trigger_df %>%
    summarise(Average = round(mean(cases), 2))
  
  trigger = vre_trigger_df %>%
    summarise(Trigger = round(2*sd(cases) + average$Average, 2))
  
  vre_trigger_plt = vre_trigger_df %>%
    ggplot(aes(x = fiscal_period, y = cases, fill = classification)) +
    geom_bar(stat = 'identity') +
    geom_hline(aes(yintercept = average$Average), color = "#a7c957", linetype = "dashed") +
    annotate("text", x = '2021-02', y = average$Average, label = glue('Average: {average$Average}'), hjust = 0, vjust = -1) +
    geom_hline(aes(yintercept = trigger$Trigger), color = "#bc4749", linetype = "dashed") +
    annotate("text", x = '2021-02', y = trigger$Trigger, label = glue('Trigger: {trigger$Trigger}'), hjust = 0, vjust = -1) +
    labs(x = "Fiscal Period", y = "Cases", fill = "Classification") +
    scale_fill_manual(values = c("#9d4edd", "#5a189a")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(vre_trigger_plt) %>% layout(legend = list(orientation = "h", y = -0.3)))
  
}


#### Home Server ####
vre_rates_server <<- function(vre_acquired_df, vre_pt_days_df){
  
  vre_rates_df = vre_acquired_df %>%
    group_by(fiscal_quarter) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_quarter, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(grepl('2021|2022', fiscal_quarter)) %>%
    left_join(vre_pt_days_df) %>%
    mutate(rate = round(1000*(cases/pt_days), 2))
  
  vre_rates_plt = vre_rates_df %>%
    ggplot(aes(x = fiscal_quarter, y = rate)) +
    geom_line(group = 1, color = "#5a189a") +
    geom_hline(aes(yintercept = 3), color = "#a7c957", linetype = "dashed") +
    annotate("text", x = '2021-Q1', y = 3, label = 'MRSA Rate Target: 3.0', hjust = 0, vjust = -1) +
    labs(x = "Fiscal Quarter", y = "Rate") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(vre_rates_plt))
  
}

vre_bioburden_server <<- function(vre_collected_df){
  
  vre_bioburden_df = vre_collected_df %>%
    group_by(fiscal_period, classification) %>%
    summarise(cases = n(), .groups = 'drop') %>%
    complete(fiscal_period, classification, fill = list(cases = 0)) %>%
    ungroup() %>%
    filter(grepl('2021|2022', fiscal_period))
  
  vre_bioburden_plt = vre_bioburden_df %>%
    ggplot(aes(x = fiscal_period, y = cases, fill = classification)) +
    geom_bar(stat = 'identity') +
    labs(x = "Fiscal Period", y = "Cases", fill = "Classification") +
    scale_fill_manual(values = c("#9d4edd", "#5a189a", "#fabc3c")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(ggplotly(vre_bioburden_plt) %>% layout(legend = list(orientation = "h", y = -0.3)))
  
}
