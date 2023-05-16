# Define server logic required to draw a histogram
function(input, output, session) {
  
  #### covid ####
  covid_reactive_acquired_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'COVID' & acquired_facility %in% input$covid_facilities & classification %in% input$covid_classifications)
  })
  
  covid_reactive_collected_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'COVID' & collected_facility %in% input$covid_facilities)
  })
  
  covid_reactive_pt_days_df = reactive({
    covid_pt_days_df = PT_DAYS %>%
      filter(collected_facility %in% input$covid_facilities) %>%
      group_by(fiscal_quarter) %>%
      summarise(pt_days = sum(pt_days)) %>%
      ungroup()
  })
  
  output$covid_trigger_plt = renderPlotly({covid_trigger_server(covid_reactive_acquired_df())})
  
  output$covid_rates_plt = renderPlotly({covid_rates_server(covid_reactive_acquired_df(), covid_reactive_pt_days_df())})
  
  output$covid_bioburden_plt = renderPlotly({covid_bioburden_server(covid_reactive_collected_df())})
  
  #### CDI ####
  cdi_reactive_acquired_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'CDI' & acquired_facility %in% input$cdi_facilities & classification %in% input$cdi_classifications)
  })
  
  cdi_reactive_collected_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'CDI' & collected_facility %in% input$cdi_facilities)
  })
  
  cdi_reactive_pt_days_df = reactive({
   cdi_pt_days_df = PT_DAYS %>%
      filter(collected_facility %in% input$cdi_facilities) %>%
      group_by(fiscal_quarter) %>%
      summarise(pt_days = sum(pt_days)) %>%
      ungroup()
  })
  
  output$cdi_trigger_plt = renderPlotly({cdi_trigger_server(cdi_reactive_acquired_df())})
  
  output$cdi_rates_plt = renderPlotly({cdi_rates_server(cdi_reactive_acquired_df(), cdi_reactive_pt_days_df())})
  
  output$cdi_bioburden_plt = renderPlotly({cdi_bioburden_server(cdi_reactive_collected_df())})
  
  
  #### MRSA ####
  mrsa_reactive_acquired_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'MRSA' & acquired_facility %in% input$mrsa_facilities & classification %in% input$mrsa_classifications)
  })
  
  mrsa_reactive_collected_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'MRSA' & collected_facility %in% input$mrsa_facilities)
  })
  
  mrsa_reactive_pt_days_df = reactive({
    mrsa_pt_days_df = PT_DAYS %>%
      filter(collected_facility %in% input$mrsa_facilities) %>%
      group_by(fiscal_quarter) %>%
      summarise(pt_days = sum(pt_days)) %>%
      ungroup()
  })
  
  output$mrsa_trigger_plt = renderPlotly({mrsa_trigger_server(mrsa_reactive_acquired_df())})
  
  output$mrsa_rates_plt = renderPlotly({mrsa_rates_server(mrsa_reactive_acquired_df(), mrsa_reactive_pt_days_df())})
  
  output$mrsa_bioburden_plt = renderPlotly({mrsa_bioburden_server(mrsa_reactive_collected_df())})
  
  #### ESBL ####
  esbl_reactive_acquired_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'ESBL' & acquired_facility %in% input$esbl_facilities & classification %in% input$esbl_classifications)
  })
  
  esbl_reactive_collected_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'ESBL' & collected_facility %in% input$esbl_facilities)
  })
  
  esbl_reactive_pt_days_df = reactive({
    esbl_pt_days_df = PT_DAYS %>%
      filter(collected_facility %in% input$esbl_facilities) %>%
      group_by(fiscal_quarter) %>%
      summarise(pt_days = sum(pt_days)) %>%
      ungroup()
  })
  
  output$esbl_trigger_plt = renderPlotly({esbl_trigger_server(esbl_reactive_acquired_df())})
  
  output$esbl_rates_plt = renderPlotly({esbl_rates_server(esbl_reactive_acquired_df(), esbl_reactive_pt_days_df())})
  
  output$esbl_bioburden_plt = renderPlotly({esbl_bioburden_server(esbl_reactive_collected_df())})
  
  #### VRE ####
  vre_reactive_acquired_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'VRE' & acquired_facility %in% input$vre_facilities & classification %in% input$vre_classifications)
  })
  
  vre_reactive_collected_df = reactive({
    INFECTION_DB %>%
      filter(infection_type == 'VRE' & collected_facility %in% input$vre_facilities)
  })
  
  vre_reactive_pt_days_df = reactive({
    vre_pt_days_df = PT_DAYS %>%
      filter(collected_facility %in% input$vre_facilities) %>%
      group_by(fiscal_quarter) %>%
      summarise(pt_days = sum(pt_days)) %>%
      ungroup()
  })
  
  output$vre_trigger_plt = renderPlotly({vre_trigger_server(vre_reactive_acquired_df())})
  
  output$vre_rates_plt = renderPlotly({vre_rates_server(vre_reactive_acquired_df(), vre_reactive_pt_days_df())})
  
  output$vre_bioburden_plt = renderPlotly({vre_bioburden_server(vre_reactive_collected_df())})
  
}
