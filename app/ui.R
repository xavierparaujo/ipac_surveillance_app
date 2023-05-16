dashboardPage(
  title = "IPACX",
  skin = "black",
  dashboardHeader(title = tags$img(src='logo.png', height='35', width='140')),
  dashboardSidebar(
    sidebarMenu(
      menuItem("COVID", tabName = "covid", icon = icon("th")),
      menuItem("CDI", tabName = "cdi", icon = icon("th")),
      menuItem("MRSA", tabName = "mrsa", icon = icon("th")),
      menuItem("ESBL", tabName = "esbl", icon = icon("th")),
      menuItem("VRE", tabName = "vre", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.png"),
      tags$style(HTML("
        /* Fix the header */
        .main-header {
          position: fixed;
          width: 100%;
        }
        /* Fix the main sidebar */
        .main-sidebar {
          position: fixed;
        }
        /* Fix the additional sidebar */
        .fixed-sidebar {
          position: fixed;
          width: 10%;
          height: 100%;
          overflow-y: auto;
          padding-top: 50px;
          padding-left: 10px; /* Adjust this value based on the width of your main sidebar */
        }
        /* Set main content to be scrollable */
        .scrollable-content {
          width: 85%;
          margin-left: 15%;
          padding-left: 0px; /* Adjust this value based on the width of your main sidebar */
          height: 100%;
          overflow-y: auto;
          padding-top: 50px;
        }
      "))
    ),
    tabItems(
      # First tab content
      covid_page,
      cdi_page,
      mrsa_page,
      esbl_page,
      vre_page
    )
  )
)
