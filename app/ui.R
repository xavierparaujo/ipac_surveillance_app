dashboardPage(
  dashboardHeader(title = "IPAC Surveillance App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("CDI", tabName = "cdi", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home", home),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)