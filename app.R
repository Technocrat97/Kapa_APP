library(shiny)
library(shinyauthr)
library(shinydashboard)
library(shinyjs)
library(DBI)
library(RSQLite)
library(shinyFeedback)
library(shinyWidgets)
library(shinythemes)


db <- dbConnect(RSQLite::SQLite(), dbname = "SWM-Datenbank.db")

idtype <- dbGetQuery(db, 'SELECT distinct Kostenberechnung_ID from Kostenberechnung_all')
#abteilungstype <- dbGetQuery(db, 'SELECT distinct  from ')



# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
    user = c("user1", "user2"),
    password = c("pass1", "pass2"),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
)



# get distinct values 

table_intro <- list(Kostenberechnung_all = "KAPA Datensatz", Kostenberechnung = "Test",
                
                    other = "Hier zu finden sind die ...")






ui <- dashboardPage(
  dashboardHeader(
    
    title = "KAPA-App",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout")
    )
    
    
    
    #evtl hier weiteren Link einpflegen
    
    
    # tags$li(
    #   class = "dropdown",
    #   tags$a(
    #     icon("github"),
    #     href = "https://github.com/paulc91/shinyauthr",
    #     title = "See the code on github"
    #   )
    # )
    
  ), # header
  
  dashboardSidebar(
    collapsed = TRUE, 
    div(htmlOutput("welcome"), style = "padding: 20px"),
    sidebarMenu(
      menuItem("View Tables", tabName = "view_table", icon = icon("search")),
      menuItem("Create Tables", tabName = "create_table", icon = icon("plus-square")),
      menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
      menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
      menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
      menuItem("View Plot", tabName = "view_plot", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )),  # sidebar
  
  
  
  
  
  dashboardBody(
   
    
    ## login start
    shinyauthr::loginUI(  
      "login",
      additional_ui = tagList(
        tags$p("test the different outputs from the sample logins below
               as well as an invalid login attempt.", class = "text-center"),
        HTML(knitr::kable(user_base[, -3], format = "html", table.attr = "style='width:100%;'"))
      ) # additional_ui
    ),
    uiOutput("testUI")
    
, 
tabItems(
  tabItem(tabName = "view_table", uiOutput("tab1UI")),
  tabItem(tabName = "del_table", uiOutput("tab2UI")),
  tabItem(tabName = "update_table", uiOutput("tab3UI")),
  tabItem(tabName = "create_table", uiOutput("tab4UI")),
  tabItem(tabName = "insert_value", uiOutput("tab5UI")),
  tabItem(tabName = "view plot", uiOutput("tab7UI")),
  tabItem(tabName = "about", uiOutput("tab6UI"))
)
    
    
    
  ) # body
  
  
  
  
) # dashboardpage









server <- function(input, output, session) {
  
  
  
  
  #################### Authentification Sektion
  
  user_info <- reactive({credentials()$info})
  
  output$welcome <- renderText({ 
    req(credentials()$user_auth)
    paste("Welcome ","<font color=\"#f3b404\"><b>", {user_info()$permissions}, "</b></font>","!") 
  })
  
  
  
  
  # We can set the sidebar to be collapsed by default and to be expanded after user authorization.
observe({
  if(credentials()$user_auth) {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  } else {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  }
})

  
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # Logout Funktion 
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  
  #################### Authentification Sektion -E
  
  
  
  
  
  
  
  
  
  # Tab 1 view
  
  output$tab1UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            box(),
            selectInput(
              inputId = "sel_table_1",
              label = "Tables in Database",
              choices = dbListTables(db),
              selected = "Kostenberechnung_all"
            ),
            textOutput(outputId = "tab_intro"),
            tags$head(tags$style("#tab_intro{
                               font-size: 15px;
                               font-style: italic;
                               }"))
          ),
          mainPanel(
                      h4(strong("Table Preview")),
                      dataTableOutput(outputId = "sel_table_view")
                    )
                  )
              )
            })
            
  
  

  # output$tab1UI <- renderUI({
  #   box(width = NULL, status = "primary",
  #       sidebarLayout(
  #         sidebarPanel(
  #           box(width = 12,
  #               collapsible = TRUE,
  #               div(style = "height: 15px; background-color: white;"),
  #               title = "Database Info:",
  #               p("")),
  #           selectInput(),
  #           textOutput(outputId = "tab_intro"),
  #           tags$head(tags$style("#tab_intro{font-size: 15px;font-style: italic;}"))
  #         ),
  #         mainPanel(
  #           h4(strong("Table Preview")),
  #           dataTableOutput(outputId = "sel_table_view")
  #         )
  #       )
  #   )
  # })
  
  
  
  ## f nicht
  
  # output$sel_table_view <- renderDataTable()
   # 
   output$tab_intro <- renderText(
    if (input$sel_table_1 %in% c("Kostenberechnnung_all","Kostenberechnung"))
    {table_intro[[input$sel_table_1]]}
     else {table_intro$other})
  
  
 # f nicht
  
  
  
  
  ## tab 1 view -E
  
  
  
  
  # Tab 2 create
  
  
  
  
  
  
  
  # Tab 2 -E create -E
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Tab 3 Update
  
  
  output$tab3UI <- renderUI({
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = "Note:", "")
      ),
      fluidRow(
        box(title = "Rename Table", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel(
              textInput(),
              actionButton())
        ),
        box(title = "Rename Column", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        ),
        box(title = "Add Column", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        )
      )
    )
  })
  
  
  
  # F
  
  updateSelectInput(session, "sel_table_1", choices = dbListTables(db))
 #  
  for (sel_input in c("sel_table_2","sel_table_3","sel_table_3_i","sel_table_3_ii","sel_table_5")){
    updateSelectInput(session, sel_input, choices = setdiff(dbListTables(db),
                                                            c("Kostenberechnung_all", "Kostenberechnung")))
  }

 updateSelectInput(session, "sel_col_3", choices = colnames(d))
 
  
  
  
  # Tab 3 update -E
  
  
  
  
  ## insert
  
  
  output$values <- renderUI({
    # UI outputs rendered in the place that has an uiOutput id "values"
    
    req(isTruthy(input$sel_table_5))
    
    values <- list()
    d <- dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_5)
    )
    typ <- dbGetQuery(
      conn = db, statement = paste0('PRAGMA table_info(',input$sel_table_5,')')
    )
    for (col in colnames(d)) {
      typ_i = typ$type[typ$name==col]
      values[[col]] <- box(
        title = paste0(as.character(col),' (',typ_i,')'), 
        width = 6, solidHeader = TRUE, status = "primary",
        
        if (typ_i == "BOOLEAN") {radioButtons(inputId = paste0("value_", col), label = "Value",
                                              c("TRUE","FALSE") )}
        else if (typ_i == "NUMERIC" | typ_i == "FLOAT" |
                 typ_i == "INTEGER" | typ_i == "NUM" ) 
        {numericInput(inputId = paste0("value_", col), label = "Value", value = 0)} 
        else if (typ_i == "DATE") {dateInput(inputId = paste0("value_", col),
                                             label = "Value",
                                             value = "2020-12-01") }
        else {tagList(useShinyFeedback(),
                      textInput(inputId = paste0("value_", col), label = "Value"))}
      )
    }
    values
  })
  
  
  
  
  
  ## insert -E
  
  
  
  
  
  # Tab 4
  
  
  output$tab4UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Table name"),
        numericInput(inputId = "ncols", label = "Number of columns", 1, min = 4),
        uiOutput(outputId = "cols"),
        actionButton(inputId = "create_table", label = "Create table", class = "btn-info", style = "")
    )
  })
  
  
  
  output$cols <- renderUI({
    req(input$ncols >= 1)
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Column", i), width = 6, solidHeader = TRUE, status = "primary",
        textInput(inputId = paste0("colName", i), label = "Column name"),
        selectInput(inputId = paste0("colType", i), label = "Column type", 
                    choices = c("NUMERIC", "VARCHAR(255)","BOOLEAN","DATE")
        )
      )
    }
    cols
  })
  
  
  
  # Tab 4 -E
  
  
  

  # Tab 5 insert
  
  output$tab5UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Table name"),
        numericInput(inputId = "ncols", label = "Number of columns", 1, min = 1),
        uiOutput(outputId = "cols"),
        actionButton(inputId = "create_table", label = "Create table", class = "btn-info", style = "")
    )
  })
  
  
  # Tab 5
  
  
  
  
  
  
  
  
  # Tab 6 Plot
  
  
 ' output$my_table <- renderTable({
    Ergebnisse als Datensatz anzeigen
    results'
  
  
  
  
  
  # Tab 6 -E
  
  
  
  
  
  # Tab 7 About
  
  
  
  output$tab6UI <- renderText({
    HTML("<h1>Willkommen auf der KAPA Webseite!</h1>
         <p>Hier finden Sie Informationen und Tipps zu verschiedenen Funktionen</p>
         <p> </p>
         <p> </p>
         <h2>Funktionsweise::</h2>
         <p>Diese App bietet dem Nutzer die Möglichkeit, einen Austausch mit einer Datenbank 
         über eine Nutzerfreundliche Oberfläche zu ermöglichen </p>"
           
           
           
           )
  })
  
  
  
  
  
  # Tab 7
  
  
  
  
  ################################################## Fehlervermeidung Sektion
  
  
  # if (tolower(input$table_name) %in% tolower(dbListTables(db)) |
  #     !isTruthy(input$table_name) |
  #     grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$table_name) == FALSE) {
  #   showModal(modalDialog(
  #     title = "Invalid table name",
  #     "You get this message possibly because:
  #   1) the table already exists;
  #   2) the table name is blank;
  #   or 3) this is an invalid table name.",
  #     footer = modalButton("OK"), easyClose = TRUE ) )
  #   return()
  # }
  # 
  # 
  # 
  # # make sure there is value in the input 
  # if (!isTruthy(input$ncols)) {
  #   showModal(modalDialog(
  #     title = "Invalid table name",
  #     "Please type in the right column number.",
  #     footer = modalButton("OK"), easyClose = TRUE ) )
  # }
  # 
  # # make sure the input value of column number is larger than one  
  # if (input$ncols < 1) {
  #   showModal(modalDialog(
  #     title = "No columns",
  #     "Each table must have one or more columns.",
  #     footer = modalButton("OK"), easyClose = TRUE
  #   )) 
  # }
  # 
  # 
  # gather all the colnames into a list
  # col_names_list = list()
#for (i in seq_len(input$ncols)) {
 #  col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
# }
  # 
  # # make sure the column name is valid  
  # if ( any(col_names_list == "") | 
  #      sum(duplicated(col_names_list)) > 0 |
  #      any(grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",col_names_list) == FALSE) |
  #      any(tolower(col_names_list) %in% sqlite_kw_lo) ) {
  #   showModal(modalDialog(
  #     title = "Invalid column name",
  #     "You get this message possibly because: 
  #     1) the column name already exists;
  #     2) the field is blank;
  #     3) this is an invalid SQLite column name;
  #     or 4) the field name conflicts with a SQLite keyword.",
  #     footer = modalButton("OK"), easyClose = TRUE
  #   ))
  #   return()
  # }
  
  ########## f nicht
  
  # Fehlermeldung -E
  
  
  
  
  
  ########################################################## SQL Sektion 
  
  ## f nicht
  
  # observeEvent(input$create_table, {
  #   # make sure table name is not the same as an existing table in the database, blank, or invalid
  #   if () {}
  #   # make sure there is value in the input 
  #   if () {}
  #   # make sure the input value of column number is larger than one  
  #   else if () {}  
  #   else {
  #     # gather all the column names into a list
  #     # make sure the column name is valid  
  #     if () {}
  #     
  #     # compile query
  #     query <- paste0('CREATE TABLE ',input$table_name,' (')
  
  #     for (i in seq_len(input$ncols)) { 
  #       query <- paste0(query,input[[paste0("colName", i)]],' ',input[[paste0("colType", i)]],',')
  #     }
  
  
  #     query <- paste0(str_sub(query,1,-2),')')
  #     dbGetQuery(
  #       conn = db,
  #       statement = query )
  #     # if successful, update inputs
  #     updateNumericInput(session, "ncols", value = "1")
  #     updateTextInput(session, "table_name", value = "")
  #     for (sel_input in c("sel_table_2","sel_table_3","sel_table_3_i","sel_table_3_ii","sel_table_5")) {
  #       updateSelectInput(session, sel_input, 
  #                         choices = setdiff(dbListTables(db),
  #                                           c("custs","order_items","orders",
  #                                             "prods_i","stores")))
  #     }
  #     updateSelectInput(session, "sel_table_1", choices = dbListTables(db))
  #     showModal(modalDialog(
  #       title = "Success",
  #       "The table has been successfully created.",
  #       footer = modalButton("OK"), easyClose = TRUE ) )
  #   }
  # }) 
  
  
  ### f nicht
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
