user_roles <- list(
  individual = Sys.getenv("USERNAME"),
  team = "Data Team"
)

required_packages <- c("shiny",
                       "shinydashboard",
                       "DT",
                       "DBI",
                       "odbc",
                       "lubridate",
                       "shinyBS",
                       "shinyjs",
                       "later")


new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)
if (exists("con") && dbIsValid(con)) {
  dbDisconnect(con)
}


glossary_df <- data.frame(
  Code = c("CSV", "ALO", "PFD", "SIT", "MED", "EMP"),
  Description = c(
    "Child Supervision",
    "Alone",
    "Personal Floatation Device",
    "Lack of Situational Awareness",
    "Medical Suspected",
    "Empty Pool"
  ),
  Notes = c(
    "Only Applicable when age is less than 14",
    "Only Applicable when age is greater than or equal to 14",
    "When PFD was a factor in death i.e. not wearing/carrying it, not removing in an appropriate situation",
    "Should not be used alongisde Alcohol (ALC)",
    "Should not be used alongisde Old Age (OLD)",
    "Home pool, or portable pool, could have been emptied"
  ),
  stringsAsFactors = FALSE
)


ui <- dashboardPage(
  ## Header
  dashboardHeader(title = "Tagging App"), 
  ## Sidebar
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "Weekly Summary",
      tabName = "weekly_summary",
      icon = icon("calendar")
    ),
    menuItem("My Audit Log", tabName = "summary_tab", icon = icon("history")),
    menuItem(
      "Holiday Summary",
      tabName = "holiday_tab",
      icon = icon("calendar")
    ),
    
    menuItem("Glossary", tabName = "glossary_tab", icon = icon("book")),
    
    if (user_roles$individual %in% c("Chris", "Stella")) {
      menuItem("All Tags", tabName = "special_tab", icon = icon("star"))
    }
  )), 
  ## Body
  dashboardBody(
    
    ## JS listener for arrow keys
    tags$script(
      HTML(
        "
      document.addEventListener('keydown', function(event) {
        if (event.keyCode === 37) {
          Shiny.setInputValue('left_arrow', Math.random());
        } else if (event.keyCode === 39) {
          Shiny.setInputValue('right_arrow', Math.random());
        }else if (event.keyCode === 38) {
          Shiny.setInputValue('up_arrow', Math.random());
        }
      });
    "
      )
    ),
    
    ## Make background cover the full screen 
    tags$head(tags$style(
      HTML(
        "
      .shiny-notification {
        font-size: 18px;
        padding: 15px;
        width: 300px;
        height: auto;
        text-align: center;
      }
      .content-wrapper,
      .right-side,
      .main-footer {
        background-color: #edf3f9 !important;
      }
      .wrapper,
      .content-wrapper {
        min-height: 100vh;
        background-color: #edf3f9 !important;
      }
      #loading-overlay {
        position: fixed;
        width: 100%;
        height: 100%;
        background: rgba(255, 255, 255, 0.9);
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 24px;
        font-weight: bold;
        color: #007bff;
      }
    "
      )
    )),
    
    useShinyjs(),
    div(id = "loading-overlay", "Loading Tagging App"),
    
    ## Define tabs
    do.call(tabItems, {
      
      ## Weekly summary of the individual user's tab
      tab_list <- list(tabItem(tabName = "weekly_summary", fluidRow(
        column(
          2,
          h2(paste0("Welcome ", user_roles$individual, "!")),
          uiOutput("drowningSummary"),
          tags$div(
            tags$label("Pick a date (Optional)"),
            tags$span(
              class = "glyphicon glyphicon-info-sign",
              id = "date_info",
              style = "cursor: pointer; margin-left: 5px;"
            ),
            bsTooltip(
              "date_info",
              "Pick a date or use arrows to move between weeks.",
              placement = "right",
              trigger = "hover"
            ),
            dateInput(
              "date",
              label = NULL,
              value = Sys.Date(),
              max = Sys.Date()
            ),
            uiOutput("locationSelect"),
            selectInput(
              "periodSelect",
              "Select Time Period:",
              choices = c("Current Year", "All Years")
            )
          ),
          tags$div(
            style = "background-color: #93c1db; padding: 15px; border-radius: 5px; margin-top: 10px;",
            tags$div(
              style = "display: inline-flex; align-items: center;",
              h3("Create a New Tag", style = "margin-right: 5px;"),
              tags$span(
                class = "glyphicon glyphicon-info-sign",
                id = "tag_info",
                style = "cursor: pointer;"
              ),
              bsTooltip(
                "tag_info",
                "Fill in the fields below and click add. Your tag will then be an option to select.",
                placement = "right",
                trigger = "hover"
              )
            ),
            textInput(
              "weekTagDescInput",
              "Give the New Tag a Description:",
              value = "",
              placeholder = "Enter description here"
            ),
            textInput(
              "weekTagIDInput",
              "Give the New Tag a Unique Code (up to three characters long):",
              value = "",
              placeholder = "Enter up to 3 characters"
            ),
            tags$script(HTML(
              "$('#weekTagIDInput').attr('maxlength', '3');"
            )),
            actionButton("weekAddLabel", "Add")
          )
        ),
        column(
          10,
          uiOutput("weeklySummaryTable"),
          actionButton("displayPreviousWeek", "\u2190 Previous Week"),
          actionButton("skipToLatestUntagged", "\u2191 Skip to Untagged Week"),
          actionButton("displayNextWeek", "Next Week \u2192")
        )
      )))
      ## Audit history of the individual user's tagging
      tab_list <- c(tab_list, list(tabItem(tabName = "summary_tab", fluidRow(
        box(
          title = "Your Tagging History",
          width = 12,
          status = "primary",
          DTOutput("auditTable")
        )
      ))))
      ## Renders the glossary tab
      tab_list <- c(tab_list, list(tabItem(tabName = "glossary_tab", fluidRow(
        box(
          title = "Glossary of Tags",
          width = 12,
          status = "primary",
          DTOutput("glossaryTable")
        )
      ))))
      ## Uses team user and shows results per holiday
      tab_list <- c(tab_list, list(tabItem(
        tabName = "holiday_tab",
        column(
          2,
          h2(paste0("Welcome, ", user_roles$team, "!")),
          uiOutput("calendarDrowningSummary"),
          uiOutput("calendarYearSelector"),
          uiOutput("calendarHoliday"),
          tags$div(
            style = "background-color: #93c1db; padding: 15px; border-radius: 5px; margin-top: 10px;",
            tags$div(
              style = "display: inline-flex; align-items: center;",
              h3("Create a New Tag", style = "margin-right: 5px;"),
              tags$span(
                class = "glyphicon glyphicon-info-sign",
                id = "tag_info",
                style = "cursor: pointer;"
              ),
              bsTooltip(
                "tag_info",
                "Fill in the fields below and click add. Your tag will then be an option to select.",
                placement = "right",
                trigger = "hover"
              )
            ),
            textInput(
              "weekTagDescInput",
              "Give the New Tag a Description:",
              value = "",
              placeholder = "Enter description here"
            ),
            textInput(
              "weekTagIDInput",
              "Give the New Tag a Unique Code (up to three characters long):",
              value = "",
              placeholder = "Enter up to 3 characters"
            ),
            tags$script(HTML(
              "$('#weekTagIDInput').attr('maxlength', '3');"
            )),
            actionButton("weekAddLabel", "Add")
            
          )
        ),
        column(10, uiOutput("calendarSummaryTable"), fluidRow(column(
          12,
          div(
            style = "display: flex; justify-content: left; align-items: center; gap: 15px; flex-wrap: wrap;",
            actionButton("calendarPrevYear", "\u2190 Previous Year"),
            
            actionButton("calendarNextYear", "Next Year \u2192")
          )
        )))
      )))
      if (user_roles$individual %in% c("Chris", "Stella")) {
        tab_list <- c(tab_list, list(tabItem(tabName = "special_tab", fluidRow(
          column(
            12,
            uiOutput("weeklySummaryTableAllTags"),
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              
              actionButton(
                "skipToEarliestAllTags",
                "\u219e Earliest Drowning",
                style = "margin-bottom: 15px"
              ),
              actionButton(
                "displayPreviousWeekAllTags",
                "\u2190 Previous Week",
                style = "margin-bottom: 15px"
              ),
              dateInput(
                "dateAllWeeks",
                label = NULL,
                value = Sys.Date(),
                max = Sys.Date()
              ),
              uiOutput("locationSelectAllTags"),
              actionButton("displayNextWeekAllTags", "Next Week \u2192", style = "margin-bottom: 15px"),
              actionButton("skipToLatestAllTags", "Latest Drowning \u21a0", style = "margin-bottom: 15px"),
              
            )
          )
        ))))
      }
      
      tab_list
    })
  )
)


server <- function(input, output, session) {
  box_style <- "flex: 3; padding: 15px; border: 4px solid #3C8DBC; border-radius: 12px; background-color: #FDFDFE;"
  arrowLock <- reactiveVal(FALSE)
  
  dbQuerySafe <- function(query) {
    tryCatch({
      con <- dbConnect(
        odbc(),
        Driver = "SQL Server",
        Server = "heimatau.database.windows.net",
        Database = "WSNZ",
        Port = 1433,
        Uid = "wsnztagger",
        Pwd = "tag_girl123"
      )
      dbGetQuery(con, query)
    }, error = function(e) {
      data.frame()
    })
  }
  
  renderDrowningSummaryCard <- function(row, tags_df, selected_tags, input_id_prefix) {
    select_input_id <- paste0(input_id_prefix, row$DrowningID)
    
    tag_html <- if (!is.null(selected_tags) && length(selected_tags) > 0) {
      paste(sapply(selected_tags, function(tag) {
        sprintf(
          "<div style='background-color: #f0f0f0; color: #333; border: 1px solid #ccc; border-radius: 10px; padding: 5px 10px; margin: 5px; display: inline-block; font-size: 16px;'>%s</div>",
          tag
        )
      }), collapse = "")
    } else {
      "<p>No tags selected.</p>"
    }
    
    eth_text <- if (!(row$SubEthnicityDesc %in% c("Unknown", "Maori", "New Zealand European"))) {
      paste0(row$EthnicityDesc, " (", row$SubEthnicityDesc, ")")
    } else {
      row$EthnicityDesc
    }
    
    tags$div(
      style = "display: flex; align-items: flex-start; margin-bottom: 20px; font-size: 18px;",

      tags$div(
        style = "flex: 3; padding: 15px; border: 4px solid #3C8DBC; border-radius: 12px; background-color: #FDFDFE;",
        tags$table(
          style = "width: 100%; table-layout: fixed;",
          tags$tr(tags$td(tags$strong("Ethnicity:")), tags$td(eth_text),
                  tags$td(tags$strong("Activity:")), tags$td(row$ActivityDesc)),
          tags$tr(tags$td(tags$strong("Age:")), tags$td(ifelse(is.na(row$Age), "Unknown", row$Age)),
                  tags$td(tags$strong("Site:")), tags$td(row$SiteDesc)),
          tags$tr(tags$td(tags$strong("Location:")),
                  tags$td(paste0(row$Location, " (", row$LocationDesc, ")"), colspan = 3)),
          tags$tr(tags$td(tags$strong("Date:")), tags$td(format(as.Date(row$Date), "%e %b %Y")),
                  tags$td(tags$strong("Time:")),
                  tags$td(ifelse(is.na(row$TimeOfIncident), "Unknown", paste0(row$TimeOfIncident, ":00")))),
          tags$tr(tags$td(tags$strong("Alcohol/ Drugs:")), tags$td(row$AlcoholDrugDesc),
                  tags$td(tags$strong("Buoyancy:")), tags$td(row$BuoyancyDesc)),
          tags$tr(tags$td(tags$strong("Synopsis:")), tags$td(row$Synopsis, colspan = 3))
        ),
        tags$div(
          tags$label("Select Tags:"),
          tags$span(
            class = "glyphicon glyphicon-info-sign",
            `data-toggle` = "tooltip",
            `title` = "Select existing tags from the dropdown or add a new one in the blue box.",
            style = "cursor: pointer; margin-left: 5px;"
          ),
          tags$script(HTML("$(document).ready(function() { $('[data-toggle=\"tooltip\"]').tooltip(); });")),
          selectInput(
            inputId = select_input_id,
            label = NULL,
            choices = sort(unique(tags_df$Description)),
            selected = sort(unique(selected_tags)),
            multiple = TRUE,
            width = "100%"
          )
        ),
        tags$p(style = "margin-top: 10px; color: #FAFAFA;", paste(row$DrowningID))
      ),
      tags$div(style = "flex: 1; margin-left: 10px;", HTML(tag_html))
    )
  }
  
  
  dbError <- reactiveVal(NULL)
  tag_inputs <- reactiveValues()
  
  
  current_year <- reactive({
    year(Sys.Date())
  })
  total_drownings_year <- reactive({
    filtered_df <- df()
    
    if (!is.null(input$locationSelect) &&
        length(input$locationSelect) > 0) {
      filtered_df <- filtered_df[filtered_df$LocationDesc %in% input$locationSelect, ]
    }
    
    if (input$periodSelect == "Current Year") {
      filtered_df <- filtered_df[filtered_df$Year == year(Sys.Date()), ]
    }
    
    nrow(filtered_df)
  })
  tagged_drownings_year <- reactive({
    full_tag_df()  # <- add this dependency so it updates when tags change
    
    query <- paste0("GetDrowningTagTable 'Drowning', '",
                    user_roles$individual,
                    "'")
    result <- dbQuerySafe(query)
    
    if (nrow(result) == 0)
      return(0)
    
    if (!is.null(input$locationSelect) &&
        length(input$locationSelect) > 0) {
      result <- result[result$Location %in% input$locationSelect, ]
    }
    
    if (input$periodSelect == "Current Year") {
      result <- result[result$Year == year(Sys.Date()), ]
    }
    
    length(unique(result$DrowningID))
  })
  
  
  filtered_data <- reactiveVal(data.frame())
  weekly_data <- reactiveVal(data.frame())
  initial_selections <- reactiveValues()
  
  output$glossaryTable <- renderDT({
    datatable(glossary_df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  # Check DB Connection
  CheckDBConnection <- function(con) {
    tryCatch({
      dbGetQuery(con, "SELECT 1")
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  
  # Enhanced DB Connection with tryCatch
  GetWSNZAzureConnection <- function() {
    tryCatch({
      if (exists("con") && CheckDBConnection(con)) {
        return(con)
      } else {
        dbConnect(
          odbc(),
          Driver = "SQL Server",
          Server = "heimatau.database.windows.net",
          Database = "WSNZ",
          Port = 1433,
          Uid = "wsnztagger",
          Pwd = "tag_girl123"
        )
      }
    }, error = function(e) {
      dbError(conditionMessage(e))
      NULL
    })
  }

  
  con <- GetWSNZAzureConnection()
  
  Tags = dbQuerySafe(paste("GetDrowningTagTable Tag, ", user_roles$individual))
  DrowningTag =  dbQuerySafe(paste("GetDrowningTagTable DrowningTag, ", user_roles$individual))
  
  
  output$locationSelect <- renderUI({
    loc_df <- dbQuerySafe(paste("GetDrowningTagTable Location, ", user_roles$individual))
    
    selectInput(
      "locationSelect",
      "Select Location:",
      choices = loc_df$Description,
      multiple = TRUE
    )
  })
  
  
  valid_holiday_years <- reactive({
    req(input$calendarHoliday, input$dayBuffer)
    calendar_df <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    calendar_df <- calendar_df[calendar_df$Event %in% input$calendarHoliday, ]
    if (nrow(calendar_df) == 0) return(integer(0))
    
    calendar_df$StartDate <- as.Date(calendar_df$StartDate) - days(input$dayBuffer)
    calendar_df$EndDate   <- as.Date(calendar_df$EndDate) + days(input$dayBuffer)
    
    drown_df <- df()
    drown_df$Date <- as.Date(drown_df$Date)
    
    # For each holiday year, check if any incidents fall within its buffer
    years_with_data <- sapply(split(calendar_df, calendar_df$Year), function(event_year_df) {
      any(sapply(1:nrow(event_year_df), function(i) {
        range <- event_year_df[i, ]
        any(drown_df$Date >= range$StartDate & drown_df$Date <= range$EndDate)
      }))
    })
    
    as.integer(names(years_with_data)[years_with_data])
  })
  
  
  cleanDrowningData <- function(data) {
    if (nrow(data) > 0) {
      data$Date <- as.Date(data$Date)
      data <- data[, c(
        "DrowningID",
        "Date",
        "Age",
        "SexDesc",
        "EthnicityDesc",
        "ActivityDesc",
        "SiteDesc",
        "Location",
        "LocationDesc",
        "TimeOfIncident",
        "Synopsis",
        "RegionDesc",
        "SubEthnicityDesc",
        "AlcoholDrugDesc",
        "BuoyancyDesc",
        "Year"
      )]
    }
    return(data)
  }
  
  DF <- dbQuerySafe("getallprevdrowningdata")
  DF <- cleanDrowningData(DF)
  df <- reactiveVal(DF)
  
  full_tag_df <- reactiveVal(data.frame(
    TagID = character(),
    Description = character(),
    DrowningID = character()
  ))
  
  # Then, update it inside an observer:
  observe({
    if (nrow(Tags) > 0 && nrow(DrowningTag) > 0) {
      full_tag_df(merge(Tags, DrowningTag, by = 'TagID'))
    }
  })
  
  observeEvent(input$periodSelect, {
    if (input$periodSelect == "All Years") {
      df(DF)
    } else {
      print(names(DF))
      df(DF[DF$Year == year(Sys.Date()), ])
    }
  }, ignoreInit = TRUE)
  
  
  # Render database error in modal
  showErrorModal <- function(error_message) {
    showModal(modalDialog(
      title = "Critical Error",
      div(style = "color: red; font-weight: bold;", error_message),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }
  
  # General observer for database error, and tags added and removed
  observeTagChanges <- function(data_source, prefix) {
    lapply(1:nrow(data_source), function(i) {
      row <- data_source[i, ]
      select_input_id <- paste0(prefix, row$DrowningID)
      
      observeEvent(input[[select_input_id]], {
        new_tags <- input[[select_input_id]]
        old_tags <- full_tag_df()$Description[full_tag_df()$DrowningID == row$DrowningID]
        
        added_tags <- setdiff(new_tags, old_tags)
        removed_tags <- setdiff(old_tags, new_tags)
        
        if (length(added_tags) > 0) {
          insert_queries <- sapply(added_tags, function(tag) {
            user_to_use <- if (startsWith(select_input_id, "calendarTags_")) {
              user_roles$team
            } else {
              user_roles$individual
            }
            sprintf("EXEC DrowningTagInsertion '%s', '%s', '%s';",
                    row$DrowningID,
                    gsub("'", "''", tag),
                    user_to_use)
          })
          lapply(insert_queries, dbQuerySafe)
        }
        
        if (length(removed_tags) > 0) {
          delete_queries <- sapply(removed_tags, function(tag) {
            user_to_use <- if (startsWith(select_input_id, "calendarTags_")) {
              user_roles$team
            } else {
              user_roles$individual
            }
            sprintf("EXEC DrowningTagDeletion '%s', '%s', '%s';",
                    row$DrowningID,
                    gsub("'", "''", tag),
                    user_to_use)
          })
          
          lapply(delete_queries, dbQuerySafe)
        }
        
        # Update local
        DrowningTag <<- DrowningTag[!(
          DrowningTag$DrowningID == row$DrowningID &
            DrowningTag$TagID %in% Tags$TagID[Tags$Description %in% removed_tags]
        ), ]
        
        if (length(added_tags) > 0) {
          DrowningTag <<- rbind(
            DrowningTag,
            data.frame(
              DrowningID = row$DrowningID,
              TagID = Tags$TagID[Tags$Description %in% added_tags],
              UserName = user_roles$individual,
              stringsAsFactors = FALSE
            )
          )
        }
        
        full_tag_df(merge(Tags, DrowningTag, by = "TagID"))
      }, ignoreInit = TRUE)
    })
  }
  
  observe({
    if (!is.null(dbError())) {
      showErrorModal(dbError())
      dbError(NULL)
    }
    
    observeTagChanges(weekly_data(), "weeklyTags_")
    observeTagChanges(calendar_data(), "calendarTags_")
  })
  
  audit_data <- reactiveVal(data.frame())
  
  observeEvent(input$tabs, {
    if (input$tabs == "weekly_summary") {
      query <- sprintf("GetDrowningTagTable 'DrowningTag_AUDIT', '%s'", user_roles$individual)
      audit_data(dbQuerySafe(query))
    }else if (input$tabs == "special_tab") {
      latest_week <- tail(valid_weeks_all_tags(), 1)
      if (!is.null(latest_week)) {
        updateDateInput(session, "dateAllWeeks", value = latest_week)
      }
    }
  }, ignoreInit = TRUE)
  
  output$auditTable <- renderDT({
    datatable(audit_data(), options = list(pageLength = 15), rownames = FALSE)
  })
  observe({
    query <- sprintf("GetDrowningTagTable 'DrowningTag_AUDIT', '%s'", user_roles$individual)
    audit_data(dbQuerySafe(query))
  })
  # Display total already tagged the welcome message
  output$drowningSummary <- renderUI({
    tags$h4(HTML(
      paste0(
        "Tagged drownings this year: <br>",
        tagged_drownings_year(),
        " / ",
        total_drownings_year()
      )
    ))
  })
  
  get_latest_untagged_week <- function(df,
                                       DrowningTag,
                                       username,
                                       location_filter = NULL,
                                       periodSelect = "Current Year") {
    if (nrow(df) == 0)
      return(NULL)
    
    df$Date <- as.Date(df$Date)
    df$WeekStart <- lubridate::floor_date(df$Date, unit = "week", week_start = 1)
    
    # Filter by year if needed
    if (periodSelect == "Current Year") {
      df <- df[df$Year == lubridate::year(Sys.Date()), ]
    }
    
    tagged_ids <- unique(DrowningTag$DrowningID[DrowningTag$UserName == username])
    df_filtered <- df[!(df$DrowningID %in% tagged_ids), ]
    
    if (!is.null(location_filter) && length(location_filter) > 0) {
      df_filtered <- df_filtered[df_filtered$LocationDesc %in% location_filter, ]
    }
    
    if (nrow(df_filtered) == 0)
      return(NULL)
    
    latest_week <- max(df_filtered$WeekStart, na.rm = TRUE)
    return(latest_week)
  }
  
  
  
  
  observeEvent(input$skipToLatestUntagged | input$up_arrow, {
    if (input$tabs != "weekly_summary") return()
    
    latest_week <- get_latest_untagged_week(
      df(),
      DrowningTag,
      user_roles$individual,
      input$locationSelect,
      input$periodSelect
    )
    
    if (is.null(latest_week)) {
      showNotification("All matching drownings are tagged!", type = "message")
    } else {
      updateDateInput(session, "date", value = latest_week)
      
      monday <- floor_date(latest_week, "week", week_start = 1)
      sunday <- ceiling_date(latest_week, "week", week_start = 1) - days(1)
      filtered_df <- df()[df()$Date >= monday &
                            df()$Date <= sunday, ]
      
      # Apply location filter if needed
      if (length(input$locationSelect) > 0) {
        filtered_df <- filtered_df[filtered_df$LocationDesc %in% input$locationSelect, ]
      }
      
      weekly_data(filtered_df)
    }
  })
  
  
  # Updates filtered dataframe using the date
  observeEvent(c(input$date, input$locationSelect, input$periodSelect),
               {
                 req(input$date)
                 
                 safe_date <- as.Date(input$date)
                 if (is.na(safe_date))
                   return(NULL)  # Avoid invalid dates
                 
                 monday <- lubridate::floor_date(safe_date, "week", week_start = 1)
                 sunday <- lubridate::ceiling_date(safe_date, "week", week_start = 1) - lubridate::days(1)
                 
                 filtered_df <- df()[df()$Date >= monday &
                                       df()$Date <= sunday, ]
                 
                 # Then apply location filter if needed
                 if (length(input$locationSelect) > 0) {
                   filtered_df <- filtered_df[filtered_df$LocationDesc %in% input$locationSelect, ]
                 }
                 
                 if (input$periodSelect == "Current Year") {
                   filtered_df <- filtered_df[filtered_df$Year == year(Sys.Date()), ]
                 }
                 
                 
                 # Just to be safe, make sure Date is in Date class
                 if (nrow(filtered_df) > 0 &&
                     !inherits(filtered_df$Date, "Date")) {
                   filtered_df$Date <- as.Date(filtered_df$Date)
                 }
                 
                 weekly_data(filtered_df)
                 
               },
               ignoreInit = TRUE)
  
  output$weeklyTableHasData <- reactive({
    nrow(weekly_data()) > 0
  })
  outputOptions(output, "weeklyTableHasData", suspendWhenHidden = FALSE)
  
  # Render each event details
  output$weeklySummaryTable <- renderUI({
    hide("loading-overlay")
    
    data <- weekly_data()
    if (nrow(data) == 0) return(HTML("<p>No drowning incidents found for the selected week.</p>"))
    
    tag_list <- full_tag_df()
    lapply(seq_len(nrow(data)), function(i) {
      row <- data[i, ]
      selected_tags <- tag_list$Description[tag_list$DrowningID == row$DrowningID]
      renderDrowningSummaryCard(row, Tags, selected_tags, input_id_prefix = "weeklyTags_")
    })
  })  
  
  

  
  handle_week_navigation <- function(direction = c("next", "previous"), tab = input$tabs) {
    direction <- match.arg(direction)
    
    switch(tab,
           "weekly_summary" = {
             req(input$date)
             new_date <- as.Date(input$date) + ifelse(direction == "next", 7, -7)
             
             if (input$periodSelect == "Current Year") {
               min_date <- as.Date("2025-01-01")
               new_date <- max(min_date, min(new_date, Sys.Date()))
             } else {
               new_date <- min(new_date, Sys.Date())
             }
             
             updateDateInput(session, "date", value = new_date)
           },
           
           "holiday_tab" = {
             if (is.null(input$calendarHoliday) || input$calendarHoliday == "") {
               showNotification("Please select a holiday first.", type = "error")
             } else {
               current_year_val <- calendar_year()
               valid_years <- valid_holiday_years()
               
               if (length(valid_years) == 0) {
                 showNotification("No valid years found for this holiday.", type = "error")
                 return()
               }
               
               new_year <- if (direction == "next") {
                 min(valid_years[valid_years > current_year_val], na.rm = TRUE)
               } else {
                 max(valid_years[valid_years < current_year_val], na.rm = TRUE)
               }
               
               if (!is.finite(new_year)) {
                 showNotification(sprintf("No %s years with incidents.", direction), type = "message")
               } else {
                 calendar_year(new_year)
                 
               }
             }
           },
           
           "special_tab" = {
             req(input$dateAllWeeks)
             current <- floor_date(as.Date(input$dateAllWeeks), "week", week_start = 1)
             week_list <- valid_weeks_all_tags()
             new_week <- if (direction == "next") {
               min(week_list[week_list > current], na.rm = TRUE)
             } else {
               max(week_list[week_list < current], na.rm = TRUE)
             }
             
             if (!is.finite(new_week)) {
               showNotification(sprintf("No %s weeks with incidents.", direction), type = "message")
             } else {
               updateDateInput(session, "dateAllWeeks", value = new_week)
             }
           }
    )
  }

  
  observeEvent(input$left_arrow, {
    if (arrowLock()) return()
    arrowLock(TRUE)
    
    handle_week_navigation("previous")
    
    # Unlock after 1 second
    later::later(function() {
      arrowLock(FALSE)
    }, delay = 1)
  })
  
  observeEvent(input$right_arrow, {
    if (arrowLock()) return()
    arrowLock(TRUE)
    
    handle_week_navigation("next")
    
    # Unlock after 1 second
    later::later(function() {
      arrowLock(FALSE)
    }, delay = 1)
  })
  
  
  
  observeEvent(input$displayPreviousWeek, {
    handle_week_navigation("previous")
  })
  
  observeEvent(input$displayNextWeek, {
    handle_week_navigation("next")
  })
  
  
  
  
  # addLabel
  observeEvent(input$weekAddLabel, {
    id <- input$weekTagIDInput
    desc <- input$weekTagDescInput
    
    if (id == '' || desc == '') {
      showNotification("Unable to create a tag with an empty ID or Description", type = "error")
      return()
    }
    
    result <- tryCatch({
      con <- GetWSNZAzureConnection()
      dbExecute(con,
                "EXEC CreateTag @TagID = ?, @Description = ?",
                params = list(toupper(id), desc))
    }, error = function(e) {
      if (grepl("Tag ID already exists", e$message)) {
        showNotification("This Tag ID is already in use. Please choose a different ID.", type = "error")
      } else if (grepl("Description already exists", e$message)) {
        showNotification("This description is already in use. Please use the existing tag.", type = "error")
      } else {
        showNotification(paste("An error occurred:", e$message), type = "error")
      }
      return(NULL)
    })
    
    # Update dropdowns regardless of success/failure
    if (!is.null(result)) {
      showNotification(paste("Successfully created tag:", id, "-", desc), type = "message")
      
      # Update Tags dataset
      Tags <<- rbind(Tags,
                     data.frame(
                       TagID = id,
                       Description = desc,
                       stringsAsFactors = FALSE
                     ))
      
      updateTextInput(session, "weekTagDescInput", value = "")
      updateTextInput(session, "weekTagIDInput", value = "")
    }
    
    # Always refresh tag dropdowns, even on error
    data <- weekly_data()
    for (i in 1:nrow(data)) {
      row <- data[i, ]
      select_input_id <- paste0("weeklyTags_", row$DrowningID)
      
      updateSelectInput(session,
                        inputId = select_input_id,
                        choices = unique(Tags$Description))
    }
  })
  
  
  
  observeEvent(Tags, {
    data <- weekly_data()
    
    for (i in 1:nrow(data)) {
      row <- data[i, ]
      select_input_id <- paste0("weeklyTags_", row$DrowningID)
      updateSelectInput(session,
                        inputId = select_input_id,
                        choices = unique(Tags$Description))
    }
  }, ignoreInit = TRUE)
  
  
  # Create a reactive value to store the filtered data
  full_tag_df <- reactiveVal(merge(Tags, DrowningTag, by = 'TagID'))
  weekly_data_all_tags <- reactiveVal(data.frame())
  
  
  
  
  
  output$weeklyTableAllTagsHasData <- reactive({
    nrow(weekly_data_all_tags()) > 0
  })
  
  outputOptions(output, "weeklyTableAllTagsHasData", suspendWhenHidden = FALSE)
  
  output$weeklySummaryTableAllTags <- renderUI({
    hide("loading-overlay")
    
    tryCatch({
      data <- weekly_data_all_tags()
      if (length(input$locationSelectAllTags) > 0) {
        data <- data[data$LocationDesc %in% input$locationSelectAllTags, ]
      }
      
      if (nrow(data) == 0)
        return(HTML(
          "<p>No drowning incidents found for the selected week.</p>"
        ))
      summary_ui <- lapply(1:nrow(data), function(i) {
        row <- data[i, ]
        select_input_id <- paste0("weeklyTags_", row$DrowningID)  # Unique selectInput ID for each incident
        
        # Query database to get selected tags for this drowning case
        selected_tags_query <- sprintf(
          "SELECT
            TagID, COUNT(*) AS Count, STRING_AGG(LEFT(Username,1), ', ') WITHIN GROUP (ORDER BY LEFT(Username,1)) as Usernames
            FROM DrowningTag
            WHERE DrowningID =   %d
            GROUP BY TagID
            ORDER BY Count DESC

            ",
          row$DrowningID
        )
        selected_tags_df <- dbQuerySafe(selected_tags_query)
        
        # Convert TagID to Description and format as "Description (Count)"
        selected_tags <- if (nrow(selected_tags_df) > 0) {
          paste0(Tags$Description[match(selected_tags_df$TagID, Tags$TagID)],
                 # Get tag descriptions
                 " (",
                 selected_tags_df$Usernames,
                 ") ")
        } else {
          NULL  # If no tags exist, return NULL
        }
        
        
        tag_html <- if (!is.null(selected_tags_df) && nrow(selected_tags_df) > 0) {
          paste(sapply(1:nrow(selected_tags_df), function(i) {
            tag_desc <- Tags$Description[match(selected_tags_df$TagID[i], Tags$TagID)]
            usernames <- selected_tags_df$Usernames[i]
            user_count <- lengths(regmatches(usernames, gregexpr("[A-Za-z]", usernames)))
            
            border_style <- if (user_count >= 4) {
              "2px solid #3C8DBC"  # Blue border
            } else {
              "1px solid #ccc"     # Default gray border
            }
            
            sprintf(
              "<div style='background-color: #f0f0f0; color: #333; border: %s; border-radius: 10px; padding: 5px 10px; margin: 5px; display: inline-block; font-size: 16px;'>%s (%s)</div>",
              border_style,
              tag_desc,
              usernames
            )
          }), collapse = "")
        } else {
          "<p>No tags selected.</p>"
        }
        
        if (!(row$SubEthnicityDesc %in% c("Unknown", "Maori", "New Zealand European"))) {
          eth_text = paste0(row$EthnicityDesc, " (", row$SubEthnicityDesc, ")")
        } else{
          eth_text = row$EthnicityDesc
        }
        tags$div(
          style = "display: flex; align-items: flex-start; margin-bottom: 20px;",
          tags$div(
            style = box_style,
            tags$table(
              style = "width: 100%; table-layout: fixed; font-size: 16px;",
              tags$tr(
                tags$td(tags$strong("Ethnicity:")),
                tags$td(eth_text),
                tags$td(tags$strong("Activity:")),
                tags$td(row$ActivityDesc)
              ),
              tags$tr(
                tags$td(tags$strong("Age:")),
                tags$td(ifelse(
                  is.na(row$Age), "Unknown", row$Age
                )),
                tags$td(tags$strong("Site:")),
                tags$td(row$SiteDesc)
              ),
              tags$tr(tags$td(tags$strong(
                "Location:"
              )), tags$td(
                paste0(row$Location, " (", row$LocationDesc, ")"),
                colspan = 3
              )),
              tags$tr(
                tags$td(tags$strong("Date:")),
                tags$td(format(as.Date(row$Date), "%e %b %Y")),
                tags$td(tags$strong("Time:")),
                tags$td(ifelse(
                  is.na(row$TimeOfIncident),
                  "Unknown",
                  paste0(row$TimeOfIncident, ":00")
                ))
              ),
              tags$tr(
                tags$td(tags$strong("Synopsis:")),
                tags$td(row$Synopsis, colspan = 3)
              )
            ),
            tags$p(style = "margin-top: 10px; color: #FAFAFA;", paste(row$DrowningID))
            
          ),
          
          tags$div(style = "flex: 1; margin-left: 10px;", HTML(tag_html))
        )
      })
    }, error = function(e) {
      showNotification(paste("Error rendering weekly summary table:", e$message),
                       type = "error")
      HTML("<p>Error displaying data.</p>")
    })
  })
  
  
  

  

  
  
  valid_weeks_all_tags <- reactive({
    req(df())
    df_data <- df()
    if (length(input$locationSelectAllTags) > 0) {
      df_data <- df_data[df_data$LocationDesc %in% input$locationSelectAllTags, ]
    }
    df_data$WeekStart <- lubridate::floor_date(df_data$Date, unit = "week", week_start = 1)
    sort(unique(df_data$WeekStart))
  })
  
  
  
  observeEvent(input$dateAllWeeks, {
    req(input$dateAllWeeks)
    selected_week <- floor_date(as.Date(input$dateAllWeeks), "week", week_start = 1)
    
    if (!(selected_week %in% valid_weeks_all_tags())) {
      showNotification("Invalid week. Please pick a week with an incident.", type = "error")
      return()
    }
    
    monday <- selected_week
    sunday <- ceiling_date(selected_week, "week", week_start = 1) - days(1)
    
    filtered_df <- df()[df()$Date >= monday & df()$Date <= sunday, ]
    
    if (length(input$locationSelectAllTags) > 0) {
      filtered_df <- filtered_df[filtered_df$LocationDesc %in% input$locationSelectAllTags, ]
    }
    
    weekly_data_all_tags(filtered_df)
  }, ignoreInit = TRUE)
  
  observeEvent(Tags, {
    data <- weekly_data_all_tags()
    
    for (i in 1:nrow(data)) {
      row <- data[i, ]
      select_input_id <- paste0("weeklyTags_", row$DrowningID)
      updateAllWeeksSelectInput(session,
                                inputId = select_input_id,
                                choices = unique(Tags$Description))
    }
  }, ignoreInit = TRUE)
  
  

  output$locationSelectAllTags <- renderUI({
    loc_df <- dbQuerySafe(paste("GetDrowningTagTable Location, ", user_roles$individual))
    
    selectInput(
      "locationSelectAllTags",
      NULL,
      choices = loc_df$Description,
      multiple = TRUE
    )
    
    
  })
  observeEvent(input$skipToEarliestAllTags, {
    earliest_week <- head(valid_weeks_all_tags(), 1)
    if (!is.null(earliest_week)) {
      updateDateInput(session, "dateAllWeeks", value = earliest_week)
    } else {
      showNotification("No data available for the selected location(s).", type = "error")
    }
  })
  
  observeEvent(input$skipToLatestAllTags, {
    latest_week <- tail(valid_weeks_all_tags(), 1)
    if (!is.null(latest_week)) {
      updateDateInput(session, "dateAllWeeks", value = latest_week)
    } else {
      showNotification("No data available for the selected location(s).", type = "error")
    }
  })
  
  calendar_data <- reactiveVal(data.frame())
  calendar_year <- reactiveVal(year(Sys.Date()))
  
  # Function to filter incidents based on selected holidays and buffer
  observe({
    req(df())
    
    holidays <- input$calendarHoliday
    buffer <- input$dayBuffer
    selected_year <- calendar_year()
    
    if (is.null(holidays) || length(holidays) == 0) {
      calendar_data(data.frame())
      return()
    }
    
    calendar_df <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    
    # Filter by selected event(s) and selected year
    calendar_df <- calendar_df[
      calendar_df$Event %in% holidays & calendar_df$Year == selected_year,
    ]
    
    if (nrow(calendar_df) == 0) {
      calendar_data(data.frame())
      return()
    }
    
    calendar_df$StartDate <- as.Date(calendar_df$StartDate) - days(buffer)
    calendar_df$EndDate   <- as.Date(calendar_df$EndDate) + days(buffer)
    
    drown_df <- df()
    drown_df$Date <- as.Date(drown_df$Date)
    
    matched <- do.call(rbind, lapply(1:nrow(calendar_df), function(i) {
      range <- calendar_df[i, ]
      drown_df[drown_df$Date >= range$StartDate & drown_df$Date <= range$EndDate, ]
    }))
    
    calendar_data(unique(matched))
  })

  

  
 
  output$calendarYearText <- renderText({
    paste("Showing:", calendar_year())
  })
  
  
  
  # UI Rendering

  output$calendarSummaryTable <- renderUI({
    hide("loading-overlay")
    
    data <- calendar_data()
    holidays <- input$calendarHoliday
    buffer <- input$dayBuffer
    selected_year <- calendar_year()
    
    calendar_df <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    calendar_df <- calendar_df[
      calendar_df$Event %in% holidays & calendar_df$Year == selected_year,
    ]
    
    calendar_df$StartDate <- as.Date(calendar_df$StartDate) - days(buffer)
    calendar_df$EndDate   <- as.Date(calendar_df$EndDate) + days(buffer)
    
    if (nrow(data) == 0) {
      if (nrow(calendar_df) == 0) {
        return(HTML(sprintf(
          "<h3>No matching holidays found for <strong>%s</strong> in <strong>%d</strong>.</h3>",
          input$calendarHoliday,
          selected_year
         
        )))
      } else {
        return(HTML(sprintf(
          "<h3>No drowning incidents found for <strong>%s</strong> with a %d-day buffer (%s to %s).</h3>",
          input$calendarHoliday,
          input$dayBuffer,
          format(min(calendar_df$StartDate), "%d %b %Y"),
          format(max(calendar_df$EndDate), "%d %b %Y")
        )))
      }
    }
    
    tag_list <- full_tag_df()
    tagList(
      HTML(sprintf(
        "<div style='text-align: left; margin-bottom: 20px;'>
     <h3 style='margin-bottom: 5px;'>%s %d with a %d-day buffer</h3>
     <div style='font-size: 18px; color: #555;'>%s to %s</div>
   </div>",
        input$calendarHoliday,
        calendar_year(),
        input$dayBuffer,
        format(min(calendar_df$StartDate), "%d %b %Y"),
        format(max(calendar_df$EndDate), "%d %b %Y")
      )),
      lapply(seq_len(nrow(data)), function(i) {
        row <- data[i, ]
        selected_tags <- tag_list$Description[tag_list$DrowningID == row$DrowningID]
        renderDrowningSummaryCard(row, Tags, selected_tags, input_id_prefix = "calendarTags_")
      })
    )
  })
  
  
  
  output$calendarHoliday <- renderUI({
    loc_df <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    
    div(style = "display: flex; align-items: center; gap: 10px; margin-top: 10px;",
        selectInput(
          "calendarHoliday",
          NULL,
          choices = unique(loc_df$Event[loc_df$EventType == "Public Holiday"]),
          multiple = FALSE
        ),
        numericInput("dayBuffer", NULL, 0, width = "90px")
    )
  })
  
  
  
  
  
  
  observeEvent(input$calendarNextYear, {
    shinyjs::disable("calendarPrevYear")
    shinyjs::disable("calendarNextYear")
    
    handle_week_navigation("next", "holiday_tab")
    
    invalidateLater(1000, session)
    observe({
      shinyjs::enable("calendarPrevYear")
      shinyjs::enable("calendarNextYear")
    })
  })
  
  
  observeEvent(input$calendarPrevYear, {
    shinyjs::disable("calendarPrevYear")
    shinyjs::disable("calendarNextYear")
    
    handle_week_navigation("previous", "holiday_tab")
    
    # re-enable after a short delay
    invalidateLater(1000, session)
    observe({
      shinyjs::enable("calendarPrevYear")
      shinyjs::enable("calendarNextYear")
    })
  })
  
  
  observeEvent(input$displayPreviousWeekAllTags, {
    handle_week_navigation("previous", "special_tab")
  })
  
  observeEvent(input$displayNextWeekAllTags, {
    handle_week_navigation("next", "special_tab")
  })
  
  output$calendarYearSelector <- renderUI({
    req(input$calendarHoliday)
    
    calendar_df <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    available_years <- sort(valid_holiday_years(), decreasing = TRUE)
    
    selectInput(
      "calendarYearDropdown",
      label = NULL,
      choices = available_years,
      selected = year(Sys.Date())
    )
  })
  
  # observe({
  #   req(input$tabs == "holiday_tab")
  #   isolate({
  #     print(paste("Here with ",input$calendarYearDropdown, calendar_year() ))
  #     if (!identical(input$calendarYearSelector, calendar_year())) {
  #       print("HERE")
  #       updateSelectInput(session, "calendarYearDropdown", selected = calendar_year())
  #     }
  #   })
  # })
  # 
  
  observeEvent(input$calendarYearDropdown, {
    new_val <- as.numeric(input$calendarYearDropdown)
    # print(paste("New value: ",new_val))
    # print(paste("Calendar value: ",calendar_year()))
    if (!identical(calendar_year(), new_val)) {
      calendar_year(new_val)
      updateSelectInput(session, "calendarYearDropdown", selected = new_val)
      
    }
  })
  
  
  total_drownings_all <- reactive({
    req(df(), input$calendarHoliday)
    
    # Get all calendar entries for the selected holiday (all years)
    calendar_df_all <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    calendar_df_all <- calendar_df_all[calendar_df_all$Event %in% input$calendarHoliday, ]
    
    if (nrow(calendar_df_all) == 0) return(0)
    
    # Use buffer
    calendar_df_all$StartDate <- as.Date(calendar_df_all$StartDate) - days(input$dayBuffer)
    calendar_df_all$EndDate   <- as.Date(calendar_df_all$EndDate) + days(input$dayBuffer)
    
    drown_df <- df()
    drown_df$Date <- as.Date(drown_df$Date)
    
    matched <- do.call(rbind, lapply(1:nrow(calendar_df_all), function(i) {
      range <- calendar_df_all[i, ]
      drown_df[drown_df$Date >= range$StartDate & drown_df$Date <= range$EndDate, ]
    }))
    
    nrow(unique(matched))
  })
  
  
  
  tagged_drownings_all <- reactive({
    req(full_tag_df(), input$calendarHoliday)
    
    calendar_df_all <- dbQuerySafe(paste0("GetDrowningTagTable Calendar, '", user_roles$team, "'"))
    calendar_df_all <- calendar_df_all[calendar_df_all$Event %in% input$calendarHoliday, ]
    
    if (nrow(calendar_df_all) == 0) return(0)
    
    calendar_df_all$StartDate <- as.Date(calendar_df_all$StartDate) - days(input$dayBuffer)
    calendar_df_all$EndDate   <- as.Date(calendar_df_all$EndDate) + days(input$dayBuffer)
    
    drown_df <- df()
    drown_df$Date <- as.Date(drown_df$Date)
    
    matched <- do.call(rbind, lapply(1:nrow(calendar_df_all), function(i) {
      range <- calendar_df_all[i, ]
      drown_df[drown_df$Date >= range$StartDate & drown_df$Date <= range$EndDate, ]
    }))
    
    if (nrow(matched) == 0) return(0)
    
    matched_ids <- unique(matched$DrowningID)
    tagged_ids <- unique(full_tag_df()$DrowningID)
    
    length(intersect(matched_ids, tagged_ids))
  })
  
  
  output$calendarDrowningSummary <- renderUI({
    tags$h4(HTML(
      paste0(
        "Tagged drownings (all time): <br>",
        tagged_drownings_all(),
        " / ",
        total_drownings_all()
      )
    ))
  })
  

  

}

shinyApp(ui = ui, server = server)
