library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(data.table)
library(DT)
library(DCIEM)

#-----------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        $('#date').attr('readonly', true);
      });
    ")),
    tags$style(HTML("
      .border-right {
        border-right: 1px solid #ddd;
      }
      .border-left {
        border-left: 1px solid #ddd;
      }
      body, .container-fluid {
        min-width: 900px;
        max-width: 900px;
        margin-right: auto;
        margin-left: auto;
      }
      .input-box {
        border: 1px solid #ddd;
        padding: 6px;
        border-radius: 5px;
        margin-bottom: 10px;
        background-color: #f9f9f9;
      }
      .input-box .col-sm-2, .input-box .col-sm-1 {
        min-width: 100px;
        max-width: 120px;
      }
    "))
  ),

  titlePanel(
      title = span(img(src = "./dciemhex.png"), " DCIEM")
    ),


  div(class = "input-box",
      fluidRow(
        column(2, dateInput("date", "Date")),
        column(2, selectInput("timeIn", "Time In",
                              choices = {
                                full_sequence <- format(seq(as.POSIXct("00:00", format="%H:%M"),
                                                            as.POSIXct("23:59", format="%H:%M"),
                                                            by="min"),
                                                        "%H:%M")
                                start_index <- match("07:00", full_sequence)
                                c(full_sequence[start_index:length(full_sequence)],
                                  full_sequence[1:(start_index-1)])
                              })),
        column(2, selectInput("timeOut", "Time Out",
                              choices = {
                                full_sequence <- format(seq(as.POSIXct("00:00", format="%H:%M"),
                                                            as.POSIXct("23:59", format="%H:%M"),
                                                            by="min"),
                                                        "%H:%M")
                                start_index <- match("07:00", full_sequence)
                                c(full_sequence[start_index:length(full_sequence)],
                                  full_sequence[1:(start_index-1)])
                              })),
        # column(2, selectInput("timeIn", "Time In", choices = format(seq(as.POSIXct("00:00", format="%H:%M"), as.POSIXct("23:59", format="%H:%M"), by="min"), "%H:%M"))),
        # column(2, selectInput("timeOut", "Time Out", choices = format(seq(as.POSIXct("00:00", format="%H:%M"), as.POSIXct("23:59", format="%H:%M"), by="min"), "%H:%M"))),
        column(1, numericInput("EBT", "EBT", value = 0 )),
        column(1, numericInput("MaxDepth", "MaxDepth", value = 0 )),
        column(1, numericInput("airIn", "Air in", value = 200) ),
        column(1, numericInput("airOut", "Air Out", value = 100 )),

      ),
      fluidRow(
        column(10,  actionButton("addBtn", "Add Entry", class = "btn-action"))
      )
  ),
  div(class = "output-box",
      DTOutput("table")
  )
)

#-----------------------------------------------------------------------------
server <- function(input, output, session) {

    # function to increase RG letters
    increment_letter <- function(letter) {
      if (letter == "" || is.na(letter) || letter == "Z" || letter == "z") {
        return("")
      } else {
        return(LETTERS[which(LETTERS == toupper(letter)) + 1])
      }
    }

  rv <- reactiveValues(data = data.frame(Date = as.Date(character()),
                                         TimeIn = character(),
                                         TimeOut = character(),
                                         AirIn = numeric(),
                                         AirOut = numeric(),
                                         MaxDepth = numeric(),
                                         BT = numeric(),
                                         EBT = numeric(),
                                         RG = numeric(),
                                         RG2 = character(),
                                         SI = character(),
                                         RF = numeric(),
                                         stringsAsFactors = FALSE))

  #------------------ Input settings ------------------------------------------

  # observer for timeIn
  observeEvent(input$timeIn, {
    if (!values$timeOutChanged) {
      updateSelectInput(session, "timeOut", selected = input$timeIn)
    }
  })

  # observer for timeOut
  observeEvent(input$timeOut, {
    values$timeOutChanged <- TRUE
  }, ignoreInit = TRUE)

  #@@@@@@@ update TimeOut based on TimeIn
  times <- format(seq(as.POSIXct("00:00", format="%H:%M"), as.POSIXct("23:59", format="%H:%M"), by="min"), "%H:%M")
  values <- reactiveValues(timeOutChanged = FALSE)

  #@@@@@@@ update BT based on TimeOut
  observe({
    # Ensure that both TimeIn and TimeOut have been selected
    if (!is.null(input$timeIn) && !is.null(input$timeOut)) {
      # Convert TimeIn and TimeOut to POSIXct
      dateTimeIn <- ymd_hm(paste(input$date, input$timeIn))
      dateTimeOut <- ymd_hm(paste(input$date, input$timeOut))

      # Handle case where TimeOut is on the next day
      if (dateTimeOut < dateTimeIn) {
        dateTimeOut <- dateTimeOut + days(1)
      }

      # Calculate the difference in minutes
      timeDifference <- as.numeric(difftime(dateTimeOut, dateTimeIn, units = "mins"))

      # Update EBT input
      updateNumericInput(session, "EBT", value = timeDifference)
    }
  })

  #@@@@@@@ only allow entry where TimeOut is after TimeIn (If all conditions are met, enable the button)
  enableButton <- reactive({
    currentDateTimeIn <- ymd_hm(paste(input$date, input$timeIn)) # Fetch the current input values
    currentDateTimeOut <- ymd_hm(paste(input$date, input$timeOut))

    if (currentDateTimeOut < currentDateTimeIn) { # Check if currentDateTimeOut is less than currentDateTimeIn
      return(FALSE)
    }

    if (nrow(rv$data) > 0) {  # Check the last row's dateTimeOut against current dateTimeIn
      lastDateTimeOut <- ymd_hm(paste(rv$data$Date[nrow(rv$data)], rv$data$TimeOut[nrow(rv$data)]))
      if (currentDateTimeIn < lastDateTimeOut) {
        return(FALSE)
      }
    }

    return(TRUE) # If all conditions are met, enable the button

  })

    observe({ # Observe changes and enable/disable the button
    if (enableButton()) {
      shinyjs::enable("addBtn")
    } else {
      shinyjs::disable("addBtn")
    }
  })

    #@@@@@@@ Initiate "Add Entry" button and add rows
  observeEvent(input$addBtn, {



    # Reset the flag indicating manual change of timeOut
    values$timeOutChanged <- FALSE

    # Check if there is at least one row in the data
    if (nrow(rv$data) > 0) {
      # Fetch the TimeOut of the last row
      lastTimeOut <- rv$data$TimeOut[nrow(rv$data)]

      # Update TimeIn to be the same as the last row's TimeOut
      updateSelectInput(session, "timeIn", selected = lastTimeOut)

      # Recalculate dateTimeIn with the updated TimeIn
      dateTimeIn <- ymd_hm(paste(input$date, lastTimeOut))
    } else {
      # If there are no rows, use the current input for TimeIn
      dateTimeIn <- ymd_hm(paste(input$date, input$timeIn))
    }

    # Combine Date and BT and convert to POSIXct
    dateTimeIn <- ymd_hm(paste(input$date, input$timeIn))
    dateTimeOut <- ymd_hm(paste(input$date, input$timeIn))




    if (nrow(rv$data) > 0) {

      # Calculate SI
      lastDateTimeOut <- ymd_hm(paste(rv$data$Date[nrow(rv$data)], rv$data$TimeOut[nrow(rv$data)]))
      diff_in_seconds <- as.numeric(difftime(dateTimeIn, lastDateTimeOut, units = "secs")) # Calculate the difference in seconds and then in hours and minutes
      hours <- diff_in_seconds %/% 3600
      minutes <- (diff_in_seconds %% 3600) / 60
      minutes2 <- hours*60 + minutes
      SIval <- sprintf("%02d:%02d", hours, minutes) # Format SIval
    } else {
      SIval <- NA # Set SI to 24:00 if there's no previous row
      minutes2 <- 24*60
    }

    # Calculate BT
    if (nrow(rv$data) > 0) {
      lastRF <- rv$data$RF[nrow(rv$data)]
      Timeval <- ceiling(lastRF * input$EBT) # NOTE always rounds up
    } else {
      Timeval <- input$EBT
    }

    # Function to increment a letter alphabetically
    increment_letter <- function(letter) {
      if (letter == "" || is.na(letter) || letter == "Z" || letter == "z") {
        return("")
      } else {
        return(LETTERS[which(LETTERS == toupper(letter)) + 1])
      }
    }

    # Calculate RG2 based on the last RG value
    RG2val <- if (nrow(rv$data) > 0) {
      increment_letter(rv$data$RG[nrow(rv$data)])
    } else {
      ""
    }




    # Create new entry
    newEntry <- data.frame(Date = input$date,
                           AirIn = input$airIn,
                           AirOut = input$airOut,
                           TimeIn = format(dateTimeIn, "%H:%M"),
                           TimeOut = format(ymd_hm(paste(input$date, input$timeOut)), "%H:%M"),
                           BT = Timeval,
                           MaxDepth = input$MaxDepth,
                           EBT = input$EBT,
                           RG = DCIEM::get_RG(input$MaxDepth, input$EBT),
                           RG2 = RG2val,
                           SI = SIval,
                           RF = DCIEM::get_RF(get_RG(input$MaxDepth, input$EBT), minutes2)
                           )


    # Add new entry to data
    rv$data <- rbind(rv$data, newEntry)

   })

  #@@@@@@@ render DT

  output$table <- renderDT({
    datatable(rv$data,
              options = list(pageLength = 100, searching = FALSE),
              editable = list(target = 'cell', disable = list(columns = c(8)))) %>%
      formatStyle('AirOut',
                  backgroundColor = styleInterval(c(30, 50), c('red', 'orange', 'white')))

  })


  #@@@@@@@ function to allow cells to be edited (needs refining)
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    str(info)
    rv$data[info$row, info$col] <<- DT::coerceValue(info$value, rv$data[info$row, info$col])

    # Update RG if MaxDepth or EBT is edited
    if (info$col %in% c("MaxDepth", "EBT")) { # Assuming 'MaxDepth' is 4th and 'EBT' is 5th column
      rv$data[info$row, 'RG'] <<- get_RG(rv$data[info$row, 'MaxDepth'], rv$data[info$row, 'EBT'])
    }
  })

  #@@@@@@@ Render the data table with a delete button column
    output$table <- renderDT({
      rv$data %>%
        mutate(Delete = sprintf('<button onclick="Shiny.onInputChange(\'delete_row\', %s)" class="btn btn-danger btn-xs">Delete</button>', row_number())) %>%
        datatable(escape = FALSE, options = list(pageLength = 100, searching = FALSE),
                  editable = list(target = 'cell', disable = list(columns = c(8))))
    })

    # Observe delete button clicks
    observeEvent(input$delete_row, {
      req(input$delete_row)  # Ensure there's a value to work with
      rv$data <- rv$data[-input$delete_row, ]  # Remove the row

      # Recalculate dependent values (e.g., RG2) for all rows
      rv$data$RG2 <- c(rv$data$RG[1], sapply(rv$data$RG[-length(rv$data$RG)], increment_letter))
    })

}

shinyApp(ui, server)
options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

