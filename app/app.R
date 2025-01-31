library(shiny)
library(shinyWidgets)
library(dplyr)
library(gt)
library(shinyjs)
#library(DBI)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
#library(shinylive)
library(httr2)

ui <- fluidPage(titlePanel("MGIMS Attendance"),
                fluidRow(column(
                  width = 12,
                  # align="center",
                  column(
                    width = 3,
                    align = "center",
                    disabled(
                      airDatepickerInput(
                        inputId = "date",
                        label = "Month",
                        view = "months",
                        minView = "months",
                        dateFormat = "MMM yyyy",
                        autoClose = TRUE,
                        minDate = as.Date("2024-01-01"),
                        maxDate = floor_date(Sys.Date() - 1, "month"),
                        value = floor_date(Sys.Date() - 1, "month")
                      )
                    )
                  ),
                  column(
                    width = 3,
                    offset = 1,
                    align = "center",
                    disabled(selectInput("dept", "Department", choices = NA))
                  ),
                  column(
                    width = 3,
                    offset = 1,
                    align = "center",
                    disabled(selectInput("cat", "Category", choices = NA))
                  ),
                  column(
                    width = 1,
                    align = "center",
                    br(),
                    actionButton("go", "Go")
                  ),
                )),
                fluidRow(gt_output("table")),
                fluidRow(uiOutput("updateData")))

##Button function####
details_btn <- function(...) {
  as.character(shiny::actionButton(label = "",
                                   icon = icon("calendar-days"),
                                   ...)) %>%
    gt::html()
}

server <- function(input, output) {
  
  last_date <- as.Date(unlist(
    request(
      paste0(
        "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
        "nmcatt_api_attend?select=log_date&order=log_date.desc&limit=1"
      )
    ) |>
      req_headers(apikey = Sys.getenv("apikey")) |>
      req_perform() |>
      resp_body_json()
  ))
  
  #Update
  output$updateData <- renderText({
    paste0("<p style='text-align:center;'>","Last updated till ",format(last_date,"%d %B %Y"),"</p>")})
 
  #Department
  enable("dept")
  
  department <- reactive({
    c("All", unname(
      unlist(
        request(
          "https://hocwxloxuwuvozbeyepc.supabase.co//rest/v1/rpc/get_sorted_distinct"
        ) %>%
          req_headers(
            "apikey" = Sys.getenv("apikey"),
            "Authorization" = paste("Bearer", Sys.getenv("apikey")),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(
            list(
              column_name = "division",
              table_name = "nmcatt_api_emp",
              sort_order = "ASC"
            )
          ) %>%
          req_perform() %>%
          resp_body_json()
      )
    ))
  })
  
  observe({
    updateSelectInput(inputId = "dept",
                      choices = department())
  })
  
  #Categories
  enable("cat")
  
  catOpt <- reactive({
    c("Faculty", "Senior Resident", "Junior Resident", "Other")
  })
  
  observe({
    updateSelectInput(inputId = "cat",
                      choices = catOpt())
  })
  
  # Time cut offs
  
  entry_cut <- " 09:20:59"
  exit_cut_wd <- " 04:45:00"
  exit_cut_sat <- " 12:45:00"
  
  ## Ind Calender ####
  observeEvent(input$details, {
    withProgress(
      message = paste0(
        'Getting attendance data for ',
        unlist(request(
          paste0(
            "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
            "nmcatt_api_emp?select=emp_name&emp_id=eq.",input$emp_id
          )
        ) |>
          req_headers(apikey = Sys.getenv("apikey")) |>
          req_perform() |>
          resp_body_json()),
        "."
      ),
      value = 0,
      {
        incProgress(1 / 5, detail = paste("Getting details"))
        
        monthSeq <- data.frame(date = seq(
          floor_date(input$date, 'month'),
          ceiling_date(input$date, 'month') - 1,
          by = 1
        ))
        
        
        data <- 
          request(paste0(Sys.getenv("db_url"), "/rest/v1/rpc/get_employee_attend_details")) %>%
          req_headers(
            "apikey" = Sys.getenv("apikey"),
            "Authorization" = paste("Bearer", Sys.getenv("apikey")),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(list(emp_id_param = input$emp_id, daterange = monthSeq$date)) %>%
          req_perform() %>%
          resp_body_json() %>% 
          map(unlist) %>% 
          map(~ as.data.frame(t(.))) %>%
          bind_rows() %>% 
          subset(select = which(!duplicated(names(
            .
          )))) %>%
          mutate(date = as.Date(log_date)) %>%
          filter(floor_date(date, "month") == as.Date(input$date)) %>%
          mutate(date = as.character(log_date)) %>%
          select(
            date,
            in_location,
            in_time,
            out_location,
            out_time,
            emp_id,
            emp_name,
            designation,
            division
          ) %>%
          mutate(leave_type = "P") %>% 
          bind_rows(
            request(paste0(
              Sys.getenv("db_url"),
              "/rest/v1/rpc/get_employee_leave_details"
            )) %>%
              req_headers(
                "apikey" = Sys.getenv("apikey"),
                "Authorization" = paste("Bearer", Sys.getenv("apikey")),
                "Content-Type" = "application/json"
              ) %>%
              req_body_json(list(emp_id_param = input$emp_id, daterange = monthSeq$date)) %>%
              req_perform() %>%
              resp_body_json() %>% 
              map(unlist) %>% 
              map(~ as.data.frame(t(.))) %>%
              bind_rows() %>% 
              #mutate(date = as.Date(date)) %>% 
              subset(select = which(!duplicated(names(.)))) #%>%
            #filter(floor_date(date, "month") == as.Date(input$date)) %>%
            #select(date, emp_id, leave_type, designation, emp_name, division)
          ) %>% 
          select(
            date,
            leave_type,
            in_location,
            in_time,
            out_location,
            out_time,
            emp_id,
            emp_name,
            designation,
            division
          ) %>%
          mutate(date = as.Date(date)) %>% 
          arrange(emp_id, date) %>%
          rename("attendance" = "leave_type") %>%
          group_by(date, emp_id, designation, emp_name, division) %>%
          summarise(
            attendance = first(attendance),
            in_location = str_flatten_comma(in_location, na.rm = T),
            out_location = str_flatten_comma(out_location, na.rm = T),
            in_time = str_flatten_comma(in_time, na.rm = T),
            out_time = str_flatten_comma(out_time, na.rm = T)
          ) %>%
          ungroup()
        
        if (nrow(data) != 0) {
          data <- data %>%
            mutate(out_time = if_else(out_time == "0000-00-00 00:00:00", NA, out_time)) %>%
            mutate(
              entry = case_when(
                as.POSIXct(in_time, format = "%Y-%m-%d %H:%M:%S") <= as.POSIXct(paste0(date, entry_cut)) ~ "On time entry",
                as.POSIXct(in_time, format = "%Y-%m-%d %H:%M:%S") > as.POSIXct(paste0(date, entry_cut)) ~ "Late entry"
              )
            ) %>%
            mutate(
              exit = case_when(
                is.na(out_time) ~ "No exit",
                (
                  as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") >= as.POSIXct(paste0(date, exit_cut_wd))
                ) &
                  !(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "On time exit",
                (
                  as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") < as.POSIXct(paste0(date, exit_cut_wd))
                ) &
                  !(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "Early exit",
                (
                  as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") >= as.POSIXct(paste0(date, exit_cut_sat))
                ) &
                  (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "On time exit",
                (
                  as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") < as.POSIXct(paste0(date, exit_cut_sat))
                ) &
                  (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "Early exit"
              )
            ) %>%
            mutate(
              time = case_when(
                attendance != "P" ~ NA,
                entry == "On time entry" &
                  exit == "On time exit" ~ "On time",
                entry == "Late entry" &
                  exit == "On time exit" ~ "Late entry",
                entry == "On time entry" &
                  exit == "Early exit" ~ "Early exit",
                entry == "Late entry" &
                  exit == "Early exit" ~ "Late entry & early exit",
                entry == "On time entry" &
                  exit == "No exit" ~ "No exit",
                entry == "Late entry" &
                  exit == "No exit" ~ "Late entry & No exit"
              )
            ) %>%
            select(
              date,
              emp_id,
              emp_name,
              designation,
              division,
              attendance,
              time,
              in_time,
              in_location,
              out_time,
              out_location
            ) %>%
            unique() %>%
            mutate(
              in_time = format(
                as.POSIXct(in_time, format = "%Y-%m-%d %H:%M:%S"),
                "%I:%M %p"
              ),
              out_time = format(
                as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S"),
                "%I:%M %p"
              )
            ) %>%
            mutate(in_location = if_else(in_location == "", NA, in_location)) %>%
            mutate(out_location = if_else(out_location == "", NA, out_location))
        }
        
        holidays <-
          request("https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/rpc/get_sorted_distinct") %>%
          req_headers(
            "apikey" = Sys.getenv("apikey"),
            "Authorization" = paste("Bearer", Sys.getenv("apikey")),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(list(
            column_name = "hol_date",
            table_name = "nmcatt_api_holidays",
            sort_order = "ASC"
          )) %>%
          req_perform() %>%
          resp_body_json() %>% 
          map(unlist) %>% 
          map(~ as.data.frame(t(.))) %>%
          bind_rows() %>% 
          rename("date" = "distinct_value") %>%
          mutate(holiday = "H")
        
        dates <- seq(as.Date(input$date),
                     if (Sys.Date() > ceiling_date(as.Date(input$date), "month") -
                         1) {
                       ceiling_date(as.Date(input$date), "month") - 1
                     } else {
                       Sys.Date() - 1
                     },
                     "days")
        
        incProgress(1 / 5, detail = paste("Getting details"))
        
        if (request(
          paste0(
            "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
            "nmcatt_api_emp"
          )
        ) |>
        req_headers(
          "apikey" = Sys.getenv("apikey"),
          "Authorization" = paste("Bearer", Sys.getenv("apikey")),
          "Content-Type" = "application/json"
        ) %>%
        req_url_query(
          #"select" = "*, nmcatt_api_emp(*)",  # Fetch all fields and include related table
          "emp_id" = paste0("eq.",input$emp_id)  # Apply WHERE filter
        ) %>%
        req_perform() |>
        resp_body_json()  %>% 
        map(unlist) %>% 
        map(~ as.data.frame(t(.))) %>%
        bind_rows() %>% 
        count() %>%
        .$n != 0) {
          
          emp_dates <-
            request(
              paste0(
                "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
                "nmcatt_api_emp"
              )
            ) |>
            req_headers(
              "apikey" = Sys.getenv("apikey"),
              "Authorization" = paste("Bearer", Sys.getenv("apikey")),
              "Content-Type" = "application/json"
            ) %>%
            req_url_query(
              #"select" = "*, nmcatt_api_emp(*)",  # Fetch all fields and include related table
              "emp_id" = paste0("eq.",input$emp_id)  # Apply WHERE filter
            ) %>%
            req_perform() |>
            resp_body_json()  %>% 
            map(unlist) %>% 
            map(~ as.data.frame(t(.))) %>%
            bind_rows() %>%
            mutate(min_date = if_else(
              as.Date(creation_date) > min(dates),
              as.Date(creation_date),
              min(dates)
            )) %>%
            mutate(max_date = if_else(
              active_status == "A",
              max(dates),
              as.Date(
                data %>% filter(emp_id == input$emp_id) %>% summarise(date = max(date)) %>% .$date
              )
            )) %>%
            select(min_date, max_date)
          
          
          emp_seq <-
            data.frame(date = as.character(
              seq(emp_dates$min_date, emp_dates$max_date, by = "days")
            ))
        } else {
          emp_seq <-
            data.frame(date = as.character(seq(min(dates), max(dates), by = "days")))
        }
        
        emp_holiday <- holidays %>%
          filter(date %in% emp_seq$date) %>%
          mutate(emp_id = input$emp_id)
        
        
        if (nrow(data) == 0) {
          data <-
            bind_rows(
              request(
                paste0(
                  "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
                  "nmcatt_api_emp"
                )
              ) |>
                req_headers(
                  "apikey" = Sys.getenv("apikey"),
                  "Authorization" = paste("Bearer", Sys.getenv("apikey")),
                  "Content-Type" = "application/json"
                ) %>%
                req_url_query(
                  "active_status" = "eq.A",  
                  "emp_id" = paste0("eq.",input$emp_id)  
                ) %>%
                req_perform() |>
                resp_body_json()  %>% 
                map(unlist) %>% 
                map(~ as.data.frame(t(.))) %>%
                bind_rows() %>%
                #filter(!emp_id %in% unique(data$emp_id)) %>%
                select(emp_id, designation, emp_name, division) %>%
                mutate(date = as.Date(data_date()))
            ) %>%
            full_join(data.frame(date = as.Date(emp_seq$date))) %>%
            fill(emp_id, emp_name, designation, division) %>%
            mutate(
              attendance = "A",
              time = NA,
              in_time = NA,
              in_location = NA,
              out_time = NA,
              out_location = NA
            )
        }
        
        incProgress(1 / 5, detail = paste("Getting details"))
        
        emp_df <-
          data %>%
          filter(emp_id == input$emp_id) %>%
          full_join(emp_seq %>%
                      mutate(date = as.Date(date),
                             emp_id = input$emp_id),
                    by = join_by(date, emp_id)) %>%
          full_join(emp_holiday %>% mutate(date = as.Date(date)),
                    by = join_by(date, emp_id)) %>%
          mutate(holiday = if_else(wday(date) == 1, "H", holiday)) %>%
          arrange(date) %>%
          mutate(
            attendance = case_when(
              attendance == "EL" ~ "EL",
              holiday == "H" ~ "H",
              is.na(attendance) ~ "A",
              .default = attendance
            )
          ) %>%
          select(-holiday) %>%
          unique() %>%
          fill(c(emp_name, designation, division), .direction = "downup") %>%
          mutate(time = case_when(attendance != "P" ~ NA,
                                  .default = time))
        
        incProgress(1 / 5, detail = paste("Getting details"))
        
        #Ind calender
        
        ind_cal <- emp_df %>%
          filter(month(date) == month(input$date)) %>%
          full_join(monthSeq) %>%
          mutate(
            wday = wday(date),
            week = epiweek(date),
            day = day(date)
          ) %>%
          #collect() %>%
          select(week,
                 wday,
                 day,
                 attendance,
                 time,
                 in_time,
                 in_location,
                 out_time,
                 out_location) %>%
          pivot_wider(
            names_from = wday,
            values_from = c(
              attendance,
              day,
              time,
              in_time,
              out_time,
              in_location,
              out_location
            )
          ) %>%
          #arrange(week) %>% ## removed arrange as it was causing problems in December
          gt() %>%
          cols_merge(columns = ends_with("1"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_merge(columns = ends_with("2"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_merge(columns = ends_with("3"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_merge(columns = ends_with("4"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_merge(columns = ends_with("5"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_merge(columns = ends_with("6"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_merge(columns = ends_with("7"),
                     pattern = "<font size='2'><strong><<{2}>></strong></font> </br></br> <font size='4'><strong><<{1}>></strong></font></br> <font size='1'><strong><<{3}>></strong></font></br> <font size='1'><strong><<IN: {4}>> </br> <<{6}>></strong></font></br> <font size='1'><strong><<OUT: {5}>> </br> <<{7}>></strong></font>") %>%
          cols_label(
            "attendance_1" = "Sunday",
            "attendance_2" = "Monday",
            "attendance_3" = "Tuesday",
            "attendance_4" = "Wednesday",
            "attendance_5" = "Thursday",
            "attendance_6" = "Friday",
            "attendance_7" = "Saturday"
          ) %>%
          cols_move(
            columns = c(
              attendance_1,
              attendance_2,
              attendance_3,
              attendance_4,
              attendance_5,
              attendance_6,
              attendance_7
            ),
            after = week
          ) %>%
          cols_hide(week) %>%
          cols_width(starts_with("attend") ~ pct(10)) %>%
          cols_align("center",
                     columns = everything()) %>%
          tab_header(title = format(as.Date(input$date), "%B %Y")) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_1,
                                   rows = attendance_1 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_2,
                                   rows = attendance_2 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_3,
                                   rows = attendance_3 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_4,
                                   rows = attendance_4 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_5,
                                   rows = attendance_5 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_6,
                                   rows = attendance_6 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_body(columns = attendance_7,
                                   rows = attendance_7 == "P")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_1,
                                   rows = attendance_1 == "H")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_2,
                                   rows = attendance_2 == "H")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_3,
                                   rows = attendance_3 == "H")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_4,
                                   rows = attendance_4 == "H")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_5,
                                   rows = attendance_5 == "H")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_6,
                                   rows = attendance_6 == "H")
          ) %>%
          tab_style(
            style = cell_fill(color = "gray"),
            locations = cells_body(columns = attendance_7,
                                   rows = attendance_7 == "H")
          ) %>% tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_1,
                                   rows = time_1 != "On time")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_2,
                                   rows = time_2 != "On time")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_3,
                                   rows = time_3 != "On time")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_4,
                                   rows = time_4 != "On time")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_5,
                                   rows = time_5 != "On time")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_6,
                                   rows = time_6 != "On time")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightyellow"),
            locations = cells_body(columns = attendance_7,
                                   rows = time_7 != "On time")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_1,
                                   rows = attendance_1 == "A")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_2,
                                   rows = attendance_2 == "A")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_3,
                                   rows = attendance_3 == "A")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_4,
                                   rows = attendance_4 == "A")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_5,
                                   rows = attendance_5 == "A")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_6,
                                   rows = attendance_6 == "A")
          ) %>%
          tab_style(
            style = list(cell_fill(color = "#FF7F7F"),
                         cell_text(color = "white")),
            locations = cells_body(columns = attendance_7,
                                   rows = attendance_7 == "A")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_1,
              rows = attendance_1 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_2,
              rows = attendance_2 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_3,
              rows = attendance_3 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_4,
              rows = attendance_4 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_5,
              rows = attendance_5 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_6,
              rows = attendance_6 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = attendance_7,
              rows = attendance_7 %in% c("SL", "RH", "CL", "T", "O", "EL", "ML")
            )
          ) %>%
          tab_style(
            style = cell_borders(
              sides = c("top", "bottom", "right"),
              color = "black",
              weight = px(1.5),
              style = "solid"
            ),
            locations = cells_body()
          ) %>%
          opt_table_outline(color = "black") %>%
          tab_options(
            table.width = pct(95),
            column_labels.border.bottom.color = "black",
            column_labels.border.top.color = "black",
            heading.border.bottom.color = "black",
            table_body.border.bottom.color = "black"
          ) %>%
          tab_footnote(footnote = md(
            paste0(
              str_flatten_comma(
                emp_df %>% group_by(time) %>% count() %>% filter(!is.na(time)) %>% mutate(time = paste0("**", n, "** days ", time)) %>% .$time
              ),
              "<br><br>**P**: Present, **A**: Absent/Leave without Pay<br>**CL**: Casual Leave, **EL**: Earned Leave, **ML**: Medical Leave, **O**: Other Leave<br>**H**: Holiday, **RH**: Restricted Holiday, **T**: Tour, **SL**: Special Leave"
            )
          ))
        
        incProgress(1 / 5, detail = paste("Getting details"))
        
        ind_leave <- request(paste0(
          "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
          "nmcatt_api_leave"
        )) |>
          req_headers(
            "apikey" = Sys.getenv("apikey"),
            "Authorization" = paste("Bearer", Sys.getenv("apikey")),
            "Content-Type" = "application/json"
          ) %>%
          req_url_query("emp_id" = paste0(
            "eq.",
            str_pad(
              input$emp_id,
              width = 8,
              side = c("left"),
              pad = "0",
              use_width = TRUE
            )
          )) %>%
          req_perform() |>
          resp_body_json()  %>%
          map(unlist) %>%
          map( ~ as.data.frame(t(.))) %>%
          bind_rows() %>%
          mutate(year = year(date)) %>%
          collect() %>%
          rename("attendance" = leave_type) %>%
          select(emp_id, attendance, year) %>%
          group_by(year, attendance) %>%
          mutate(
            attendance = case_when(
              attendance == "CL" ~ "Casual Leave",
              attendance == "EL" ~ "Earned Leave",
              attendance == "SL" ~ "Special Leave",
              attendance == "ML" ~ "Medical Leave",
              attendance == "RH" ~ "Restricted Holiday",
              attendance == "O" ~ "Other",
              attendance == "T" ~ "Tour"
            )
          ) %>%
          summarise(n = n()) %>%
          pivot_wider(names_from = attendance,
                      values_from = n) %>%
          ungroup() %>%
          select(one_of(
            c(
              "year",
              "Casual Leave",
              "Earned Leave",
              "Special Leave",
              "Medical Leave",
              "Restricted Holiday",
              "Other",
              "Tour"
            )
          )) %>%
          arrange(desc(year)) %>% 
          gt(rowname_col = "year") %>%
          tab_header(title = "Leave Tally") %>%
          sub_missing(missing_text = "0") %>% 
          cols_width(one_of(
            c(
              "year",
              "Casual Leave",
              "Earned Leave",
              "Special Leave",
              "Medical Leave",
              "Restricted Holiday",
              "Other",
              "Tour"
            )
          ) ~ pct(5)) %>%
          cols_align("center", one_of(
            c(
              "year",
              "Casual Leave",
              "Earned Leave",
              "Special Leave",
              "Medical Leave",
              "Restricted Holiday",
              "Other",
              "Tour"
            )
          )) %>%
          tab_options(
            table.width = pct(95),
            column_labels.border.bottom.color = "black",
            column_labels.border.top.color = "black",
            heading.border.bottom.color = "black",
            table_body.border.bottom.color = "black"
          ) %>%
          tab_style(
            style = cell_borders(
              sides = c("top", "bottom", "right", "left"),
              color = "black",
              weight = px(1.5),
              style = "solid"
            ),
            locations = cells_body()
          ) %>%
          opt_table_outline(color = "black")
      }
    )
    showModal(
      modalDialog(
        tagList(ind_cal,
                ind_leave),
        title = tagList(
          request(paste0(
            "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
            "nmcatt_api_emp","?select=emp_name"
          )) |>
            req_headers(
              "apikey" = Sys.getenv("apikey"),
              "Authorization" = paste("Bearer", Sys.getenv("apikey")),
              "Content-Type" = "application/json"
            ) %>%
            req_url_query("emp_id" = paste0(
              "eq.",
              str_pad(
                input$emp_id,
                width = 8,
                side = c("left"),
                pad = "0",
                use_width = TRUE
              )
            )) %>%
            req_perform() |>
            resp_body_json()  %>%
            map(unlist) %>%
            map( ~ as.data.frame(t(.))) %>%
            bind_rows(),
          div(style = "position: relative; float: right;",
              modalButton(
                label = "", icon = icon("close")
              ))
        ),
        footer = NULL,
        easyClose = TRUE,
        icon = icon("clipboard-user"),
        size = "l"
      )
    )
  })
  
  ##Sum table####
  
  deptF <- eventReactive(input$go, {
    if (input$dept == "All") {
      d <- department()
    } else{
      d <- c(input$dept, "A")
    }
    return(d)
  })
  
  dept <- eventReactive(input$go, {
    return(input$dept)
  })
  
  cat <- eventReactive(input$go, {
    return(input$cat)
  })
  
  data_date <- eventReactive(input$go, {
    return(input$date)
  })
  
  date_seq <- eventReactive(input$go, {
    datesq = as.character(seq(
      floor_date(as.Date(data_date())), as.POSIXlt(ceiling_date(as.Date(data_date(
      )), "month") - days(1)), by = "days"
    ))
    
    return(datesq)
  })
  
  
  observeEvent(input$go, {
    output$table <- render_gt({
      withProgress(
        message = paste0(
          'Getting attendance data for ',
          cat(),
          " of ",
          dept(),
          " department."
        ),
        value = 0,
        {
          data <-
            request("https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/rpc/get_attend_details") %>%
            req_headers(
              "apikey" = Sys.getenv("apikey"),
              "Authorization" = paste("Bearer", Sys.getenv("apikey")),
              "Content-Type" = "application/json"
            ) %>%
            req_body_json(list(depart_param = deptF(), 
                               cat_param = cat(),
                               daterange = date_seq()
            )) %>%
            req_perform() %>%
            resp_body_json() %>% 
            map(unlist) %>% 
            map(~ as.data.frame(t(.))) %>%
            bind_rows() %>%
            subset(select = which(!duplicated(names(
              .
            )))) %>%
            mutate(date = log_date) %>%
            #filter(floor_date(date, "month") == as.Date(data_date())) %>%
            select(
              date,
              in_device_id,
              in_time,
              out_device_id,
              out_time,
              emp_id,
              emp_name,
              designation,
              division
            ) %>%
            mutate(
              leave_type = "P"
            ) %>% 
            bind_rows(
              request("https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/rpc/get_leave_details") %>%
                req_headers(
                  "apikey" = Sys.getenv("apikey"),
                  "Authorization" = paste("Bearer", Sys.getenv("apikey")),
                  "Content-Type" = "application/json"
                ) %>%
                req_body_json(list(depart_param = deptF(),
                                   cat_param = cat(),
                                   daterange = date_seq())) %>%
                req_perform() %>%
                resp_body_json() %>% 
                map(unlist) %>% 
                map(~ as.data.frame(t(.))) %>%
                bind_rows() %>%
                subset(select = which(!duplicated(names(.)))) #%>%
              #mutate(date = as.Date(date)) %>% 
              #filter(floor_date(date, "month") == as.Date(data_date())) %>%
              # select(date,
              #        emp_id,
              #        leave_type,
              #        designation,
              #        emp_name,
              #        division)
            ) %>%
            select(
              date,
              leave_type,
              in_device_id,
              in_time,
              out_device_id,
              out_time,
              emp_id,
              emp_name,
              designation,
              division
            ) %>%
            mutate(date = as.Date(date)) %>% 
            arrange(emp_id, date) %>%
            rename("attendance" = "leave_type")
          
          data <- data %>%
            bind_rows(
              request(
                paste0(
                  "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
                  "nmcatt_api_emp"
                )
              ) |>
                req_headers(
                  "apikey" = Sys.getenv("apikey"),
                  "Authorization" = paste("Bearer", Sys.getenv("apikey")),
                  "Content-Type" = "application/json"
                ) %>%
                req_url_query(
                  "cat" = paste0("eq.",cat()),
                  "division" = paste0("eq.",dept()) 
                ) %>%
                req_perform() |>
                resp_body_json()  %>% 
                map(unlist) %>% 
                map(~ as.data.frame(t(.))) %>%
                bind_rows() %>%
                filter(!emp_id %in% unique(data$emp_id)) %>%
                select(emp_id, designation, emp_name, division) %>%
                mutate(date = as.Date(data_date()))
            )
          
          holidays <-
            request("https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/rpc/get_sorted_distinct") %>%
            req_headers(
              "apikey" = Sys.getenv("apikey"),
              "Authorization" = paste("Bearer", Sys.getenv("apikey")),
              "Content-Type" = "application/json"
            ) %>%
            req_body_json(list(
              column_name = "hol_date",
              table_name = "nmcatt_api_holidays",
              sort_order = "ASC"
            )) %>%
            req_perform() %>%
            resp_body_json() %>% 
            map(unlist) %>% 
            map(~ as.data.frame(t(.))) %>%
            bind_rows() %>% 
            rename("date" = "distinct_value") %>%
            mutate(holiday = "H")
          
          
          dates <- seq(as.Date(data_date()),
                       if (Sys.Date() > ceiling_date(as.Date(data_date()), "month") -
                           1) {
                         ceiling_date(as.Date(data_date()), "month") - 1
                       } else {
                         Sys.Date() - 1
                       },
                       "days")
          
          full_df <-  data.frame()
          
          for (i in unique(data$emp_id)) {
            if (request(
              paste0(
                "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
                "nmcatt_api_emp","?select=creation_date"
              )
            ) |>
            req_headers(
              "apikey" = Sys.getenv("apikey"),
              "Authorization" = paste("Bearer", Sys.getenv("apikey")),
              "Content-Type" = "application/json"
            ) %>%
            req_url_query(
              "emp_id" = paste0("eq.",i)
            ) %>%
            req_perform() |>
            resp_body_json()  %>% 
            map(unlist) %>% 
            map(~ as.data.frame(t(.))) %>%
            bind_rows() %>% 
            .$creation_date > max(dates))
            {
              NA
            } else {
              if (request(
                paste0(
                  "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
                  "nmcatt_api_emp"
                )
              ) |>
              req_headers(
                "apikey" = Sys.getenv("apikey"),
                "Authorization" = paste("Bearer", Sys.getenv("apikey")),
                "Content-Type" = "application/json"
              ) %>%
              req_url_query(
                "emp_id" = paste0("eq.",i)
              ) %>%
              req_perform() |>
              resp_body_json()  %>% 
              map(unlist) %>% 
              map(~ as.data.frame(t(.))) %>%
              bind_rows() %>% 
              count() %>%
              .$n != 0) {
                emp_dates <-
                  request(
                    paste0(
                      "https://hocwxloxuwuvozbeyepc.supabase.co/rest/v1/",
                      "nmcatt_api_emp"
                    )
                  ) |>
                  req_headers(
                    "apikey" = Sys.getenv("apikey"),
                    "Authorization" = paste("Bearer", Sys.getenv("apikey")),
                    "Content-Type" = "application/json"
                  ) %>%
                  req_url_query(
                    "emp_id" = paste0("eq.",i)
                  ) %>%
                  req_perform() |>
                  resp_body_json()  %>% 
                  map(unlist) %>% 
                  map(~ as.data.frame(t(.))) %>%
                  bind_rows() %>%
                  mutate(min_date = if_else(
                    as.Date(creation_date) > min(dates),
                    as.Date(creation_date),
                    min(dates)
                  )) %>%
                  mutate(max_date = if_else(
                    active_status == "A",
                    max(dates),
                    as.Date(
                      data %>% filter(emp_id == i) %>% summarise(date = max(date)) %>% .$date
                    )
                  )) %>%
                  select(min_date, max_date)
                
                emp_seq <-
                  data.frame(date = as.character(
                    seq(emp_dates$min_date, emp_dates$max_date, by = "days")
                  ))
              } else {
                emp_seq <-
                  data.frame(date = as.character(seq(
                    min(dates), max(dates), by = "days"
                  )))
              }
              
              emp_holiday <- holidays %>%
                filter(date %in% emp_seq$date) %>%
                mutate(emp_id = i)
              
              emp_df <-
                data %>%
                filter(emp_id == i) %>%
                full_join(emp_seq %>%
                            mutate(date = as.Date(date),
                                   emp_id = i),
                          by = join_by(date, emp_id)) %>%
                full_join(emp_holiday %>% mutate(date = as.Date(date)),
                          by = join_by(date, emp_id)) %>%
                mutate(holiday = if_else(wday(date) == 1, "H", holiday)) %>%
                arrange(date) %>%
                mutate(
                  attendance = case_when(
                    attendance == "EL" ~ "EL",
                    holiday == "H" ~ "H",
                    is.na(attendance) ~ "A",
                    .default = attendance
                  )
                ) %>%
                select(-holiday) %>%
                unique() %>%
                fill(c(emp_name, designation, division), .direction = "downup") %>%
                group_by(date, emp_id, designation, emp_name, division) %>%
                summarise(
                  attendance = first(attendance),
                  in_time = str_flatten_comma(in_time, na.rm = T),
                  out_time = str_flatten_comma(out_time, na.rm = T)
                ) %>%
                ungroup()
              
              
              full_df <- full_df %>% bind_rows(emp_df)
              
              incProgress(1 / length(unique(data$emp_id)),
                          detail = paste("Getting details for ", unique(emp_df$emp_name)))
            }
          }
          
          if (nrow(full_df != 0)) {
            full_df <- full_df %>%
              mutate(out_time = if_else(out_time == "0000-00-00 00:00:00", NA, out_time)) %>%
              mutate(
                entry = case_when(
                  as.POSIXct(in_time, format = "%Y-%m-%d %H:%M:%S") <= as.POSIXct(paste0(date, entry_cut)) ~ "On time entry",
                  as.POSIXct(in_time, format = "%Y-%m-%d %H:%M:%S") > as.POSIXct(paste0(date, entry_cut)) ~ "Late entry"
                )
              ) %>%
              mutate(
                exit = case_when(
                  is.na(out_time) ~ "No exit",
                  (
                    as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") >= as.POSIXct(paste0(date, exit_cut_wd))
                  ) &
                    !(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "On time exit",
                  (
                    as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") < as.POSIXct(paste0(date, exit_cut_wd))
                  ) &
                    !(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "Early exit",
                  (
                    as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") >= as.POSIXct(paste0(date, exit_cut_sat))
                  ) &
                    (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "On time exit",
                  (
                    as.POSIXct(out_time, format = "%Y-%m-%d %H:%M:%S") < as.POSIXct(paste0(date, exit_cut_sat))
                  ) &
                    (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) ~ "Early exit"
                )
              ) %>%
              mutate(
                time = case_when(
                  attendance != "P" ~ NA,
                  entry == "On time entry" &
                    exit == "On time exit" ~ "On time",
                  entry == "Late entry" &
                    exit == "On time exit" ~ "Late entry",
                  entry == "On time entry" &
                    exit == "Early exit" ~ "Early exit",
                  entry == "Late entry" &
                    exit == "Early exit" ~ "Late entry & early exit",
                  entry == "On time entry" &
                    exit == "No exit" ~ "No exit",
                  entry == "Late entry" &
                    exit == "No exit" ~ "Late entry & No exit"
                )
              ) %>% 
              mutate(attendance = case_when(
                attendance == "P" & entry == "Late entry" ~ "Late",
                .default = attendance
              )) %>% 
              group_by(emp_id, emp_name, designation, division, attendance) %>%
              count() %>%
              pivot_wider(names_from = attendance,
                          values_from = n) %>%
              mutate(across(everything(),  ~ replace_na(., 0)))
            
            col <-
              c("A", "P", "H", "RH", "CL", "EL", "SL", "ML", "O", "T")[!c("A", "P", "H", "RH", "CL", "EL", "SL", "ML", "O", "T") %in% names(full_df)]
            
            for (i in col) {
              full_df[[i]] <- 0
            }
            
            full_df <- full_df %>%
              mutate(total_days = rowSums(across(where(
                is.numeric
              )))) %>%
              mutate(across(everything(),  ~ replace_na(., 0))) %>%
              mutate(present_days = total_days - A) %>%
              relocate(total_days, .after = last_col()) %>%
              mutate(across(where(is.numeric), as.integer)) %>%
              mutate(designation = factor(
                designation,
                levels = c(
                  "Professor",
                  "Associate Professor",
                  "Statistician",
                  "Assistant Professor",
                  "Senior Resident",
                  "Junior Resident",
                  "Tutor",
                  "Other"
                )
              )) %>%
              arrange(division, designation)
            
            output$downloadData <- downloadHandler(
              filename = function() {
                paste0(dept(),
                       "_",
                       cat(),
                       "_",
                       format(data_date(), "%B_%Y"),
                       ".csv")
              },
              content = function(file) {
                write.csv(full_df, file, row.names = FALSE)
              }
            )
          }
          if (nrow(full_df) > 0) {
            full_df %>%
              mutate(details = map(
                emp_id,
                .f = ~ details_btn(
                  inputId = "details",
                  onclick = paste0("Shiny.setInputValue('emp_id','", .x, "')")
                )
              )) %>%
              group_by(if (dept() == "All")
                division
                else
                  NULL) %>%
              gt(id = "nmcattend",
                 rowname_col = "emp_id") %>%
              tab_spanner(label = "Holiday",
                          columns = any_of(c("H", "RH"))) %>%
              tab_spanner(label = "Leave",
                          columns = any_of(c("CL", "EL", "SL", "ML", "O"))) %>%
              tab_spanner(label = "Duplicate Entry",
                          columns = contains(",")) %>%
              cols_label(
                "A" = "Absent",
                "P" = "Present",
                any_of("T") ~ "Tour",
                any_of("O") ~ "Other",
                "present_days" = "Present Days",
                "total_days" = "Total Days",
                "emp_id" = "Name",
                "emp_name" = "Name",
                "details" = "Details"
              ) %>%
              cols_hide(where( ~ is.numeric(.x) &&
                                 sum(.x, na.rm = T) == 0)) %>%
              cols_hide(c(division)) %>%
              cols_unhide(A) %>%
              cols_move_to_start(columns = any_of(c("P","Late"))) %>%
              cols_move_to_end(columns = any_of(c(
                "H", "RH", "A", "present_days", "total_days"
              ))) %>%
              tab_header(title = dept(),
                         subtitle = paste(cat(), "-", format(as.Date(
                           data_date()
                         ), "%b %Y"))) %>%
              cols_merge(columns = c(emp_id, emp_name, designation),
                         pattern = "{1} </br> <font size='4'><strong>{2}</strong></font> </br> <<{3}>>") %>%
              cols_width(any_of(
                c(
                  "A",
                  "P",
                  "Late",
                  "H",
                  "RH",
                  "CL",
                  "EL",
                  "SL",
                  "ML",
                  "O",
                  "T",
                  "present_days",
                  "total_days",
                  "details"
                )
              ) ~ pct(7),
              emp_id ~ pct(15)) %>%
              cols_align("center",
                         columns = any_of(
                           c(
                             "A",
                             "P",
                             "Late",
                             "H",
                             "RH",
                             "CL",
                             "EL",
                             "SL",
                             "ML",
                             "O",
                             "T",
                             "present_days",
                             "total_days",
                             "details"
                           )
                         )) %>%
              tab_style(
                style = cell_text(v_align = "middle", align = "center"),
                locations = cells_column_labels(columns = any_of(
                  c(
                    "P",
                    "A",
                    "Late",
                    "T",
                    "total_days",
                    "present_days",
                    "details"
                  )
                ))
              ) %>%
              tab_style(
                style = cell_fill(color = "#FF7F7F"),
                locations = cells_body(rows = `present_days` != `total_days`)
              ) %>%
              tab_style(
                style = cell_text(
                  size = "4",
                  align = "center",
                  weight = "bold"
                ),
                locations = cells_row_groups()
              ) %>%
              opt_align_table_header(align = c("center")) %>%
              cols_move_to_end(details) %>%
              tab_options(table.width = pct(95)) %>%
              tab_footnote(downloadButton("downloadData", "Download"))
          } else {
            data.frame(Error = paste0(
              "No ",
              cat(),
              " in ",
              dept(),
              " in ",
              format(data_date(), "%b %Y")
            )) %>% gt() %>%
              tab_options(table.width = pct(95))
          }
        }
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)