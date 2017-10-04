# Copyright (C) 2017 CannaData Solutions
#
# This file is part of CannaFrontdesk.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#' Frontdesk Shiny Application
#'
#' @import shiny CannaQueries shinyCleave rintrojs RMariaDB pool DT dplyr CannaModules CannaSelectize hms aws.s3 c3 jsonlite jose openssl httr base64enc
#' @importFrom tools file_ext
#' @importFrom tidyr replace_na spread_
#' @inheritParams CannaSignup::signup
#' @param bucket Name of AWS bucket
#' @export
#'

frontdesk <-
  function(pool = pool::dbPool(
    RMariaDB::MariaDB(),
    host = getOption("CannaData_host"),
    port = as.integer(getOption("CannaData_port")),
    user = getOption("CannaData_user"),
    password = getOption("CannaData_password"),
    db = getOption("CannaData_db")
  ), clientName = getOption("CannaData_clientName"),
           # host = getOption("CannaData_host"),
           # port = as.integer(getOption("CannaData_port")),
           # user = getOption("CannaData_user"),
           # password = getOption("CannaData_password"),
           base_url = getOption("CannaData_baseUrl"),
           # db = getOption("CannaData_db"),
           bucket = getOption("CannaData_AWS_bucket"),
  auth_id = getOption("auth_id"),
  auth_secret = getOption("auth_secret"),
  scope = "openid email",
  connection_name = "Username-Password-Authentication") {
    Sys.setenv("TWILIO_SID" = getOption("TWILIO_SID"),
               "TWILIO_TOKEN" = getOption("TWILIO_TOKEN"),
               "AWS_ACCESS_KEY_ID"=getOption("AWS_ACCESS_KEY_ID"),
               "AWS_SECRET_ACCESS_KEY"=getOption("AWS_SECRET_ACCESS_KEY"),
               "AWS_DEFAULT_REGION" = getOption("AWS_DEFAULT_REGION"))
    APP_URL <- paste0(base_url, "frontdesk/")
    
    # the scope of this variable is ui, and server for a single session I believe
    # so like global.R in that ui and server can access, but unlike global.R
    # in that you don't share state across sessions me thinks
    state <- ""
    
    access_token <- ""
    
    make_authorization_url <- function(req, APP_URL) {
      url_template <- paste0("https://cannadata.auth0.com/authorize?response_type=code&scope=%s&",
                             "state=%s&client_id=%s&redirect_uri=%s&connection=%s")
      redirect_uri <- APP_URL
      # save state in session global variable
      state <<- paste0(sample(c(LETTERS, 0:9), 12, replace =TRUE), collapse = "")
      sprintf(url_template,
              utils::URLencode(scope, reserved = TRUE, repeated = TRUE),
              utils::URLencode(base64enc::base64encode(charToRaw(state))),
              utils::URLencode(auth_id, reserved = TRUE, repeated = TRUE),
              utils::URLencode(redirect_uri, reserved = TRUE, repeated = TRUE),
              utils::URLencode(connection_name, reserved = TRUE, repeated = TRUE)
      )
    }
    
    ui <-
      function(req) {
        if (length(parseQueryString(req$QUERY_STRING)$code) == 0 && !interactive()) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("location.replace(\"%s\");", authorization_url)))))
        } else if (!interactive()) {
          params <- parseQueryString(req$QUERY_STRING)
          
          # this check may not be doing it's job but it seems to be
          if (rawToChar(base64enc::base64decode(params$state)) != state) {
            authorization_url <- make_authorization_url(req, APP_URL)
            return(tagList(
              tags$script(HTML(sprintf("location.replace(\"%s\");", authorization_url)))))
          }
          
          resp <- tryCatch(httr::POST("https://cannadata.auth0.com/oauth/token",
                                      body = list(
                                        grant_type = "authorization_code",
                                        client_id = auth_id,
                                        client_secret = auth_secret,
                                        code = params$code,
                                        redirect_uri = APP_URL
                                      ), httr::accept_json(),
                                      encode = "json"), error = function(e) NULL)
          
          if (httr::http_error(resp)) {
            authorization_url <- make_authorization_url(req, APP_URL)
            return(tagList(
              tags$script(HTML(sprintf("location.replace(\"%s\");", authorization_url)))))
          }
          
          respObj <- jsonlite::fromJSON(rawToChar(resp$content))
          
          # verify access token
          
          if (!isTruthy(respObj$id_token)) {
            authorization_url <- make_authorization_url(req, APP_URL)
            return(tagList(
              tags$script(HTML(sprintf("location.replace(\"%s\");", authorization_url)))))
          }
          
          kid <- jsonlite::fromJSON(rawToChar(base64decode(strsplit(respObj$id_token, "\\.")[[1]][1])))$kid
          
          keys <- jsonlite::fromJSON("https://cannadata.auth0.com/.well-known/jwks.json")$keys
          
          # 64 characters to a row
          key <- gsub("(.{64})", "\\1\n", keys[keys$kid == kid, ]$x5c)
          publicKey <- openssl::read_cert(paste("-----BEGIN CERTIFICATE-----", key, "-----END CERTIFICATE-----", sep = "\n"))$pubkey
          
          user <- tryCatch(jose::jwt_decode_sig(respObj$id_token, publicKey), error = function(e) NULL)
          
          if (is.null(user)) {
            authorization_url <- make_authorization_url(req, APP_URL)
            return(tagList(
              tags$script(HTML(sprintf("location.replace(\"%s\");", authorization_url)))))
          }
          
          access_token <<- respObj$access_token
          return(shiny::htmlTemplate(
            filename = system.file(package = "CannaFrontdesk", "templates", "template.html"),
            clientName = clientName
          ))
        } else {
          return(shiny::htmlTemplate(
            filename = system.file(package = "CannaFrontdesk", "templates", "template.html"),
            clientName = clientName
          ))
        }
      }
      
    #     shiny::bootstrapPage(div(
    #        htmltools::htmlDependency("selectize", "0.11.2", c(href = "shared/selectize"), stylesheet = "css/selectize.bootstrap3.css", head = format(shiny::tagList(shiny::HTML("
    # <!--[if lt IE 9]>"),
    #                                                                                                                                                                   shiny::tags$script(src = "shared/selectize/js/es5-shim.min.js"),
    #                                                                                                                                                                   shiny::HTML("<![endif]-->"), shiny::tags$script(src = "shared/selectize/js/selectize.min.js")))),
    #        rintrojs::introjsUI(),
    #        parsleyr::parsleyLib(),
    #        shinyCleave::cleaveLib(),
    #        htmltools::htmlDependency( "CannaFrontdesk", "0.0.1", system.file(package = "CannaFrontdesk", "www"), script = "script.js", stylesheet = "style.css", attachment = c("din_light.ttf", "din_medium.ttf") ),
    #        parsleyr::parsleyrCSS(),
    #        shiny::includeScript(system.file(package = "CannaSelectize", "javascript", "CannaSelectize.js")),
    #        htmltools::htmlDependency("font-awesome","4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css"),
    #        navbarUI(clientName),
    #        div(id = "content",
    #            shiny::navlistPanel(id = "tabset", well=FALSE,
    #                                shiny::tabPanel("homepage",CannaFrontdesk:::queueUI("frontdesk")),
    #                                shiny::tabPanel("patientInfo",CannaFrontdesk:::patientInfoUI("patient_info")),
    #                                shiny::tabPanel("newPatient", CannaFrontdesk:::newPatientUI("new_patient")))
    #            )
    #     ))
    
    server <- function(input, output, session) {
      params <- parseQueryString(isolate(session$clientData$url_search))
      
      if (length(params$code) == 0 && !interactive()) { 
        return()
      }
      
      if (!interactive()) {
        user <- jsonlite::fromJSON(rawToChar(httr::GET(
          httr::modify_url("https://cannadata.auth0.com/", path = "userinfo/",
                           query = list(access_token = access_token))
        )$content))
        
      }
      
      output$user_name <- renderUI({
        req(user)
        tagList(
          p(class = "navbar-text",
            user$email
          )
        )
      })
    
      if (interactive()) {
        session$onSessionEnded(stopApp)
      }
      options(shiny.maxRequestSize = 300 * 1024 ^ 2)
      ## input names may change!!!
      # REACTIVES ---------------------------------------------------------------
      trigger_new <- reactiveVal(0)
      trigger_returning <- reactiveVal(0)
      patients <- reactive({
        trigger_returning()
        trigger_new()
        q_f_patients_select(pool)
      })
      
      # has button that can update queue module
      patient_info <-
        callModule(
          patientInfo,
          "patient_info",
          pool,
          reactive({
            if (isTruthy(input$patient) &&
                isTRUE(patients()$verified[patients()$idpatient == as.numeric(input$patient)] == 3)) {
              as.numeric(input$patient)
            } else {
              NULL
            }
          }),
          bucket,
          reactive(queue()),
          trigger,
          reload,
          trigger_new,
          trigger_returning,
          patient_proxy,
          reload_patient,
          trigger_patients
        )
      # needs to update outer selectize
      patient_info_new <-
        callModule(
          newPatient,
          "new_patient",
          pool,
          reactive({
            if (isTruthy(input$patient) && isTRUE(patients()$verified[patients()$idpatient == as.numeric(input$patient)] %in% c(1,2))) {
              as.numeric(input$patient)
            } else {
              NULL
            }
          }),
          bucket,
          trigger_new,
          trigger_returning,
          patient_proxy,
          reload_patient,
          trigger_patients
        )
      
      trigger <- reactiveVal(0)
      reload <- reactiveVal(0)
      queue <-
        callModule(
          queue,
          "frontdesk",
          pool,
          patient_proxy,
          trigger,
          reload,
          reload_patient,
          trigger_patients
        )
      trigger_patients <- reactiveVal(0)
      all_patients <-
        callModule(allPatients,
                   "all_patients",
                   pool,
                   reload_patient,
                   trigger_patients)
      
      # OBSERVES ---------------------------------------------------------------
      
      observeEvent(input$patient, {
        req(input$patient)
        if (patients()$verified[patients()$idpatient == as.numeric(input$patient)] == 3)
          updateNavlistPanel(session, "tabset", "patientInfo")
        else
          updateNavlistPanel(session, "tabset", "newPatient")
      })
      
      # server side selectize inputs
      patient_proxy <- selectizeProxy("patient")
      reload_patient <- reactiveVal(NULL)
      observeEvent(reload_patient(), {
        req(reload_patient())
        # check for new patients anytime we update selectize
        # not needed for now unless we determine it is needed
        # trigger_new(trigger_new() + 1)
        updateSelectizeInput(
          session,
          "patient",
          choices = bind_rows(
            patients()[patients()$idpatient == reload_patient()$selected,],
            patients()[!(patients()$idpatient == reload_patient()$selected),]
          ),
          server = TRUE,
          selected = reload_patient()$selected
        )
      })
      
      observe({
        updateSelectizeInput(session,
                             "patient",
                             choices = isolate(patients()),
                             server = TRUE)
      })
      
      # id scanner
      observeEvent(input$read_barcode, {
        req(input$read_barcode)
        # PDF417
        if (any(input$read_barcode$californiaId %in% patients()$californiaID)) {
          status <-
            patients()$verified[patients()$californiaID == input$read_barcode$californiaId]
          if (status %in% 1:2) {
            showModal(modalDialog(
              h1("New Patient!"),
              tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
              easyClose = TRUE,
              tags$script(
                "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
              ),
              h2(
                paste(
                  input$read_barcode$firstName,
                  input$read_barcode$lastName,
                  "is ready to fill out the signup form."
                )
              )
            ))
            reload_patient(list(selected = patients()$idpatient[input$read_barcode$californiaId == patients()$californiaID], 
                                time = Sys.time()))
          } else if (status == 3) {
            reload_patient(list(selected = patients()$idpatient[input$read_barcode$californiaId == patients()$californiaID], 
                                time = Sys.time()))
          }
        } else {
          showModal(modalDialog(
            easyClose = TRUE,
            tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
            h1("New Patient!"),
            tags$script(
              "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
            ),
            h2(
              paste(
                "Click below to add",
                input$read_barcode$firstName,
                input$read_barcode$lastName
              )
            ),
            footer = actionButton("add_new_patient", "Add New Patient", class = "btn btn-info add-queue-btn")
          ))
        }
      })
      
      observeEvent(input$add_new_patient, {
        req(input$read_barcode)
        removeModal()
        i_f_new_patient(
          pool,
          input$read_barcode$californiaId,
          paste0(substr(input$read_barcode$expirationDate,1,2), "/",
                 substr(input$read_barcode$expirationDate,3,4),"/",
                 substr(input$read_barcode$expirationDate,5,8)),
          input$read_barcode$firstName,
          input$read_barcode$lastName,
          input$read_barcode$middleName,
          paste0(substr(input$read_barcode$birthday,1,2), "/",
                 substr(input$read_barcode$birthday,3,4),"/",
                 substr(input$read_barcode$birthday,5,8)),
          input$read_barcode$address,
          input$read_barcode$city,
          substr(input$read_barcode$zip, 1, 5)
        )
        
        trigger_new(trigger_new() + 1)
        reload_patient(list(selected = patients()$idpatient[input$read_barcode$californiaId %in% patients()$californiaID], time = Sys.time()))

        showModal(modalDialog(
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1(
            paste(
              "Sign in app is ready for",
              input$read_barcode$firstName,
              input$read_barcode$lastName
            )
          )
        ))
      })
      
    }
    
    shiny::shinyApp(ui = ui, server = server)
    
  }
