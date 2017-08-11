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
#' @import shiny CannaQueries shinyCleave rintrojs RMariaDB pool DT dplyr CannaModules CannaSelectize hms aws.s3
#' @importFrom tools file_ext
#' @importFrom tidyr replace_na
#' @inheritParams CannaSignup::signup
#' @param bucket Name of AWS bucket
#' @param budtender Name of user
#' @export
#'

frontdesk <-
  function(clientName = Sys.getenv("CannaData_clientName"),
           host = Sys.getenv("CannaData_host"),
           port = as.integer(Sys.getenv("CannaData_port")),
           user = Sys.getenv("CannaData_user"),
           password = Sys.getenv("CannaData_password"),
           base_url = Sys.getenv("CannaData_baseUrl"),
           db = Sys.getenv("CannaData_db"),
           bucket = Sys.getenv("CannaData_AWS_bucket"),
           budtender = Sys.getenv("SHINYPROXY_USERNAME")) {
    ui <-
      shiny::htmlTemplate(filename = system.file(package = "CannaFrontdesk", "templates", "template.html"),
                          clientName = clientName)
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
      session$onSessionEnded(stopApp)
      options(shiny.maxRequestSize = 300 * 1024 ^ 2)
      # create pool on launch
      pool <- pool::dbPool(
        RMariaDB::MariaDB(),
        host = host,
        port = as.integer(port),
        user = user,
        password = password,
        db = db
      )
      ## input names may change!!!
      # REACTIVES ---------------------------------------------------------------
      # returning patients
      trigger_returning <- reactiveVal(0)
      returning_patients <- reactive({
        trigger_returning()
        q_f_returning_patients(pool)
      })
      # process for selectize
      returning_select <- reactive({
        req(returning_patients())
        if (nrow(returning_patients()) > 0) {
          x <- returning_patients()$idpatient
          names(x) <- returning_patients()$name
          return(x)
        } else {
          return(character(0))
        }
      })

      # new patients
      trigger_new <- reactiveVal(0)
      new_patients <- reactive({
        trigger_new()
        q_f_new_patients(pool)
      })
      # for selectize
      new_select <- reactive({
        req(new_patients())
        if (nrow(new_patients()) > 0) {
          x <- new_patients()$idpatient
          names(x) <- new_patients()$name
          return(x)
        } else {
          return(character(0))
        }
      })
      
      select_patient <- reactive(c(
        returning_select(),
        new_select()
      ))
      # has button that can update queue module
      patient_info <- callModule(patientInfo, "patient_info", pool, reactive({
        if (isTruthy(input$patient) && input$patient %in% returning_select()) {
        input$patient
        } else {
          NULL
        }
      }), bucket, reactive(queue()),
      trigger, reload, trigger_new, trigger_returning, patient_proxy, session, reload_patient
      )
      # needs to update outer selectize
      patient_info_new <- callModule(newPatient, "new_patient", pool, reactive({
        if (isTruthy(input$patient) && input$patient %in% new_select()) {
        input$patient
        } else {
          NULL
        }
      }), bucket, trigger_new, trigger_returning, patient_proxy, session, reload_patient)
      
      trigger <- reactiveVal(0)
      reload <- reactiveVal(0)
      queue <- callModule(queue, "frontdesk", pool, patient_proxy, trigger, reload, session)

      # OBSERVES ---------------------------------------------------------------
      
      observeEvent(input$patient, {
        req(input$patient)
        if (input$patient %in% returning_select())
          updateNavlistPanel(session, "tabset", "patientInfo")
        else
          updateNavlistPanel(session, "tabset", "newPatient")
      })
      
      # server side selectize inputs
      patient_proxy <- selectizeProxy("patient")
      reload_patient <- reactiveVal(NULL)
      observeEvent(reload_patient(),{
        req(reload_patient())
        updateSelectizeInput(session, "patient", choices = select_patient(), 
                             server = TRUE, 
                             selected = reload_patient()$selected)
      })
      
      observe({
        updateSelectizeInput(session, "patient", choices = isolate(select_patient()), 
                             server = TRUE)
      })

      # id scanner
      observeEvent(input$read_barcode, {
        req(input$read_barcode)
        # PDF417
        if (any(grepl(
          input$read_barcode$californiaId,
          returning_patients()$name
        ))) {
          updateNavlistPanel(session, "tabset", "patientInfo")
          update_value(patient_proxy, returning_patients()$idpatient[grepl(input$read_barcode$californiaId,
                                                                           returning_patients()$name)])
        } else if (any(grepl(input$read_barcode$californiaId, new_patients()$name))) {
          showModal(modalDialog(h1("New Patient!"),easyClose = TRUE,
                                tags$script(
                                  "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
                                ),
                                h2(
                                  paste(
                                    input$read_barcode$firstName,
                                    input$read_barcode$lastName,
                                    "is ready to fill out the signup form."
                                  )
                                )))
          updateNavlistPanel(session, "tabset", "newPatient")
          update_value(patient_proxy, new_patients()$idpatient[grepl(input$read_barcode$californiaId, new_patients()$name)])
        } else {
          showModal(modalDialog(easyClose = TRUE,
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
          input$read_barcode$firstName,
          input$read_barcode$lastName,
          input$read_barcode$middleName,
          paste0(
            substr(input$read_barcode$birthday, 5, 8),
            "/",
            substr(input$read_barcode$birthday, 1, 2),
            "/",
            substr(input$read_barcode$birthday, 3, 4)
          ),
          input$read_barcode$address,
          input$read_barcode$city,
          input$read_barcode$zip
        )
        updateNavlistPanel(session, "tabset", "newPatient")
        trigger_new(trigger_new() + 1)
        reload_patient(list(
          selected = new_patients()$idpatient[grepl(input$read_barcode$californiaId, new_patients()$name)]
        ))
        # updateSelectizeInput(session, "patient", choices = select_patient(),
        #                      selected = new_patients()$idpatient[grepl(input$read_barcode$californiaId, new_patients()$name)],
        #                      server = TRUE)
        showModal(modalDialog(
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(
          paste(
            "Sign in app is ready for",
            input$read_barcode$firstName,
            input$read_barcode$lastName
          )
        )))
      })
      
    }
    
    shiny::shinyApp(ui = ui, server = server)
    
  }
