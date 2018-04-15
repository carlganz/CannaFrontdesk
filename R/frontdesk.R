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
#' @import shiny CannaQueries shinyCleave rintrojs RMariaDB pool DT dplyr CannaModules CannaSelectize hms scales
#' @import aws.s3 c3 jsonlite jose openssl httr base64enc twilio googleAuthR googlePrintr DBI parallel docuSignr CannaMetrc
#' @importFrom tools file_ext
#' @importFrom tidyr replace_na spread_
#' @inheritParams CannaSignup::signup
#' @param bucket Name of AWS bucket
#' @param gcp_json Google Cloud Print Json
#' @param twilio_sid Twilio secret id
#' @param twilio_token Twilio token
#' @export
#'

frontdesk <-
  function(pool = pool::dbPool(
    RMariaDB::MariaDB(),
    host = getOption("CannaData_host"),
    port = as.integer(getOption("CannaData_port")),
    user = getOption("CannaData_user"),
    password = getOption("CannaData_password"),
    db = getOption("CannaData_db"),
    `ssl-ca` = "https://s3.amazonaws.com/rds-downloads/rds-combined-ca-bundle.pem",
    `ssl-verify` = "server-cert"
  ), clientName = getOption("CannaData_clientName"),
           # host = getOption("CannaData_host"),
           # port = as.integer(getOption("CannaData_port")),
           # user = getOption("CannaData_user"),
           # password = getOption("CannaData_password"),
           base_url = getOption("CannaData_baseUrl"),
           # db = getOption("CannaData_db"),
           bucket = getOption("CannaData_AWS_bucket"),
  gcp_json = getOption("CannaData_google_json"),
  auth_id = getOption("auth_id"),
  auth_secret = getOption("auth_secret"),
  scope = "openid email", twilio_sid = getOption("TWILIO_SID"),
  twilio_token = getOption("TWILIO_TOKEN"),
  connection_name = getOption("auth_connection"),
  state = getOption("CannaData_state")) {
    Sys.setenv("TWILIO_SID" = getOption("TWILIO_SID"),
               "TWILIO_TOKEN" = getOption("TWILIO_TOKEN"),
               "AWS_ACCESS_KEY_ID" = getOption("AWS_ACCESS_KEY_ID"),
               "AWS_SECRET_ACCESS_KEY" = getOption("AWS_SECRET_ACCESS_KEY"),
               "AWS_DEFAULT_REGION" = getOption("AWS_DEFAULT_REGION")
               )
    APP_URL <- paste0(base_url, "frontdesk/")
    
    access_token <- ""
    make_authorization_url <- function(req, APP_URL) {
      url_template <- paste0("https://cannadata.auth0.com/authorize?response_type=code&scope=%s&",
                             "state=%s&client_id=%s&redirect_uri=%s&connection=%s")
      redirect_uri <- APP_URL
      # save state in session global variable
      state <- paste0(sample(c(LETTERS, 0:9), 12, replace =TRUE), collapse = "")
      idfacility <- parseQueryString(req$QUERY_STRING)$idfacility
      if (length(idfacility) == 1) {
        redirect_uri <- paste0(redirect_uri, "?idfacility=", idfacility)
      }
      list(url = sprintf(url_template,
                         utils::URLencode(scope, reserved = TRUE, repeated = TRUE),
                         utils::URLencode(base64enc::base64encode(charToRaw(state))),
                         utils::URLencode(auth_id, reserved = TRUE, repeated = TRUE),
                         utils::URLencode(redirect_uri, reserved = TRUE, repeated = TRUE),
                         utils::URLencode(connection_name, reserved = TRUE, repeated = TRUE)
      ), state = state)
    }
    
    ui <- function(req) {
      if (length(parseQueryString(req$QUERY_STRING)$code) == 0 && !interactive()) {
        authorization_url <- make_authorization_url(req, APP_URL)
        return(tagList(
          tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                   document.cookie = cname + \"=\" + cvalue + \";path=/\";
      };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
      } else if (!interactive()) {
        params <- parseQueryString(req$QUERY_STRING)
        
        if (!isTRUE(rawToChar(base64enc::base64decode(params$state)) == parseCookies(req$HTTP_COOKIE)$cannadata_token)) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                     document.cookie = cname + \"=\" + cvalue + \";path=/\";
        };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
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
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                     document.cookie = cname + \"=\" + cvalue + \";path=/\";
        };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
        }
        
        respObj <- jsonlite::fromJSON(rawToChar(resp$content))
        
        # verify access token
        
        if (!isTruthy(respObj$id_token)) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                     document.cookie = cname + \"=\" + cvalue + \";path=/\";
        };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
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
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
    document.cookie = cname + \"=\" + cvalue + \";path=/\";
};setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
        }
        return(shiny::htmlTemplate(
          filename = system.file(package = "CannaFrontdesk", "templates", "template.html"),
          clientName = clientName, state = state, accessToken = respObj$access_token
        ))
      } else {
        return(shiny::htmlTemplate(
          filename = system.file(package = "CannaFrontdesk", "templates", "template.html"),
          clientName = clientName, state = state, accessToken = ""
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
    
    
    options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloudprint"))
    options("googleAuthR.ok_content_types" = c(getOption("googleAuthR.ok_content_types"),
                                               "text/plain"))
  
    msg_service_sid = tw_msg_service_list()[[1]]$sid
    Sys.setenv(docuSign_username = getOption("docuSign_username"))
    Sys.setenv(docuSign_password = getOption("docuSign_password"))
    Sys.setenv(docuSign_integrator_key = getOption("docuSign_integrator_key"))
    if (getOption("CannaData_state") %in% c("CO","OR", "MD")) {
      Sys.setenv(
        "metrc_software_key" = getOption("metrc_software_key"),
        "metrc_state" = getOption("metrc_state"),
        "metrc_demo" = getOption("metrc_demo")
      )
    }
    # # login on launch
    docu_log <- docuSignr::docu_login(demo = TRUE)
    
    server <- function(input, output, session) {
      params <- parseQueryString(isolate(session$clientData$url_search))
      access_token <- isolate(input$access_token)      
      if (length(params$code) == 0 && !interactive()) { 
        return()
      }
      
      googleAuthR::gar_auth_service(gcp_json,
                                    scope = c("https://www.googleapis.com/auth/cloudprint"))
      printers <- gcp_search("")
      
      if (!interactive()) {
        user <- jsonlite::fromJSON(rawToChar(httr::GET(
          httr::modify_url("https://cannadata.auth0.com/", path = "userinfo/",
                           query = list(access_token = access_token))
        )$content))
        
        budtenderId <- q_c_budtender(pool, if (isTruthy(user$email)) user$email else NA)
        
        if (budtenderId$frontdesk == 0) {
          showModal(
            modalDialog(
              tags$script("$('.modal-content').addClass('table-container');"),
              h1("You do not have access to this page."),
              footer = NULL
            )
          )
          return()
        } else {
          Sys.setenv("metrc_user_key" = budtenderId$apiKey)
        }
      } else {
        budtenderId = list(idbudtender = 1, apiKey = getOption("metrc_user_key"))
        Sys.setenv("metrc_user_key" = budtenderId$apiKey)
      }
      settings <- q_s_settings(pool)
      facility <- reactiveVal()
      employee_facilities <- q_s_employee_facilities(pool, budtenderId$idbudtender) %>%
        filter_(~retail == 1)
      if (nrow(employee_facilities) == 0) {
        showModal(
          modalDialog(
            tags$script("$('.modal-content').addClass('table-container');"),
            h1("You do not have access to a retail facility."),
            footer = tags$button(onclick = paste0("location.href='", base_url, "settings/","';"), "Settings", class = "btn btn-info add-queue-btn")
          )
        )
        return()
      } else if (isTruthy(params$idfacility) && params$idfacility %in% employee_facilities$idfacility) {
        facility(employee_facilities[employee_facilities$idfacility == params$idfacility,])
      } else if (isTruthy(params$idfacility) && !params$idfacility %in% employee_facilities$idfacility) {
        showModal(
          modalDialog(
            tags$script("$('.modal-content').addClass('table-container');"),
            h1("You do not have access to this facility."),
            footer = tags$button(onclick = paste0("location.href='", base_url, "settings/","';"), "Settings", class = "btn btn-info add-queue-btn")
          )
        )
        return()
      } else if (nrow(employee_facilities) == 1) {
        facility(employee_facilities)
      } else {
        showModal(
          modalDialog(
            tags$script("$('.modal-content').addClass('table-container');"),
            h1("Select Retail Facility", style = "margin-bottom:15px"),
            selectizeInput("retailer", NULL, choices = structure(isolate(employee_facilities())$idfacility, names = isolate(employee_facilities())$name),
                           options = list(onInitialize = I('function() {this.setValue("");}'))),
            footer =  actionButton("select_register", "Submit", class = "btn-success btn-add")
          )
        )
        
        observe({
          if (isTruthy(input$retailer)) {
            register(employee_facilities[employee_facilities$idfacility == input$retailer, ])
          } else {
            return()
          }
      })
      }
    
      if (interactive()) {
        session$onSessionEnded(stopApp)
      }
      max_points <- q_c_settings(pool)$points_total
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
      if (state != "OR-R") {
        patient_info <-
          callModule(
            patientInfo,
            "patient_info",
            pool,
            reactive({
              if (isTruthy(input$patient) && substr(input$patient, 1, 1) == "P" &&
                  isTRUE(patients()$verified[patients()$idpatient == as.numeric(substring(input$patient, 2))] == 3)) {
                as.numeric(substring(input$patient, 2))
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
            trigger_patients,
            max_points,
            base_url,
            msg_service_sid,
            facility
          )
      }
      
      # needs to update outer selectize
      if (state != "OR-R") {
      patient_info_new <-
        callModule(
          newPatient,
          "new_patient",
          pool,
          reactive({
            if (isTruthy(input$patient) && substr(input$patient, 1, 1) == "P" && isTRUE(patients()$verified[patients()$idpatient == as.numeric(substring(input$patient, 2))] %in% c(1,2))) {
              as.numeric(substring(input$patient, 2))
            } else {
              NULL
            }
          }),
          bucket,
          trigger_new,
          trigger_returning,
          patient_proxy,
          reload_patient,
          trigger_patients,
          msg_service_sid,
          base_url,
          docu_log,
          facility
        )
      }
      
      online <- reactiveVal(
        data.frame(
          idtransaction = integer(0),
          name = character(0),
          email = character(0),
          phone = integer(0),
          status = integer(0),
          items = integer(0),
          revenue = integer(0)
        )
      )
      online_load <- reactive({
        invalidateLater(5000)
        trigger_online()
        q_f_online(pool)
      })
      
      observe({
        if (!identical(online_load(), online())) {
          if (length(setdiff(online_load()$idtransaction[online_load()$status == 5], online()$idtransaction[online()$status == 5])) > 0) {
            # add
            new_ids <- setdiff(online_load()$idtransaction[online_load()$status == 5], online()$idtransaction[online()$status == 5])
            insertUI("#unconfirmed", "afterBegin", ui = 
            tagList(div(class = "unconfirmed-wrapper",
                            lapply(seq_len(length(new_ids)), function(x) {
                            div(
                              class = "order_alert", row = new_ids[x],
                              onclick = paste0("CannaFrontdesk.click_alert(", new_ids[x],")"),
                              tags$span(onclick = "CannaFrontdesk.close_alert(this);", class = "close-alert", icon("times")),
                              icon("exclamation-triangle", class = "fa-2x"),
                              tags$p(paste0("Unconfirmed order from ", online_load()$name[new_ids[x] == online_load()$idtransaction]))
                            )
                          }))))
          }
      
          if (length(setdiff(online()$idtransaction[online()$status == 5], online_load()$idtransaction[online_load()$status == 5])) > 0) {
            # remove
            lapply(setdiff(online()$idtransaction[online()$status == 5], online_load()$idtransaction[online_load()$status == 5]), function(x) {
              removeUI(paste0(".order_alert[row = \"", x, "\"]"))
            })
          }
          online(online_load())
        }
      })
      
      observe({
        req(input$click_alert)
        reload_patient(list(type = "Online", selected = input$click_alert$box, time = Sys.time()))
      })
      
      observe({
        req(input$close_alert)
        removeUI(paste0(".order_alert[row = \"", input$close_alert$box, "\"]"))
      })
      
      trigger <- reactiveVal(0)
      reload <- reactiveVal(0)
      trigger_online <- reactiveVal(0)
      queue <-
        callModule(
          queue,
          "frontdesk",
          pool,
          patient_proxy,
          trigger,
          reload,
          reload_patient,
          trigger_patients,
          trigger_online,
          online,
          state,
          patients,
          trigger_new,
          facility
        )
      trigger_patients <- reactiveVal(0)
      if (state != "OR-R") {
        all_patients <-
          callModule(allPatients,
                     "all_patients",
                     pool,
                     reload_patient,
                     trigger_patients)
      }
      
      callModule(onlineOrder, "online_order", pool, reactive({
        if (isTruthy(input$patient) && substr(input$patient, 1, 1) == "T") {
          as.numeric(substring(input$patient, 2))
        } else {
          NULL
        }
      }), {
        reactive(if (isTruthy(input$patient) && substr(input$patient, 1, 1) == "T") {
          online()[online()$idtransaction == as.numeric(substring(input$patient, 2)), ]
        } else {
          NULL
        })
      },
      trigger_online, reload_patient, reactive({
        structure(queue()$idtransaction, names = queue()$name)
      }), printers, base_url, msg_service_sid, clientName)
      
      # OBSERVES ---------------------------------------------------------------
      
      observeEvent(input$patient, {
        req(input$patient)
        if (substr(input$patient, 1, 1) == "P" && patients()$verified[patients()$idpatient == as.numeric(substring(input$patient, 2))] == 3)
          updateNavlistPanel(session, "tabset", "patientInfo")
        else if (substr(input$patient, 1, 1) == "P" && patients()$verified[patients()$idpatient == as.numeric(substring(input$patient, 2))] %in% 1:2)
          updateNavlistPanel(session, "tabset", "newPatient")
        else
          updateNavlistPanel(session, "tabset", "preOrders")
      })
      
      # server side selectize inputs
      patient_proxy <- selectizeProxy("patient")
      reload_patient <- reactiveVal(NULL)
      observeEvent(reload_patient(), {
        req(reload_patient())
        # check for new patients anytime we update selectize
        # not needed for now unless we determine it is needed
        # trigger_new(trigger_new() + 1)
        x <- bind_rows(
          if (state != "OR-R") {
            patients() %>%
            mutate_(selectGrp = ~ "patient")
            },
          online() %>%
            mutate_(selectGrp = ~ "online", label = ~name)
        ) %>% 
          mutate_(valueFld = ~if_else(selectGrp == "patient", paste0("P", idpatient), paste0("T", idtransaction)))
        
        updateSelectizeInput(
          session,
          "patient",
          choices = x,
          server = TRUE,
          selected = if (!is.null(reload_patient()$selected)) paste0(if (reload_patient()$type == "patient") "P" else "T", reload_patient()$selected)
        )
      })
      
      observe({
        updateSelectizeInput(session,
                             "patient",
                             choices = isolate(bind_rows(
                                 patients() %>%
                                 mutate_(selectGrp = ~ "patient") %>% {
                                   if (state == "OR-R") {
                                     filter_(., ~FALSE)
                                   } else {
                                    .
                                  }
                                 },
                               online() %>%
                                 mutate_(selectGrp = ~ "online", label = ~name)
          ) %>% 
            mutate_(valueFld = ~if_else(selectGrp == "patient", paste0("P", idpatient), paste0("T", idtransaction)))),
                             server = TRUE)
      })
      
      # id scanner
      observeEvent(input$read_barcode, {
        req(input$read_barcode)
        # PDF417
        if (state != "OR" && any(input$read_barcode$id %in% patients()$id & input$read_barcode$state %in% patients()$state)) {
          status <-
            patients()$verified[patients()$id == input$read_barcode$id & patients()$state == input$read_barcode$state]
          if (status %in% 1:2) {
            showModal(modalDialog(
              h1("New Customer!"),
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
            reload_patient(list(selected = patients()$idpatient[input$read_barcode$id == patients()$id], 
                                time = Sys.time(), type = "patient"))
          } else if (status == 3) {
            reload_patient(list(selected = patients()$idpatient[input$read_barcode$id == patients()$id], 
                                time = Sys.time(), type = "patient"))
          }
        } else {
          showModal(modalDialog(
            easyClose = TRUE,
            tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
            h1("New Customer!"),
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
            footer = tagList(
              if (isTRUE(facility()$recreational == 1)) actionButton("addRec", "Recreational", class = "btn btn-info add-queue-btn"),
                        if (isTRUE(facility()$medical == 1)) actionButton("addMed", "Medical", class = "btn btn-info add-queue-btn")        
                )
          ))
        }
      })
      
      observeEvent(input$addRec, {
          if (input$read_barcode$id %in% patients()$id) {
            id <- patients() %>% filter_(~id == input$store_id) %>% slice(1) %>% pull("idpatient")
          } else {
            con <- pool::poolCheckout(pool)
            i_f_new_patient(
              con,
              input$read_barcode$id,
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
              substr(input$read_barcode$zip, 1, 5),
              input$read_barcode$state,
              verified = 3
            )
            id <- last_insert_id(con)
            pool::poolReturn(con)
            trigger_patients(trigger_patients() + 1)
            trigger_returning(trigger_returning() + 1)
            trigger(trigger() + 1)
          }
  
          reload_patient(list(selected = id, time = Sys.time(), type = "patient"))
        
        removeModal()
      })
      
      observeEvent(input$addMed, {
        con <- pool::poolCheckout(pool)
        i_f_new_patient(
          con,
          input$read_barcode$id,
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
          substr(input$read_barcode$zip, 1, 5),
          input$read_barcode$state,
          verified = 2
        )
        id <- last_insert_id(con)
        pool::poolReturn(con)
        trigger_patients(trigger_patients() + 1)
        trigger_returning(trigger_returning() + 1)
        trigger(trigger() + 1)
        
        reload_patient(list(selected = id, time = Sys.time(), type = "patient"))
        
        removeModal()
      })
   
      # rintrojs
      
      observeEvent(input$help, {
        
        intro <- data.frame(
          element = c(NA, "#sidebar", "div[data-value='homepage'] > div > div:nth-child(1) > div > div", 
                      "div[data-value='homepage'] > div > div:nth-child(2) > div > div", "div[data-value='homepage'] > div > div:nth-child(3) > div > div",
                      "div[data-value='homepage'] .fa-plus",
                      "a[data-value='allPatients']", "div[data-value='allPatients'] > div > div:nth-child(1) > div > div", "div[data-value='allPatients'] > div > div:nth-child(2) > div > div",
                      "a[data-value='patientInfo']", "div[data-value='patientInfo'] > div > div:nth-child(1) > div > div", "div[data-value='patientInfo'] > div > div:nth-child(3) > div:nth-child(1) > div",
                      "div[data-value='patientInfo']  > div > div:nth-child(2) > div:nth-child(2) > div",
                      "div[data-value='patientInfo'] > div > div:nth-child(2) > div:nth-child(2) > div > div.table-title-and-icon > i",
                      "div[data-value='patientInfo']  > div > div:nth-child(3) > div:nth-child(2) > div",
                      "div[data-value='patientInfo']  > div > div:nth-child(3) > div:nth-child(3) > div",
                      "div[data-value='patientInfo']  > div > div:nth-child(2) > div:nth-child(3) > div", "div[data-value='patientInfo'] > div > div:nth-child(3) > div:nth-child(4) > div",
                      # "div[data-value='patientInfo']  > div > div:nth-child(2) > div:nth-child(4) > div", 
                      # "div[data-value='patientInfo']  > div > div:nth-child(3) > div:nth-child(5) > div",
                      "div[data-value='patientInfo'] > div > div:nth-child(3) > div:nth-child(5) > div",
                      "a[data-value='newPatient']", "#new_patient-newPatient > div:nth-child(1) > div:nth-child(2) > div",
                      "#new_patient-newPatient > div:nth-child(2) > div:nth-child(2)", "#new_patient-newPatient > div:nth-child(2) > div:nth-child(3)",
                      "#new_patient-newPatient > div:nth-child(1) > div:nth-child(3) > div", "a[data-value='preOrders']",
                      "#online_order-onlineSale > div:nth-child(1) > div.row > div", "#online_order-onlineSale > div:nth-child(2) > div:nth-child(2) > div",
                      "#online_order-onlineSale > div.col-xs-12.col-sm-12.col-md-12.col-lg-12 > div > div",
                      "#online_order-onlineSale > div:nth-child(2) > div:nth-child(1) > div"
                      ),
          intro = c(as.character(tagList(
            h1("CannaData Frontdesk Tour"),
            h3("The frontdesk application enables you to check-in new and returning patients. It is also where you process online orders."),
            h3("Lots of information about your customers is available in the front desk application. Allow us to show you...")
          )),
          as.character(tagList(
            h1("Sidebar"),
            "The sidebar provides access to five pages:",
            tags$ul(
              tags$li(tags$b("Homepage:"), "for viewing who is currently in store, and pending online orders."),
              tags$li(tags$b("All Patients:"), "for viewing all patients."),
              tags$li(tags$b("Patient Info:"), "when an individual patient is selected their information will be displayed here."),
              tags$li(tags$b("New Patient:"), "when processing a new patient you will enter their information in this page."),
              tags$li(tags$b("Online Sale:"), "for processing online orders.")
            )
          )),
          as.character(tagList(h1("Preorder"), "When customers make preorders online they will appear in this table.")),
          as.character(tagList(h1("Queue"), "This table displays which patients are waiting to enter the store. Patients in queue can be let into store by pressing the 'Let In' button next to the patient's name.")),
          as.character(tagList(h1("In Store"), "This table displays which patients are currently in the store. When customers complete their transaction they will automatically be removed from list.")),
          as.character(tagList(h1("Add"), "Customers can be added to queue or let into store by pressing the green plus sign. You can manually add new patients and customers as well.")),
          as.character(tagList(h1("All Patients"), "The all patients page allows you to view and sort your customers.")),
          as.character(tagList(h1("Incomplete Profiles"), "This table displays patients with incomplete profiles. A patient will have an incomplete profile when you first add them into the system. We will discuss adding new patients in depth in a moment.")),
          as.character(tagList(h1("All Patients Table"), "This table displays all registered patients. You can sort by pressing column name.")),
          as.character(tagList(h1("Patient Info"), "When a patient is selected, this page will display information about the patient.")),
          as.character(tagList(h1("Patient Status"), "The current status of the patient's medical card is displayed clearly at the top.")),
          as.character(tagList(h1("Buttons"), "Patients with valid medical cards can be added to queue or let directly into store. You also have the option of deleting a patient, but this is uncommon.")),
          as.character(tagList(h1("Basic Customer Info"), "View basic information about a selected customer.")),
          as.character(tagList(h1("Edit Customer Info"), "Edit info by pressing the green pencil.")), 
          # as.character(tagList(h1("Medical Info"), "See information about the patient's medical card.")), 
          as.character(tagList(h1("Preferences"), "Understand which strains and products the customer likes.")),
          as.character(tagList(h1("Past Products"), "A pie chart indicating what types of products the customer has bought in the past.")),
          # as.character(tagList(h1("Images"), "View images of the patient's ID and medical recommendation.")), 
          as.character(tagList(h1("Notes"), "Keep notes about each customer.")), 
          as.character(tagList(h1("Reward Points"), "Check the number of reward points the customer has accumulated.")), 
          # as.character(tagList(h1("Preferences"), "Understand which strains and products the customer likes.")), 
          as.character(tagList(h1("Past Transactions"), "See more information about individual transactions by clicking on them.")), 
          as.character(tagList(h1("New Patients"), "To process a new patient you either scan their ID or manually add them on the homepage.")),
          as.character(tagList(h1("New Patient Info"), "When a new patient's incomplete profile is selected their information will appear here.")),
          as.character(tagList(h1("Enter Medical Info"), "Here is where you enter the information from the patient's medical card.")),
          as.character(tagList(h1("Upload Images"), "After you scan an image of the patient's ID and medical card, you will upload the files here.")),
          as.character(tagList(h1("Images"), "When an image is uploaded it will appear here.")),
          as.character(tagList(h1("Preorders"), "Processing preorders involves two steps:", 
                               tags$ol(tags$li("Confirming the preorder"), tags$li("Linking preorder to customer when they arrive")))),
          as.character(tagList(h1("Customer Info"), "Basic data about the person making the preorder is viewable here.")),
          as.character(tagList(h1("Order Info"), "Details about the transaction are available here.")),
          as.character(tagList(h1("Cart"), "The cart is viewable here. Items can be edited or removed, and discounts can be applied.")),
          as.character(tagList(h1("Buttons"), "Labels for the ordered items can be printed. The order can be confirmed, or cancelled with a message to the customer.",
                               "Once an order is confirmed, the confirmed button is replaced with a 'Link' button, which enables you to associate a preorder with a checked in patient."))
          )
        )
        
        introjs(session, options = list(steps=intro,
                                        showStepNumbers = FALSE,
                                        showBullets = FALSE),  
                events = list(
          "onbeforechange" = I("
if ((this._currentStep==2 || this._currentStep == 5) && !$('div[data-value=\"homepage\"]').hasClass('active')) {
        $('a[data-value=\"homepage\"]').tab('show');
} else if ((this._currentStep == 6 || this._currentStep == 8) && !$('div[data-value=\"allPatients\"]').hasClass('active')) {
        $('a[data-value=\"allPatients\"]').tab('show');
} else if ((this._currentStep == 9 || this._currentStep == 18)) {
        $('a[data-value=\"patientInfo\"]').tab('show');
        // select patient
var x=$('#patient').selectize();
    var select = x[0].selectize;
    var options = select.options;
    var option = Object.keys(options).map(function(value) {
        return options[value]
      }).filter(function(value) {
        return value.verified === 3
      })[0];
if (option) {
select.setValue(option.valueFld);
}
} else if ((this._currentStep == 19 || this._currentStep == 23) && !$('div[data-value=\"newPatient\"]').hasClass('active')) {
    $('a[data-value=\"newPatient\"]').tab('show');
} else if (this._currentStep == 24 && !$('div[data-value=\"preOrders\"]').hasClass('active')) {
  $('a[data-value=\"preOrders\"]').tab('show');
        // select preorder
var x=$('#patient').selectize();
    var select = x[0].selectize;
    var options = select.options;
    var option = Object.keys(options).map(function(value) {
        return options[value]
      }).filter(function(value) {
        return value.status === 5 || value.status === 6;
      })[0];

if (option) {
select.setValue(option.valueFld);
}
  }
")
        ))
        
      })
      
    }
    
    shiny::shinyApp(ui = ui, server = server)
    
  }
