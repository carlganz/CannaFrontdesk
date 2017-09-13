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

patientInfoUI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(div(
    class = "content",
    div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
        div(
          class = "row",
          div(class = "countdown-container notexpired",
              uiOutput(ns("expiration")))
        )),
    div(
      class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
      div(class = "row",
          div(class = "name-container",
              uiOutput(ns(
                "name"
              )))),
      box(tableTitle("Basic Info"),
          DT::dataTableOutput(ns("info"))),
      box(tableTitle("Preferences"),
          DT::dataTableOutput(ns("preference"))),
      box(h1("Patient Data"),style = "overflow:hidden",
          div(style = "margin-top:8%",
              div(class = "col-sm-6",
                  h3("Patient Points"),
                  c3Output(ns("patient_points")),
                  tags$script(
                    paste0(
                      '$("#', ns("patient_points"),'").css("zoom", "1.6").css("zoom","160%").css("-moz-transform","scale(1.6,1.6)").css("margin-top","-5%");'
                    )
                  )
              ),
              div(class = "col-sm-6",
                  h3("Product Types"),
                  uiOutput(ns("no_type"), TRUE),
                  c3Output(ns("patient_type"))
              )
          )
      )
    ),
    div(
      class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
      div(
        class = "row",
        div(
          class = "add-delete-btn-container",
          style = "width:100%;",
          div(
            tags$button(
              id = ns("remove"),
              "Delete Patient",
              class = "btn btn-info delete-btn action-button",
              style = "width:30%;"
            ),
            tags$button(
              id = ns("let_in"),
              "Let in Store",
              class = "btn btn-info add-queue-btn action-button",
              style = "width:30%;"
            ),
            tags$button(
              id = ns("add_queue"),
              "Add to Queue",
              class = "btn btn-info add-queue-btn action-button",
              style = "width:30%;"
            )
          )
        )
      ),
      box(tableTitle("Medical Info"),
          DT::dataTableOutput(ns("recommendation"))),
          box(
            class = "images",
            tableTitle("Images"),
            div(
              class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
              div(class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
                  h3("Photo ID"),
                  uiOutput(ns("id_image_out"))),
              div(class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
                  h3("Medical Card"),
                  uiOutput(ns(
                    "recommendation_image_out"
                  )))
            )
          ),
          box(h1("Patient History"),
              CannaModules::patientHistoryUI(ns("frontdesk")))
      )
    ))
}

patientInfo <-
  function(input,
           output,
           session,
           pool,
           patientId,
           bucket,
           queue,
           trigger_queue,
           reload,
           trigger_new,
           trigger_returning,
           proxy,
           parent_session,
           reload_patient,
           trigger_patients) {
    trigger_patient_info_returning <- reactiveVal(0)
    patient_info_returning <- reactive({
      req(patientId())
      trigger_patient_info_returning()
      q_f_patient_info(pool, patientId())
    })
    # days until expired
    expired <- reactive({
      req(patientId())
      difftime(patient_info_returning()$expirationDate, Sys.Date())
    })
    
    # add patient to queue
    observeEvent(input$add_queue, {
      req(patientId())
      if (expired() <= 0) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Patient's medical card is expired!")
        ))
      } else if (patientId() %in% queue()$idpatient) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(paste(
            "Patient already in",
            if (queue()$status[patientId() == queue()$idpatient] == 2)
              "store"
            else
              "queue"
          ))
        ))
      } else {
        i_f_add_queue(pool, patientId())
        trigger_queue(trigger_queue() + 1)
        trigger_patients(trigger_patients() + 1)
        reload(reload() + 1)
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Patient added to queue.")
        ))
      }
    })
    
    observeEvent(input$let_in, {
      req(patientId())
      if (expired() <= 0) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Patient's medical card is expired!")
        ))
      } else if (patientId() %in% queue()$idpatient) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(paste(
            "Patient already in",
            if (queue()$status[patientId() == queue()$idpatient])
              "store"
            else
              "queue"
          ))
        ))
      } else {
        i_f_let_in(pool, patientId())
        trigger_queue(trigger_queue() + 1)
        trigger_patients(trigger_patients() + 1)
        reload(reload() + 1)
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Patient has been let into store.")
        ))
      }
    })
    
    observeEvent(input$remove, {
      req(patientId())
      if (patientId() %in% queue()$idpatient) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(
            "Cannot remove patient while patient is in active transaction."
          )
        ))
      } else {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Are you sure you want to remove patient?"),
          h1("Data cannot be recovered once removed!"),
          footer = tags$button(id = session$ns("delete"), class = "action-button btn btn-info delete-btn", "Remove")
        ))
      }
    })
    
    observeEvent(input$delete, {
      d_f_patient(pool, patientId())
      # trigger reload of selectize
      trigger_new(trigger_new() + 1)
      trigger_returning(trigger_returning() + 1)
      trigger_patients(trigger_patients() + 1)
      removeModal()
      # updateSelectizeInput(parent_session, "patient", selected = "")
      reload_patient(list(selected = NULL))
    })
    
    observeEvent(input$edit_basic_info, {
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('form-horizontal col-lg-12');
            $('.modal-body').css('height', '400px').css('font-size','110%');"
          ),
          h1("Edit Basic Info"),
          # add parsley
          tags$form(
            id = session$ns("basic_info_form"),
            div(
              class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
              input(
                session$ns("name"),
                placeholder = "First",
                label_width = 4,
                value = patient_info_returning()$firstName,
                input_width = 8
              ),
              input(
                session$ns("name2"),
                placeholder = "Last",
                label_width = 4,
                value = patient_info_returning()$lastName,
                input_width = 8
              ),
              input(
                session$ns("californiaID"),
                type = "tel",
                placeholder = "California ID",
                label = "CA ID",
                label_width = 4,
                maxlength = 8,
                `data-parsley-californiaid` = I(""),
                value = patient_info_returning()$californiaID,
                input_width = 8
              ),
              input(
                session$ns("birthday"),
                placeholder = "DOB",
                label = "DOB",
                label_width = 4,
                `data-parsley-year` = I(""),
                `data-parsley-pattern` = "/^\\d{4}[\\/\\-](0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])$/",
                value = gsub("-", "/", patient_info_returning()$birthday),
                input_width = 8
              ),
              input(
                session$ns("phone"),
                type = "tel",
                placeholder = "Phone",
                label_width = 4,
                input_width = 8,
                `data-parsley-length` = "[12,14]",
                `data-parsley-pattern` = "/^[\\d\\+\\-\\.\\(\\)\\/\\s]*$/",
                value = paste(
                  substr(patient_info_returning()$phone, 1, 3),
                  substr(patient_info_returning()$phone, 4, 6),
                  substr(patient_info_returning()$phone, 7, 10)
                )
              ),
              tags$script(
                paste0(
                  'var cleave = new Cleave("#',
                  session$ns("phone"),
                  '", {
                  phone: true, phoneRegionCode: "us"
    })'
      )
              ),
      div(
        class = "controls form-group",
        style = "text-align: right;",
        tags$label(
          class = "checkbox col-sm-4",
          `for` = session$ns("textDeal"),
          "Text Deals"
        ),
        tags$input(
          type = "checkbox",
          id = session$ns("textDeal"),
          value = session$ns("textDeal"),
          name = session$ns("textDeal"),
          class = "col-sm-7"
        )
      )
                ),
      div(
        class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
        input(
          session$ns("address"),
          placeholder = "Address",
          label_width = 4,
          value = patient_info_returning()$address
        ),
        input(
          session$ns("city"),
          placeholder = "City",
          label_width = 4,
          value = patient_info_returning()$city
        ),
        input(
          session$ns("state"),
          placeholder = "State",
          label_width = 4,
          maxlength = 2,
          `data-parsley-length` = "[2,2]",
          value = patient_info_returning()$state
        ),
        input(
          session$ns("zip"),
          type = "tel",
          placeholder = "ZIP",
          label = "ZIP",
          label_width = 4,
          `data-pasley-type` = "integer",
          maxlength = 5,
          `data-parsley-length` = '[5,5]',
          onkeypress = "return event.charCode >= 48 && event.charCode <= 57",
          value = patient_info_returning()$zip
        ),
        input(
          session$ns("email"),
          placeholder = "Email",
          label_width = 4,
          `data-parsley-type` = "email",
          value = patient_info_returning()$email
        ),
        div(
          class = "controls form-group",
          style = "text-align: right;",
          tags$label(
            class = "checkbox col-sm-4",
            `for` = session$ns("emailDeal"),
            "Email Deals"
          ),
          tags$input(
            type = "checkbox",
            id = session$ns("emailDeal"),
            value = session$ns("emailDeal"),
            name = session$ns("emailDeal"),
            class = "col-sm-7"
          )
        ),
        if (patient_info_returning()$emailDeal == 1) {
          tags$script(paste0(
            '$("#',
            session$ns("emailDeal"),
            '").prop("checked",true)'
          ))
        },
        if (patient_info_returning()$textDeal == 1) {
          tags$script(paste0(
            '$("#',
            session$ns("textDeal"),
            '").prop("checked",true)'
          ))
        },
        tags$script(
          paste0(
            'var cleaveDOB = new Cleave("#',
            session$ns("birthday"),
            '", {
            date: true, datePattern: ["Y","m","d"]
    })'
      )
        )
          )
        ),
      footer = parsleyr::submit_form(
        session$ns("submit_info_edit"),
        "Submit",
        class = "btn btn-info add-queue-btn action-button",
        formId = session$ns("basic_info_form")
      )
      )
    )
    })
    
    observeEvent(input$submit_info_edit, {
      req(
        patientId(),
        input$name,
        input$name2,
        input$address,
        input$californiaID,
        input$city,
        input$zip,
        input$state,
        input$phone,
        input$email,
        input$birthday
      )
      
      # phone and zip are legit
      # zip
      req(nchar(input$zip) == 5,!is.na(as.integer(input$zip)))
      
      # phone
      # convert to number
      phone <- as.numeric(gsub(" ", "", input$phone))
      # remove leading 1?
      req(!is.na(phone) && nchar(phone) %in% 10:11)
      
      # ID # is legit
      req(is_californiaId(input$californiaID))
      
      req(nchar(input$state) == 2)
      
      # make sure date is date
      req(grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", input$birthday))
      
      u_f_edit_info(
        pool,
        patientId(),
        first = input$name,
        last = input$name2,
        address = input$address,
        californiaId = input$californiaID,
        city = input$city,
        zip = input$zip,
        state = input$state,
        email = input$email,
        phone = phone,
        birthday = input$birthday,
        textDeal = input$textDeal,
        emailDeal = input$emailDeal
      )
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      
      update_option(proxy, list(
        value = patientId(),
        label = paste0(input$name2, ", ", input$name, " (", input$californiaID, ")")
      ))
      
      trigger_queue(trigger_queue() + 1)
      trigger_patients(trigger_patients() + 1)
      reload(reload() + 1)
      
      removeModal()
      
    })
    
    observeEvent(input$edit_medical_info, {
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('form-horizontal').css('width','110%').css('font-size','110%');"
          ),
          h1("Edit Medical Info"),
          tags$form(
            id = session$ns("edit_medical_form"),
            input(
              session$ns("physician"),
              placeholder = "Physician",
              value = patient_info_returning()$physician,
              label_width = 4
            ),
            input(
              session$ns("expirationDate"),
              placeholder = "Expiration Date",
              `data-parsley-pattern` = "/^\\d{4}[\\/\\-](0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])$/",
              value = gsub("-", "/", patient_info_returning()$expirationDate),
              label_width = 4
            ),
            tags$script(
              paste0(
                'var cleaveExp = new Cleave("#',
                session$ns("expirationDate"),
                '", {
                date: true, datePattern: ["Y","m","d"]
    })'
      )
            ),
      input(
        session$ns("recId"),
        type = "tel",
        placeholder = "Medicard Card #",
        value = patient_info_returning()$recId,
        label_width = 4
      ),
      input(
        session$ns("medicalCondition"),
        placeholder = "Condition",
        required = FALSE,
        value = patient_info_returning()$medicalCondition,
        label_width = 4
      )
              ),
      footer = parsleyr::submit_form(
        session$ns("submit_medical_edit"),
        "Submit",
        class = "btn btn-info add-queue-btn action-button",
        formId = session$ns("edit_medical_form")
      )
        )
    )
    })
    
    observeEvent(input$submit_medical_edit, {
      # server side form validation
      req(patientId(),
          input$expirationDate,
          input$physician,
          input$recId)
      
      # validate date
      req(grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$",
                input$expirationDate))
      
      # validate recId
      # req(nchar(input$update_recId) == 15,!is.na(as.numeric(input$update_recId)))
      
      u_f_med_info(
        pool,
        patientId(),
        expirationDate = input$expirationDate,
        physician = input$physician,
        medicalCondition = input$medicalCondition,
        recId = input$recId
      )
      
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      trigger_patients(trigger_patients() + 1)
      removeModal()
    })
    
    observeEvent(input$edit_preferences_info, {
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$script("$('.modal-content').addClass('form-horizontal');"),
          h1("Edit Preferences"),
          # checkbox inputs
          tags$form(
            id = session$ns("preference_form"),
            class = "preference-edit",
            input(
              session$ns("recommender"),
              placeholder = "Referred By",
              label_width = 4,
              required = FALSE,
              value = patient_info_returning()$recommender
            ),
            h3("Strain Type"),
            div(
              tags$label(`for` = session$ns("indica"), "Indica"),
              tags$input(
                id = session$ns("indica"),
                type = "checkbox",
                value = session$ns("indica"),
                name = session$ns("indica")
              )
            ),
            add_check("indica", patient_info_returning()),
            div(
              tags$label(`for` = session$ns("sativa"), "Sativa"),
              tags$input(
                id = session$ns("sativa"),
                type = "checkbox",
                value = session$ns("sativa"),
                name = session$ns("sativa")
              )
            ),
            add_check("sativa", patient_info_returning()),
            div(
              tags$label(`for` = session$ns("hybrid"), "Hybrid"),
              tags$input(
                id = session$ns("hybrid"),
                type = "checkbox",
                value = session$ns("hybrid"),
                name = session$ns("hybrid")
              )
            ),
            add_check("hybrid", patient_info_returning()),
            h3("Product Type"),
            div(
              tags$label(`for` = session$ns("flower"), "Flower"),
              tags$input(
                id = session$ns("flower"),
                type = "checkbox",
                value = session$ns("flower"),
                name = session$ns("flower")
              )
            ),
            add_check("flower", patient_info_returning()),
            div(
              tags$label(`for` = session$ns("concentrate"), "Concentrate"),
              tags$input(
                id = session$ns("concentrate"),
                type = "checkbox",
                value = session$ns("concentrate"),
                name = session$ns("concentrate")
              )
            ),
            add_check("concentrate", patient_info_returning()),
            div(
              tags$label(`for` = session$ns("edible"), "Edible"),
              tags$input(
                id = session$ns("edible"),
                type = "checkbox",
                value = session$ns("edible"),
                name = session$ns("edible")
              )
            ),
            add_check("edible", patient_info_returning()),
            div(
              tags$label(`for` = session$ns("other"), "Other"),
              tags$input(
                id = session$ns("other"),
                type = "checkbox",
                value = session$ns("other"),
                name = session$ns("other")
              )
            )
          ),
          add_check("other", patient_info_returning()),
          footer = parsleyr::submit_form(
            session$ns("submit_preference_edit"),
            "Submit",
            class = "btn btn-info add-queue-btn action-button",
            formId = session$ns("preference_form")
          )
        )
      )
    })
    
    observeEvent(input$submit_preference_edit, {
      req(patientId())
      u_f_edit_pref(
        pool,
        patientId(),
        input$indica,
        input$sativa,
        input$hybrid,
        input$flower,
        input$concentrate,
        input$edible,
        input$other,
        if (isTruthy(input$recommender)) {
          input$recommender
        } else {
          ""
        }
      )
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })
    
    observeEvent(input$edit_images, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script("
                    $('.modal-content').addClass('form-horizontal');"),
        h1("Edit Images"),
        div(
          class = "file-input",
          div(
            class = "form-group",
            tags$label(
              `for` = session$ns("photoIdPath"),
              class = "control-label control-label-left col-sm-3",
              "Photo ID",
              span(class = "req", "*")
            ),
            div(class = "col-sm-9",
                shiny::fileInput(session$ns("photoIdPath"), NULL))
          ),
          div(
            class = "form-group",
            tags$label(
              `for` = session$ns("photoIdPath"),
              class = "control-label control-label-left col-sm-3",
              "Medical Card",
              span(class = "req", "*")
            ),
            div(class = "col-sm-9",
                shiny::fileInput(session$ns("medicalPath"), NULL))
          )
        ),
        footer = tags$button(
          id = session$ns("submit_images"),
          "Submit",
          class = "btn btn-info add-queue-btn action-button"
        )
        ))
    })
    
    observeEvent(input$submit_images, {
      req(isTruthy(input$photoIdPath) | isTruthy(input$medicalPath))
      
      if (isTruthy(input$photoIdPath)) {
        photoS3 <-
          paste0(
            paste("photo", input$new_patient, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$photoIdPath$datapath)
          )
        
        tryCatch(
          aws.s3::put_object(input$photoIdPath$datapath, photoS3, bucket),
          warning = function(w) {
            stop("S3 failed", w)
          }
        )
        
        u_f_photoId(pool, patientId(), photoS3)
        
      }
      
      if (isTruthy(input$medicalPath)) {
        medicalS3 <-
          paste0(
            paste("medical", input$new_patient, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$medicalPath$datapath)
          )
        
        tryCatch(
          aws.s3::put_object(input$medicalPath$datapath, medicalS3, bucket),
          warning = function(w) {
            stop("S3 failed \n", w)
          }
        )
        
        u_f_medId(pool, patientId(), medicalS3)
      }
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })
    
    observe({
      if (isTRUE(expired() <= 0)) {
        session$sendCustomMessage("toggle_expiration", TRUE)
      } else {
        session$sendCustomMessage("toggle_expiration", FALSE)
      }
    })
    
    output$name <- renderUI({
      if (isTruthy(patientId())) {
        h1(
          paste(
            patient_info_returning()$firstName,
            patient_info_returning()$lastName
          )
        )
      } else {
        h1("Please select a returning")
      }
    })
    
    # status
    output$expiration <- renderUI({
      if (expired() <= 0) {
        # bad
        tagList(h2("Expired!!!"),
                p(paste(
                  "medical card expired", abs(expired()), "days ago"
                )))
      } else {
        # good
        tagList(h2(expired()),
                p("days until medical card expires"))
      }
    })
    
    output$info <- DT::renderDataTable(
      patient_info_returning() %>%
        mutate_(
          emailDeal = ~ if_else(emailDeal == 1, "YES", "NO"),
          textDeal = ~ if_else(textDeal == 1, "YES", "NO"),
          birthday = ~ paste0(birthday, " (", age, " years old)")
        ) %>%
        select_(
          # Name = ~ name,
          `California ID` = ~ californiaID,
          DOB = ~ birthday,
          Address = ~ address,
          City = ~ city,
          Zip = ~ zip,
          Email = ~ email,
          `Deals by Email` = ~ emailDeal,
          Phone = ~ phone,
          `Deals by Text` = ~ textDeal
        ) %>%
        t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                     "N/A")),
      options = list(dom = 't', columnDefs = list(
        list(
          targets = 0,
          render = JS(
            "function(data, type, row, meta) {
            return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
  }"
          )
          ),
        list(targets = 1, className = "dt-left")
          )),
      rownames = TRUE,
      class = "table dt-row", selection = 'none'
      )
    
    output$preference <- DT::renderDataTable({
      info <- patient_info_returning()
      data.frame(
        check.names = FALSE,
        Strain = paste0(c("Indica", "Sativa", "Hybrid")[which(c(info$indica ==
                                                                  1, info$sativa == 1, info$hybrid == 1))], collapse = ", "),
        Product = paste0(c(
          "Flower", "Concentrate", "Edible", "Other"
        )[which(c(
          info$flower == 1,
          info$concentrate ==
            1,
          info$edible ==
            1,
          info$other ==
            1
        ))],
        collapse = ", "),
        `Referred By` = if_else(info$recommender=="", NA_character_, info$recommender)
      ) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                         "N/A"))
      
    }, options = list(dom = 't', columnDefs = list(
      list(
        targets = 0,
        render = JS(
          "function(data, type, row, meta) {
          return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
  }"
        )
        ),
      list(targets = 1, className = "dt-left")
        )), rownames = TRUE, class = "table dt-row", selection = 'none')
    
    output$recommendation <- DT::renderDataTable({
      patient_info_returning() %>%
        mutate_(
          medicalCondition = ~if_else(medicalCondition == "", NA_character_, medicalCondition)
        ) %>%
        select_(
          `Expiration Date` = ~ expirationDate,
          Physician = ~ physician,
          `Medical Card ID #` = ~ recId,
          `Medical Condtion` = ~ medicalCondition
        ) %>%
        t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                     "N/A"))
    }, options = list(dom = 't', columnDefs = list(
      list(
        targets = 0,
        render = JS(
          "function(data, type, row, meta) {
          return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
  }"
        )
        ),
      list(targets = 1, className = "dt-left")
        )), rownames = TRUE, class = "table dt-row", selection = 'none')
    
    # images
    
    output$id_image_out <- renderUI({
      if (isTruthy(patientId()) &&
          isTruthy(patient_info_returning()$photoPath)) {
        tags$img(
          src = paste0(
            "https://s3-us-west-2.amazonaws.com/",
            bucket,
            "/",
            patient_info_returning()$photoPath
          ),
          height = "100%",
          width = "100%"
        )
      } else {
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/1000px-No_image_available.svg.png",
                 height = "100%",
                 width = "100%")
      }
    })
    
    output$recommendation_image_out <- renderUI({
      if (isTruthy(patientId()) &&
          isTruthy(patient_info_returning()$medicalPath)) {
        tags$img(
          src = paste0(
            "https://s3-us-west-2.amazonaws.com/",
            bucket,
            "/",
            patient_info_returning()$medicalPath
          ),
          height = "100%",
          width = "100%"
        )
      } else {
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/1000px-No_image_available.svg.png",
                 height = "100%",
                 width = "100%")
      }
    })
    
    patient_sales <- reactive({
      req(patientId())
      q_f_patient_sales(pool, patientId()) 
    })
    
    output$patient_type <- renderC3({
      req(patientId())
      req(patient_sales()$profit)
      patient_sales() %>% 
        mutate_(type = ~tools::toTitleCase(type)) %>%
        spread_(~type, ~profit) %>%
        select_(~contains("Flower"), ~contains("Concentrate"),~contains("Edible"),~contains("Other")) %>% 
        summarise_all(function(x) {x[!is.na(x)][1]}) %>% c3() %>%
      c3_pie(format=DT::JS("function(value,ratio,id) {return '$' + value;}"))
      
    })
    
    output$patient_points <- renderC3({
      req(patientId())
      
      patient_info_returning() %>%
        select_(~points) %>% 
        mutate_(points=~if_else(is.na(points),0,points)) %>%
        c3() %>% 
        c3_gauge(min = 0, max = 10000, label = list(
          format = DT::JS(
            "function(value, ratio) {
            return value;
            }"
          )
        ))
    })
    
    output$no_type <- renderUI({
      req(!isTruthy(patient_sales()$profit))
      h3("No Data Available", style = "margin-top:15%")
    })
    
    callModule(CannaModules::patientHistory,
               "frontdesk",
               pool,
               reactive({
                 req(patientId())
                 patientId()
               }))
    
    return(reactive(patient_info_returning()))
    
  }

newPatientUI <- function(id) {
  ns <- NS(id)
  
  tagList(div(class = "content",
              tags$form(
                id = ns("newPatient"),
                class = "form",
                div(
                  class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
                  div(class = "row",
                      div(class = "name-container",
                          uiOutput(ns(
                            "name"
                          )))),
                  box(tableTitle("Basic Info"),
                      DT::dataTableOutput(ns("info"))),
                  box(
                    class = "images",
                    h1("Images"),
                    div(
                      class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
                      div(
                        class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
                        h4("Photo ID"),
                        shiny::imageOutput(ns("new_id_image"), inline = T, height = "auto")
                      ),
                      div(
                        class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
                        h4("Medical Card"),
                        shiny::imageOutput(
                          ns("new_medical_image"),
                          inline = T,
                          height = "auto"
                        )
                      )
                    )
                  )
                ),
                div(
                  class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
                  div(
                    class = "row",
                    div(
                      class = "add-delete-btn-container",
                      tags$button(id = ns("remove"), "Delete Patient", class = "btn btn-info delete-btn action-button", style = "width:30%"),
                      parsleyr::submit_form(
                        ns("submit"),
                        "Submit",
                        formId = ns("newPatient"),
                        class = "btn btn-info add-queue-btn",
                        style = "width:30%"
                      )
                    )
                  ),
                  div(class = "form-horizontal container fluid col-md-12",
                      div(
                        class = "row",
                        
                        h1("Enter Medical Information"),
                        div(
                          style = "margin-top:10%;",
                          input(ns("physician"), placeholder = "Physician", label_width = 4),
                          input(
                            ns("date"),
                            "text", `data-date-language` ="en", `data-date-week-start` =0,
                            `data-min-date` = format(Sys.Date(), "%Y-%m-%d"),
                            `data-max-date` = format(Sys.Date() + 366, "%Y-%m-%d"),
                            `data-initial-date` = NA, `data-date-format` = "yyyy/mm/dd",
                            placeholder = "Expiration Date (YYYY/MM/DD)",
                            label = "Expiration Date",
                            `data-parsley-pattern` = "/^\\d{4}[\\/\\-](0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])$/", label_width = 4
                          ),
                          tags$script(
                            paste0("$('#",ns("date"),"').parent('div').addClass('shiny-date-input');")
                          ),
                          input(
                            ns("recId"),
                            "tel",
                            placeholder = "Medical Card #",
                            `data-parsley-type` = "integer", label_width = 4
                          ),
                          tags$script(
                            paste0(
                              "var expDate=new Cleave('#",
                              ns("date"),
                              "', {
                              date: true, datePattern: ['Y', 'm', 'd']
})"
              )
                          )
                            )
                          )),
              div(class = "form-horizontal container fluid col-md-12", div(
                class = "row",
                h1("Upload Patient Images"),
                div(style = "margin-top:10%;",
                    shiny::uiOutput(ns("imageInputs"), inline = TRUE))
              ))
                        )
              )))
}

newPatient <-
  function(input,
           output,
           session,
           pool,
           patientId,
           bucket,
           trigger_new,
           trigger_returning,
           proxy,
           parent_session,
           reload_patient,
           trigger_patients) {
    trigger_patient_info_new <- reactiveVal(0)
    patient_info_new <- reactive({
      req(patientId())
      trigger_patient_info_new()
      q_f_patient_info(pool, patientId(), new = TRUE)
    })
    
    observeEvent(input$edit_basic_info, {
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('form-horizontal col-lg-12');
            $('.modal-body').css('height', '300px').css('font-size','110%');"
          ),
          h1("Edit Basic Info"),
          # add parsley
          tags$form(
            id = session$ns("basic_info_form"),
            div(
              class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
              input(
                session$ns("name"),
                placeholder = "First",
                label_width = 4,
                value = patient_info_new()$firstName,
                input_width = 8
              ),
              input(
                session$ns("name2"),
                placeholder = "Last",
                label_width = 4,
                value = patient_info_new()$lastName,
                input_width = 8
              ),
              input(
                session$ns("californiaID"),
                type = "tel",
                placeholder = "California ID",
                label = "CA ID",
                label_width = 4,
                maxlength = 8,
                `data-parsley-californiaid` = I(""),
                value = patient_info_new()$californiaID,
                input_width = 8
              ),
              input(
                session$ns("birthday"),
                placeholder = "DOB",
                label = "DOB",
                label_width = 4,
                `data-parsley-year` = I(""),
                `data-parsley-pattern` = "/^\\d{4}[\\/\\-](0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])$/",
                value = gsub("-", "/", patient_info_new()$birthday),
                input_width = 8
              )
            ),
            div(
              class = "col-xs-6 col-sm-6 col-md-6 col-lg-6",
              input(
                session$ns("address"),
                placeholder = "Address",
                label_width = 4,
                value = patient_info_new()$address
              ),
              input(
                session$ns("city"),
                placeholder = "City",
                label_width = 4,
                value = patient_info_new()$city
              ),
              input(
                session$ns("state"),
                placeholder = "State",
                label_width = 4,
                maxlength = 2,
                `data-parsley-length` = "[2,2]",
                value = patient_info_new()$state
              ),
              input(
                session$ns("zip"),
                type = "tel",
                placeholder = "ZIP",
                label = "ZIP",
                label_width = 4,
                `data-pasley-type` = "integer",
                maxlength = 5,
                `data-parsley-length` = '[5,5]',
                onkeypress = "return event.charCode >= 48 && event.charCode <= 57",
                value = patient_info_new()$zip
              )
            )
          ),
          footer = parsleyr::submit_form(
            session$ns("submit_info_edit"),
            "Submit",
            class = "btn btn-info add-queue-btn action-button",
            formId = session$ns("basic_info_form")
          )
          )
      )
      
    })
    
    observeEvent(input$submit_info_edit, {
      req(
        patientId(),
        input$name,
        input$name2,
        input$address,
        input$californiaID,
        input$city,
        input$zip,
        input$state,
        input$birthday
      )
      
      # phone and zip are legit
      # zip
      req(nchar(input$zip) == 5,!is.na(as.integer(input$zip)))
      
      # ID # is legit
      req(is_californiaId(input$californiaID))
      
      req(nchar(input$state) == 2)
      
      # make sure date is date
      req(grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", input$birthday))
      
      u_f_edit_info_new(
        pool,
        patientId(),
        first = input$name,
        last = input$name2,
        address = input$address,
        californiaId = input$californiaID,
        city = input$city,
        zip = input$zip,
        state = input$state,
        birthday = input$birthday
      )
      trigger_patient_info_new(trigger_patient_info_new() + 1)
      trigger_patients(trigger_patients() + 1)
      update_option(proxy, list(
        value = patientId(),
        label = paste0(input$name2, ", ", input$name, " (", input$californiaID, ")")
      ))
      removeModal()
    })
    
    # add patient new patient
    observeEvent(input$submit, {
      # server side form validation
      req(
        patientId(),
        input$date,
        input$recId,
        input$medicalPath,
        input$photoIdPath,
        input$physician
      )
      
      # validate date
      req(grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", input$date))
      
      # validate recId
      # req(nchar(input$recId) == 15,!is.na(as.numeric(input$recId)))
      
      # file input is file
      req(
        is.data.frame(input$medicalPath),
        nrow(input$medicalPath) == 1,
        file.exists(input$medicalPath$datapath),
        is.data.frame(input$photoIdPath),
        nrow(input$photoIdPath) == 1,
        file.exists(input$photoIdPath$datapath)
      )
      
      trigger_patient_info_new(trigger_patient_info_new() + 1)
      if (patient_info_new()$verified == 1) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Patient has not finished signup form yet. Please wait...")
        ))
      } else if (is.na(patient_info_new()$docuSigned) ||
                 patient_info_new()$docuSigned == 0) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(
            "Patient finished signup form but did not complete docuSign.\nPlease have patient sign."
          )
        ))
      } else {
        # upload images to S3
        medicalS3 <-
          paste0(
            paste("medical", input$new_patient, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$medicalPath$datapath)
          )
        
        photoS3 <-
          paste0(
            paste("photo", input$new_patient, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$photoIdPath$datapath)
          )
        
        tryCatch(
          aws.s3::put_object(input$medicalPath$datapath, medicalS3, bucket),
          warning = function(w) {
            stop("S3 failed \n", w)
          }
        )
        
        tryCatch(
          aws.s3::put_object(input$photoIdPath$datapath, photoS3, bucket),
          warning = function(w) {
            stop("S3 failed", w)
          }
        )
        
        id <- patientId()
        # add patient
        u_f_new_patient(pool,
                        id,
                        input$date,
                        input$physician,
                        photoS3,
                        medicalS3,
                        input$recId)
        
        ### add to queue?
        lapply(c("date", "physician", "recId"), function(x) {
          updateTextInput(session, x, value = "")
        })
        
        trigger_files(trigger_files() + 1)
        trigger_new(trigger_new() + 1)
        trigger_returning(trigger_returning() + 1)
        trigger_patients(trigger_patients() + 1)
        reload_patient(list(selected = id))
        session$sendCustomMessage("reset_file_input", list(id = session$ns("medicalPath")))
        session$sendCustomMessage("reset_file_input", list(id = session$ns("photoIdPath")))
        session$sendCustomMessage("reset_parsley", list(id = session$ns("newPatient")))
        ### go to patient info page with new patient there
        # updateNavlistPanel(parent_session, "tabset", "patientInfo")
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("New patient has been added")
        ))
      }
    })
    
    # remove
    observeEvent(input$remove, {
      req(patientId())
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ),
        h1("Are you sure you want to remove patient?"),
        h1("Data cannot be recovered once removed!"),
        footer = tags$button(id = session$ns("delete"), class = "action-button btn btn-info delete-btn", "Remove")
      ))
    })
    
    observeEvent(input$delete, {
      d_f_patient(pool, patientId())
      # trigger reload of selectize
      trigger_new(trigger_new() + 1)
      trigger_returning(trigger_returning() + 1)
      trigger_patients(trigger_patients() + 1)
      removeModal()
      # updateSelectizeInput(parent_session, "patient", selected = "")
      reload_patient(list(selected = NULL))
    })
    
    output$name <- renderUI({
      if (isTruthy(patientId())) {
        h1(paste(
          patient_info_new()$firstName,
          patient_info_new()$lastName
        ))
      } else {
        h1("Please select a new patient")
      }
      
    })
    
    output$info <- DT::renderDataTable({
      patient_info_new() %>%
        select_(
          #Name = ~name,
          `California ID` = ~ californiaID,
          DOB = ~ birthday,
          Address = ~ address,
          City = ~ city,
          Zip = ~ zip,
          State = ~ state
        ) %>%
        t()  %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                      "N/A"))
    }, options = list(dom = "t", columnDefs = list(
      list(
        targets = 0,
        render = JS(
          "function(data, type, row, meta) {
          return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
  }"
    )
        ),
    list(targets = 1, className = "dt-left")
        )), colnames = "", class = "table dt-row",
    rownames = TRUE, selection = "none", server = TRUE)
    
    trigger_files <- reactiveVal(0)
    output$imageInputs <- renderUI({
      trigger_files()
      tagList(div(
        class = "file-input",
        div(
          class = "form-group",
          tags$label(
            `for` = session$ns("photoIdPath"),
            class = "control-label control-label-left col-sm-4",
            "Photo ID",
            span(class = "req", "*")
          ),
          div(class = "col-sm-7",
              shiny::fileInput(
                session$ns("photoIdPath"), NULL, width = "100%"
              ),
              tags$script(HTML(
                '$("#new_patient-photoIdPath").on("change", function(value) {
        if ($(this).parents(\'.input-group\').find(\'.parsley-error\')) {
    setTimeout(function() {
          $("#new_patient-photoIdPath").parents(\'.input-group\').find(\'.parsley-error\').parsley().validate();
}, 1)
        }
      });'
              )
              )
        )),
        div(
          class = "form-group",
          tags$label(
            `for` = session$ns("photoIdPath"),
            class = "control-label control-label-left col-sm-4",
            "Medical Card",
            span(class = "req", "*")
          ),
          div(class = "col-sm-7",
              shiny::fileInput(
                session$ns("medicalPath"), NULL, width = "100%"
              ),
              tags$script(HTML(
                '$("#new_patient-medicalPath").on("change", function(value) {
        if ($(this).parents(\'.input-group\').find(\'.parsley-error\')) {
              setTimeout(function() {
          $("#new_patient-medicalPath").parents(\'.input-group\').find(\'.parsley-error\').parsley().validate();
}, 1)
        }
      });'
              )
              )
        ),
        tags$script(
          paste0(
            "$('#",
            session$ns("photoIdPath"),
            "').closest(\".input-group\").children(\"input\").prop(\"required\", true);
            //   $('#",
            session$ns("photoIdPath"),
            "').attr('','');\n",
            "$('#",
            session$ns("medicalPath"),
            "').closest(\".input-group\").children(\"input\").prop(\"required\", true);
            //  $('#",
            session$ns("medicalPath"),
            "').attr('','');"
            )
            )
      )))
    })
    
    output$new_id_image <- renderImage({
      trigger_files()
      if (isTruthy(input$photoIdPath)) {
        list(
          src = input$photoIdPath$datapath,
          width = "100%",
          height = "100%",
          alt = "New Patient Driver's License"
        )
      } else {
        list(
          src = system.file(package = "CannaFrontdesk", "www", "noimage.png"),
          width = "100%",
          height = "100%",
          alt = "Placeholder"
        )
      }
    }, deleteFile = FALSE)
    
    output$new_medical_image <- renderImage({
      trigger_files()
      if (isTruthy(input$medicalPath)) {
        list(
          src = input$medicalPath$datapath,
          width = "100%",
          height = "100%",
          alt = "New Patient doctor's recommendation"
        )
      } else {
        list(
          src = system.file(package = "CannaFrontdesk", "www", "noimage.png"),
          width = "100%",
          height = "100%",
          alt = "Placeholder"
        )
      }
    }, deleteFile = FALSE)
    
    return(reactive(patient_info_new()))
    
  }

queueUI <- function(id) {
  ns <- NS(id)
  
  tagList(div(
    class = "content",
    div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
        box(h1("Queue"),
            DT::dataTableOutput(ns(
              "queue"
            )))),
    div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
        box(
          h1("In Store"),
          DT::dataTableOutput(ns("store"))
        ))
  ))
}

queue <-
  function(input,
           output,
           session,
           pool,
           proxy,
           trigger,
           reload,
           parent_session,
           trigger_patients) {
    # queue
    trigger_queue <- reactiveVal(0)
    queue_store <- reactive({
      session$sendCustomMessage("unbind-dt", session$ns("queue"))
      session$sendCustomMessage("unbind-dt", session$ns("store"))
      trigger()
      trigger_queue()
      q_f_queue(pool)
    })
    
    queue <- reactive({
      queue_store() %>%
        filter_(~ status == 1) %>%
        mutate_() %>%
        select_( ~ -status)
    })
    
    in_store <- reactive({
      queue_store() %>%
        filter_(~ status == 2) %>%
        select_( ~ -status)
    })
    
    queue_proxy <- DT::dataTableProxy(session$ns("queue"), session)
    store_proxy <- DT::dataTableProxy(session$ns("store"), session)
    
    # observeEvent(reload(), {
    #   session$sendCustomMessage("unbind-dt", session$ns("queue"))
    #   session$sendCustomMessage("unbind-dt", session$ns("store"))
    #   DT::replaceData(queue_proxy,
    #                   queue() %>% select_( ~ -idtransaction, ~ -idpatient))
    #   DT::replaceData(
    #     store_proxy,
    #     in_store() %>% select_( ~ -idtransaction, ~ -idpatient)
    #   )
    # })
    
    # take patient from queue and let in store
    obsList <- list()
    
    observeEvent(queue_store(), {
      n <- max(c(nrow(queue()), nrow(in_store())))
      if (length(obsList) < n) {
        obsList <<-
          c(obsList, lapply(seq_len(n - length(obsList)), function(i) {
            observeEvent(input[[paste0("let", i)]], {
              u_f_let_in(pool, queue()$idtransaction[i])
              trigger_queue(trigger_queue() + 1)
              session$sendCustomMessage("unbind-dt", session$ns("queue"))
              session$sendCustomMessage("unbind-dt", session$ns("store"))
              DT::replaceData(queue_proxy,
                              queue() %>% select_(~ -idtransaction, ~ -idpatient))
              DT::replaceData(store_proxy,
                              in_store() %>% select_(~ -idtransaction, ~ -idpatient))
            })
            observeEvent(input[[paste0("removeQ", i)]], {
              d_f_queue(pool, queue()$idtransaction[i])
              trigger_queue(trigger_queue() + 1)
              trigger_patients(trigger_patients() + 1)
              reload(reload() + 1)
              session$sendCustomMessage("unbind-dt", session$ns("queue"))
              session$sendCustomMessage("unbind-dt", session$ns("store"))
              DT::replaceData(queue_proxy,
                              queue() %>% select_(~ -idtransaction, ~ -idpatient))
              DT::replaceData(store_proxy,
                              in_store() %>% select_(~ -idtransaction, ~ -idpatient))
            })
            observeEvent(input[[paste0("removeS", i)]], {
              ### need to clear cart or give warning modal with an are you sure option
              showModal(modalDialog(
                easyClose = TRUE,
                tags$script(
                  "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
                ),
                h1("Warning!"),
                h2(
                  "Cancelling active transaction will remove items from cart!"
                ),
                footer = tags$button(
                  id = session$ns(paste0("remove", i)),
                  "End Transaction",
                  class = "btn btn-info delete-btn action-button"
                )
              ))
            })
            observeEvent(input[[paste0("remove", i)]], {
              d_f_queue(pool, in_store()$idtransaction[i])
              trigger_queue(trigger_queue() + 1)
              trigger_patients(trigger_patients() + 1)
              reload(reload() + 1)
              session$sendCustomMessage("unbind-dt", session$ns("queue"))
              session$sendCustomMessage("unbind-dt", session$ns("store"))
              DT::replaceData(queue_proxy,
                              queue() %>% select_(~ -idtransaction, ~ -idpatient))
              DT::replaceData(store_proxy,
                              in_store() %>% select_(~ -idtransaction, ~ -idpatient))
              removeModal()
            })
            observeEvent(input[[paste0("infoQ", i)]], {
              # trigger_queue(trigger_queue() + 1)
              updateSelectizeInput(parent_session, "patient", selected = queue()$idpatient[i])
            })
            observeEvent(input[[paste0("infoS", i)]], {
              # trigger_queue(trigger_queue() + 1)
              updateSelectizeInput(parent_session, "patient", selected = in_store()$idpatient[i])
            })
          }))
      }
      
    })
    
    output$queue <- DT::renderDataTable({
      queue() %>% select_( ~ -idtransaction,  ~ -idpatient) %>%
        mutate_(
          letIn = ~ row_number(),
          remove = ~ row_number(),
          info = ~ row_number(),
          timeIn = ~ as.character(as.POSIXct(
            hms::as.hms(hms::as.hms(timeIn))
          ), "%I:%M %p")
        ) %>%
        select_(
          Name =  ~ name,
          `California ID` =  ~ californiaID,
          `Arrival Time` = ~ timeIn,
          ~ letIn,
          ~ info,
          ~ remove
        )
    }, rownames = TRUE, width = "100%", options = list(
      dom = 't',
      #autoWidth = TRUE,
      preDrawCallback = JS(
        'function() {
        Shiny.unbindAll(this.api().table().node());}'
      ),
      drawCallback = JS(
        'function() {
        Shiny.bindAll(this.api().table().node());
        $(".even").removeClass("even").addClass("odd");
  } '
),
columnDefs = list(
  list(
    targets = seq_len(ncol(queue()) + 2) - 1,
    orderable = FALSE,
    className = "dt-center"
  ),
  list(targets = c(0),
       width = "5%"),
  list(targets = 1,
       width = "20%"),
  list(targets = 2,
       width = "10%"),
  list(targets = 3,
       width = "10%"),
  list(targets = 4:6,
       width = "18%"),
  list(targets = 4,
       render = JS(
         paste0(
           'function(data, type, row, meta) {
           return "<button id = \'',
           session$ns("let"),
           '" + data + "\' class = \'btn btn-info let-in-btn index-btn action-button\'>Let In</button>";
           }'
)
         )),
list(targets = 5,
     render = JS(
       paste0(
         'function(data, type, row, meta) {
         return "<button id = \'',
         session$ns("infoQ"),
         '" + data + "\' class = \'btn btn-info let-in-btn index-btn action-button\' onclick = \'CannaFrontdesk.change_tab(\\"patientInfo\\")\'>Info</button>";
  }'
)
       )),
list(targets = 6,
     render = JS(
       paste0(
         'function(data, type, row, meta) {
         return "<button id = \'',
         session$ns("removeQ"),
         '" + data + "\' class = \'btn btn-info remove-btn index-btn action-button\'>Remove</button>";
  }'
)
       ))
     )
    ), colnames = c("Name", "California ID", "Arrival Time", "", "", ""),
selection = 'none')
    
    output$store <- DT::renderDataTable({
      in_store() %>% select_( ~ -idtransaction, ~ -idpatient) %>%
        mutate_(
          remove = ~ row_number(),
          info = ~ row_number(),
          timeIn = ~ as.character(as.POSIXct(
            hms::as.hms(hms::as.hms(timeIn))
          ), "%I:%M %p")
        ) %>%
        select_(
          Name = ~ name,
          `California ID` =  ~ californiaID,
          `Arrival Time` = ~ timeIn,
          ~ info,
          ~ remove
        )
    }, rownames = TRUE, options = list(
      dom = 't',
      preDrawCallback = JS(
        'function() {
        Shiny.unbindAll(this.api().table().node());}'
      ),
      drawCallback = JS(
        'function() {
        Shiny.bindAll(this.api().table().node());
        $(".even").removeClass("even").addClass("odd");} '
      ),
      columnDefs = list(
        list(
          targets = seq_len(ncol(in_store()) + 1) - 1,
          orderable = FALSE,
          className = "dt-center"
        ),
        list(targets = c(0),
             width = "5%"),
        list(targets = 1,
             width = "20%"),
        list(targets = 2,
             width = "10%"),
        list(targets = 3,
             width = "10%"),
        list(targets = 4:5,
             width = "27%"),
        list(targets = 4,
             render = JS(paste0(
               'function(data, type,row,meta) {
                return "<button id=\'',session$ns("infoS"),'" + data + "\' class = \'btn btn-info let-in-btn index-btn action-button\' onclick = \'CannaFrontdesk.change_tab(\\"patientInfo\\")\'>Info</button>";
               }'
             ))),
        list(targets = 5,
             render = JS(paste0(
               'function(data,type,row,meta) {
               return "<button id=\'',session$ns("removeS"),'" + data + "\' class = \'btn btn-info remove-btn index-btn action-button \'>Remove</button>";
               }'
             )))
      )
      ), colnames = c("Name", "California ID", "Arrival Time", "", ""),
    selection = 'none')
    
    
    return(queue_store)
    }

allPatientsUI <- function(id) {
  ns <- NS(id)
  
  tagList(div(
    class = "content",
    div(class = "col-xs-12 col-sm-12 col-md-12 col-lg-12",
        box(
          h1("All Patients"),
          DT::dataTableOutput(ns("patients"))
        ))
  ))
}

allPatients <-
  function(input,
           output,
           session,
           pool,
           parent_session,
           trigger_patients) {
    
    patients <- reactive({
      trigger_patients()
      q_f_patients(pool)
    })
    
    # take patient from queue and let in store
    obsList <- list()
    
    observeEvent(patients(), {
      n <- nrow(patients())
      if (length(obsList) < n) {
        obsList <<-
          c(obsList, lapply(seq_len(n - length(obsList)), function(i) {
            observeEvent(input[[paste0("info", i)]], {
              # trigger_queue(trigger_queue() + 1)
              updateSelectizeInput(parent_session, "patient", selected = patients()$idpatient[i])
            })
          }))
      }
      
    })
    
    output$patients <- DT::renderDataTable({
      session$sendCustomMessage("unbind-dt", session$ns("patients"))
      patients() %>%
        mutate_(info = ~row_number(),
                avgSpent = ~ round(avgSpent),
        age = ~ floor(as.numeric(Sys.Date()-as.Date(birthday))/365)
          ) %>% 
        select_(Name = ~name, `California ID`=~californiaID, 
                Age = ~age, `Expiration Date` =~expirationDate,`Last Transaction` = ~lastTransaction, 
                `Average Spent` = ~avgSpent, #Points = ~points, 
                ~info)
    }, colnames = c("Name", "California ID", "Age", "Expiration Date", "Last Transaction", 
                                    "Average Spent", #"Points",
                                    ""),
    options = list(
      dom = 't',
      columnDefs = list(list(
        targets = 0:6,
        className = "dt-center"
      ),
      list(
        targets = 0,
        width = "15%"
      ),
      list(
        targets = 1,
        width = "5%"
      ),
      list(
        targets = 2,
        width = "3%"
      ),
      list(
        targets = 3,
        width = "7%"
      ),
      list(
        targets = 4,
        width = "7%"
      ),
      list(
        targets = 5,
        width = "7%",
        render = JS(
          'function(data, type, row, meta) {
            return data ? "$" + data.toString() : data;
          }'
        )
      ),
      # list(
      #   targets = 6,
      #   width = "5%"
      # ),
      list(
        targets = 6,
        width = "8%",
        orderable = FALSE,
        render = JS(
          paste0(
            'function(data, type, row, meta) {
         return "<button id = \'',
            session$ns("info"),
            '" + data + "\' class = \'btn btn-info let-in-btn index-btn action-button\' onclick =\'CannaFrontdesk.change_tab(\\"patientInfo\\")\'>Info</button>";
  }'
          )
        )
      )
      ),
      preDrawCallback = JS(
        'function() {
        Shiny.unbindAll(this.api().table().node());}'
      ),
      drawCallback = JS(
        'function() {
        Shiny.bindAll(this.api().table().node());
        $(".even").removeClass("even").addClass("odd");} '
      )
      ),
    selection = 'none', rownames = FALSE)
    
    
    return(patients)
  }

input <-
  function(id,
           type = "text",
           ...,
           name = id,
           placeholder = tools::toTitleCase(id),
           label = placeholder,
           required = TRUE,
           class = NULL,
           disabled = FALSE,
           label_width = 3,
           input_width = 7) {
    div(
      class = "form-group",
      tags$label(
        class = paste0("control-label control-label-left col-sm-", label_width),
        `for` = id,
        label,
        if (required) {
          span(class = "req", "*")
        }
      ),
      div(
        class = paste0("controls col-sm-", input_width),
        tags$input(
          id = id,
          type = type,
          name = name,
          `data-role` = type,
          placeholder = placeholder,
          required = if (required)
            NA
          else
            NULL,
          disabled = if (disabled)
            NA
          else
            NULL,
          class = paste0(c("form-control k-textbox", class), collapse = " "),
          ...
        )
      )
    )
  }

box <- function(...) {
  div(class = "row", div(class = "table-container", ...))
}

# navbarUI <- function(name = "Store Name") {
#   tagList(div(
#     id = "header",
#     tags$nav(class = "navbar navbar-default navbar-static-top navbar-style",
#              div(class = "container nav-container", style = "margin-left: 0;margin-right:0;width:100%",
#                  div(
#                    class = "row",
#                    div(class = "col-sm-3", 
#                        tags$p(class = "navbar-text",
#                               name)),
#                    div(
#                      class = "col-md-6",
#                      div(
#                        class = "inner-addon search-icon",
#                        icon("search", lib = "glyphicon"),
#                        # tags$select(id = "patient", class = "form-control search-box")
#                        selectizeInput(
#                          "patient",
#                          NULL,
#                          NULL,
#                          options = list(maxOptions = 10,
#                                         loadThrottle = NA,
#                                         valueField = "idpatient",
#                                         searchField = c("firstName", "middleName", "lastName", "californiaID"),
#                                         placeholder = "Search",
#                                         render = I("{
#                                                    option: function(item, escape) {
#                                                    return '<div>' +
#                                                    '<strong>' + escape(item.firstName) + ' ' + (item.middleName ? escape(item.middleName) + ' ' : '') + escape(item.lastName) + ' (' + escape(item.californiaID)  + ')</strong>:' +
#                                                    '<ul>' +
#                                                    '<li><span>Date Added: ' + escape(item.addDate) + '</span></li>' +
#                                                    '<li><span>Status: ' + (item.verified === 1 ? 'Pending completion of signup form' : item.verified === 2 ? 'Pending verification' : 'Ready') + '</span></li>' +
#                                                    (item.expirationDate ? '<li><span>Expiration Date: ' + escape(item.expirationDate) + '</span></li>' : '') +
#                                                    '</ul>' +
#                                                    '</div>';
#                                                    }               
# }")
#                                         )
#                        ),
#                        tags$script("$('#patient').addClass('search-box');")
#                      )
#                    ),
#                    div(class = "col-md-3",
#                        tags$ul(class = "nav navbar-nav navbar-right", style = "margin-right:15px;",
#                          uiOutput("user_name"),
#                          tags$li(class = "dropdown",
#                           tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", role = "button",
#                                  `aria-haspopup` = "true", `aria-expanded` = "false", tagList(tags$p("Other Apps", class = "navbar-text"
#                                                                                                      ,tags$span(class = "caret")))
#                                         ),
#                           tags$ul(
#                             class = "dropdown-menu",
#                             tags$li(tags$a(href = "../inventory/", "Inventory")),
#                             tags$li(tags$a(href = "../connect/", "Connect")),
#                             tags$li(tags$a(href = "../pos/", "Cash Register")),
#                             tags$li(role = "separator", class = "divider"),
#                             tags$li(tags$a(href = "https://cannadata.auth0.com/v2/logout", "Logout"))
#                           )
#                          )
#                        )
#                      ))))
#     ))
#   }

tableTitle <- function(title, icon = "pencil") {
  div(class = "table-title-and-icon", h1(title), icon(icon, "fa-2x table-icons"))
}

add_check <-
  function(box,
           patient_info_returning,
           session = shiny::getDefaultReactiveDomain()) {
    if (patient_info_returning[[box]] == 1) {
      tags$script(paste0('$("#', session$ns(box), '").prop("checked",true)'))
    }
  }