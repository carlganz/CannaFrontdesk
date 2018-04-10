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
      col(12,
        div(
          class = "row",
          div(class = "countdown-container notexpired",
              uiOutput(ns("expiration")))
        )),
    col(6,
      div(
        div(class = "name-container",
            uiOutput(ns(
              "name"
            )))),
      box(tableTitle("Basic Info"),
          DT::dataTableOutput(ns("info"))),
      box(tableTitle("Notes"),
          uiOutput(ns("notes"))
          ),
      tagList(
        box(tableTitle("Medical Info"),
            DT::dataTableOutput(ns("recommendation"))),
        box(
          class = "images",
          tableTitle("Images"),
          col(12,
            col(6,
                h4("Photo ID"),
                uiOutput(ns("id_image_out"))),
            col(6,
                h4("Rec"),
                uiOutput(ns(
                  "recommendation_image_out"
                )))
          )
        ))
    ),
    col(6,
      div(
        class = "row",
        div(
          class = "add-delete-btn-container",
          style = "width:100%;",
          div(
            tags$button(
              id = ns("remove"),
              "Delete",
              class = "btn btn-info delete-btn action-button",
              style = "width:20%;",
              formnovalidate = NA
            ),
            tags$button(
              id = ns("let_in"),
              "Let In",
              class = "btn btn-info add-queue-btn action-button",
              style = "width:20%;"
            ),
            tags$button(
              id = ns("add_queue"),
              "Add Queue",
              class = "btn btn-info add-queue-btn action-button",
              style = "width:23%;"
            ),
            uiOutput(ns("textButton"))
          )
        )
      ),
        tagList(
          box(tableTitle("Preferences"),
              DT::dataTableOutput(ns("preference"))),
          box(h1("Past Products", style = "width:100%;text-align:left;"),style = "overflow:hidden",
              div(style = "margin-top:35px",
                  uiOutput(ns("no_type"), TRUE),
                  c3Output(ns("patient_type"))
              )
          )),
      box(h1("Reward Points"),style = "overflow:hidden",
          div(style = "margin-top:8%",
              c3Output(ns("patient_points"))
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
           reload_patient,
           trigger_patients,
           max_points,
           base_url,
           msg_service_sid,
           facility) {
    trigger_patient_info_returning <- reactiveVal(0)
    patient_info_returning <- reactive({
      req(patientId())
      trigger_patient_info_returning()
      q_f_patient_info(pool, patientId())
    })
    # days until expired
    expired <- reactive({
      req(patientId())
      if (isTruthy(patient_info_returning()$expirationDate)) {
        difftime(patient_info_returning()$expirationDate, Sys.Date())
      } else {
        (-1*ceiling(difftime(patient_info_returning()$birthday, Sys.Date())/365)) - 21
      }
    })
    
    # add patient to queue
    observeEvent(input$add_queue, {
      req(patientId())
      if (isTRUE(expired() <= 0)) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Customer is not eligible!")
        ))
      } else if (patientId() %in% queue()$idpatient) {
        showModal(modalDialog(
          easyClose = TRUE, tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(paste(
            "Customer already in",
            if (queue()$status[patientId() == queue()$idpatient] == 2)
              "store"
            else
              "queue"
          ))
        ))
      } else {
        i_f_add_queue(pool, patientId(), new = nrow(patient_history())==0, facilityNumber = facility()$idfacility)
        trigger_queue(trigger_queue() + 1)
        trigger_patients(trigger_patients() + 1)
        reload(reload() + 1)
        today_count <- patient_history() %>% filter_(~date == Sys.Date()) %>% nrow()
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Customer added to queue."),
          if (today_count > 0) {
            h1(sprintf("NOTE: This is their %s  visit today!", scales::ordinal(today_count + 1)))
          }
        ))
      }
    })
    
    observeEvent(input$let_in, {
      req(patientId())
      if (isTRUE(expired() <= 0)) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Customer is ineligible!")
        ))
      } else if (patientId() %in% queue()$idpatient) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1(paste(
            "Customer already in",
            if (queue()$status[patientId() == queue()$idpatient])
              "store"
            else
              "queue"
          ))
        ))
      } else {
        i_f_let_in(pool, patientId(), new = nrow(patient_history())==0, facilityNumber = facility()$idfacility)
        trigger_queue(trigger_queue() + 1)
        trigger_patients(trigger_patients() + 1)
        reload(reload() + 1)
        today_count <- patient_history() %>% filter_(~date == Sys.Date()) %>% nrow()
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Customer has been let into store."),
          if (today_count > 0) {
            h1(sprintf("NOTE: This is their %s visit today!", scales::ordinal(today_count + 1)))
          }
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
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1(
            "Cannot remove customer while customer is in active transaction."
          )
        ))
      } else {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Are you sure you want to remove customer?"),
          h1("Data cannot be recovered once removed!"),
          footer = tags$button(id = session$ns("delete"), class = "action-button btn btn-info delete-btn", "Remove")
        ))
      }
    })
    
    observeEvent(input$delete, {
      d_f_patient(pool, patientId())
      ### metrc delete
      if (getOption("CannaData_state") %in% c("CO", "MD", "OR")) {
        # metrc_delete_patient(facility()$medicalFacilityNumber, )
      }
      # trigger reload of selectize
      trigger_new(trigger_new() + 1)
      trigger_returning(trigger_returning() + 1)
      trigger_patients(trigger_patients() + 1)
      removeModal()
      reload_patient(list(selected = NULL, time = Sys.time()))
    })
    
    observeEvent(input$edit_basic_info, {
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          class = "edit-basic-info",
          tags$script(
            "$('.modal-lg').css('width', '85%');
            $('.modal-content').addClass('form-horizontal col-lg-12');
            $('.modal-body').css('overflow-y', '-webkit-paged-y');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Edit Basic Info"),
          # add parsley
          tags$form(
            id = session$ns("basic_info_form"),
            col(6,
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
                session$ns("id"),
                type = "tel",
                placeholder = "ID #",
                label = "ID #",
                label_width = 4,
                maxlength = 8,
                `data-parsley-californiaid` = I(""),
                value = patient_info_returning()$id,
                input_width = 8
              ),
              input(
                session$ns("idExpiration"),
                # type = "date",
                placeholder = "ID Expiration",
                label = "ID EXP",
                label_width = 4,
                `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
                value = format(as.Date(patient_info_returning()$idExpiration), "%m/%d/%Y"),
                input_width = 8,
                required = FALSE
              ),
              input(
                session$ns("birthday"),
                placeholder = "DOB",
                label = "DOB",
                label_width = 4,
                `data-parsley-year` = I(""),
                `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
                value = format(as.Date(patient_info_returning()$birthday), "%m/%d/%Y"),
                input_width = 8
              ),
              input(
                session$ns("phone"),
                type = "tel",
                placeholder = "Phone",
                required = FALSE,
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
      tags$script(
        paste0(
          'var cleave = new Cleave("#',
          session$ns("idExpiration"),
          '", {
          date: true, datePattern: ["m","d", "Y"]
    })'
        )
              ),
      tags$label("Text Deal", class = "control-label control-label-left col-sm-4"),
      col(7, class = "checkbox checkbox-icons",
               tags$li(
                 icon("mobile", "deal-type fa-2x"),
                 value = tolower(as.character(as.logical(patient_info_returning()$textDeal))),
                 class = if (isTRUE(patient_info_returning()$textDeal)) "selected",
                 alt = session$ns("textDeal")
               ))
            ),
      col(6,
        input(
          session$ns("address"),
          placeholder = "Address",
          label_width = 4, required = FALSE,
          value = patient_info_returning()$address
        ),
        input(
          session$ns("city"),
          placeholder = "City",
          label_width = 4,required = FALSE,
          value = patient_info_returning()$city
        ),
        input(
          session$ns("state"),
          placeholder = "State",
          label_width = 4,
          maxlength = 2,required = FALSE,
          `data-parsley-length` = "[2,2]",
          value = patient_info_returning()$state
        ),
        input(
          session$ns("zip"),
          type = "tel",required = FALSE,
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
          session$ns("recommender"),
          placeholder = "Referrer",
          label_width = 4,
          value= patient_info_returning()$recommender,
          required = FALSE
        ),
        input(
          session$ns("email"),
          placeholder = "Email",
          label_width = 4, required = FALSE,
          `data-parsley-type` = "email",
          value = patient_info_returning()$email
        ),
        tags$script(
          HTML("CannaFrontdesk.enable_buttons()")),
        tags$script(
          paste0(
            'var cleaveDOB = new Cleave("#',
            session$ns("birthday"),
            '", {
            date: true, datePattern: ["m","d", "Y"]
    })'
      )
        ),
      tags$label("Email Deal", class = "control-label control-label-left col-sm-4"),
      col(7, class = "checkbox checkbox-icons",
               tags$li(
                 icon("envelope-o", "deal-type fa-2x"),
                 value = tolower(as.character(as.logical(patient_info_returning()$emailDeal))),
                 class = if (isTRUE(patient_info_returning()$emailDeal)) "selected",
                 alt = session$ns("emailDeal")
               )
      ))),
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
        input$id,
        input$birthday,
        nchar(input$id) %in% 8:9
      )
      
      # phone and zip are legit
      # zip
      if (isTruthy(input$zip)) req(nchar(input$zip) == 5,!is.na(as.integer(input$zip)))
      
      # phone
      # convert to number
      if (isTruthy(input$phone)) {
      phone <- as.numeric(gsub("[ ()]", "", input$phone))
      # remove leading 1?
      req(nchar(phone) %in% 10:11,!is.na(phone))
      
      if (substr(phone, 1, 1) == "1") {
        phone <- substr(phone, 2, nchar(phone))
      }
      } else {
        phone = NA_character_
      }
      
      # ID # is legit
      req(is_californiaId(input$id))
      if (isTruthy(input$state)) {
      req(nchar(input$state) == 2)
      }
      # make sure date is date
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", input$birthday))
      u_f_edit_info(
        pool,
        patientId(),
        first = input$name,
        last = input$name2,
        address = input$address,
        id = input$id,
        idExpiration = if (isTruthy(input$idExpiration)) input$idExpiration else NA_character_,
        city = input$city,
        zip = input$zip,
        state = input$state,
        email = input$email,
        phone = phone,
        birthday = input$birthday,
        textDeal = input$textDeal,
        emailDeal = input$emailDeal,
        recommender = if (isTruthy(input$recommender)) input$recommender else NA
      )
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      
      update_option(proxy, value = patientId(), list(
        firstName = input$name, lastName = input$name2, middleName = patient_info_returning()$middleName,
        id = input$id, idpatient = patientId(),
        addDate = patient_info_returning()$addDate, 
        verified = 3, expirationDate = patient_info_returning()$expirationDate,
        label = paste0(input$name, ", ", input$name2, " (", input$id, ")")
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
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
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
              session$ns("startDate"),
              placeholder = "Start Date",
              `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
              value = format(as.Date(patient_info_returning()$effectiveDate), "%m/%d/%Y"),
              label_width = 4
            ),
            tags$script(
              paste0(
                'var cleaveStart = new Cleave("#',
                session$ns("startDate"),
                '", {
                date: true, datePattern: ["m","d", "Y"]
    })'
              )
            ),
            input(
              session$ns("expirationDate"),
              placeholder = "Exp Date",
              `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
              value = format(as.Date(patient_info_returning()$expirationDate), "%m/%d/%Y"),
              label_width = 4
            ),
            tags$script(
              paste0(
                'var cleaveExp = new Cleave("#',
                session$ns("expirationDate"),
                '", {
                date: true, datePattern: ["m","d", "Y"]
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
        session$ns("plants"),
        type = "tel", value = 6,
        placeholder = "Max Plants",
        value = patient_info_returning()$plants,
        label_width = 4
      ),
      input(
        session$ns("smokable"),
        type = "tel", value = 2,
        placeholder = "Max Smokable",
        value = patient_info_returning()$smokable,
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
    
    observe({
      req(input$startDate)
      date <- paste0(substr(input$startDate, 1, 6), as.numeric(substr(input$startDate, 7, 11)) + 1)
      updateTextInput(session, "expirationDate", value = date)
    })
    
    observeEvent(input$submit_medical_edit, {
      # server side form validation
      req(patientId(),
          input$startDate,
          input$expirationDate,
          input$physician,
          input$recId,
          input$plants,
          input$smokable)
      
      # validate date
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$",
                input$expirationDate))
      
      # validate recId
      # req(nchar(input$update_recId) == 15,!is.na(as.numeric(input$update_recId)))
      
      if (getOption("CannaData_state") %in% c("CO", "OR", "MD")) {
        # httr::with_verbose(metrc_post_patients(facility()$medicalFacilityNumber, input$recId, input$startDate, input$expirationDate,
        #                     input$plants, input$smokable, Sys.Date()))
      }
      
      u_f_med_info(
        pool,
        patientId(),
        expirationDate = input$expirationDate,
        effectiveDate = input$startDate,
        physician = input$physician,
        medicalCondition = input$medicalCondition,
        recId = input$recId, input$plants, input$smokable
      )
      
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      trigger_patients(trigger_patients() + 1)
      trigger_new(trigger_new() + 1)
      update_option(proxy, value = patientId(), list(
        firstName = patient_info_returning()$firstName, lastName = patient_info_returning()$lastName, 
        middleName = patient_info_returning()$middleName,
        id = patient_info_returning()$id, idpatient = patientId(),
        addDate = patient_info_returning()$addDate, 
        verified = 3, expirationDate = input$expirationDate,
        label = paste0(patient_info_returning()$firstName, ", ", patient_info_returning()$lastName, 
                       " (", patient_info_returning()$id, ")")
      ))
      
      removeModal()
    })
    
    observeEvent(input$edit_preferences_info, {
      showModal(
        modalDialog(
          easyClose = TRUE, size = "l",
          class = "edit-pref-info",
          tags$script("$('.modal-content').addClass('form-horizontal');"),
          h1("Edit Preferences"), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          # checkbox inputs
          tags$form(
            id = session$ns("preference_form"),
            class = "preference-edit",
            h3("Strain Type"),
            tags$div(class = "checkbox checkbox-icons",
                     tags$li(
                       tags$img(
                         src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/hybrid.svg",
                         alt = "Hybrid",
                         id = session$ns("hybrid"),
                         class = "strain-type",
                         value = tolower(as.character(as.logical(patient_info_returning()$hybrid)))
                       )
                     ),
                     tags$li(
                       
                       tags$img(
                         src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/sativa.svg",
                         alt = "Sativa",
                         id = session$ns("sativa"),
                         class = "strain-type",
                         value = tolower(as.character(as.logical(patient_info_returning()$sativa)))
                       )
                       
                     ),
                     tags$li(
                       tags$img(
                         src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/indica.svg",
                         alt = "Indica",
                         id = session$ns("indica"),
                         class = "strain-type",
                         value = tolower(as.character(as.logical(patient_info_returning()$indica)))
                       )
                     )),
            h3("Product Type"),
            tags$div(
              class = "checkbox checkbox-icons",
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/flower.svg",
                  alt = "Flower",
                  id = session$ns("flower"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$flower)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/concentrate.svg",
                  alt = "Concentrate",
                  id = session$ns("concentrate"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$concentrate)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/edible.svg",
                  alt = "Edible",
                  id = session$ns("edible"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$edible)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/beverage.svg",
                  alt = "Beverage",
                  id = session$ns("beverage"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$beverage)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/joint.svg",
                  alt = "Joint",
                  id = session$ns("joint"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$joint)))
                )
              )
            ),
            tags$div(
              class = "checkbox checkbox-icons",
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/soap.svg",
                  alt = "Soap",
                  id = session$ns("soap"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$soap)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/vaporizer.svg",
                  alt = "Vaporizer",
                  id = session$ns("vaporizer"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$vaporizer)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/tincture.svg",
                  alt = "Tincture",
                  id = session$ns("tincture"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$tincture)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/ointment.svg",
                  alt = "Ointment",
                  id = session$ns("ointment"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$ointment)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/paraphernalia.svg",
                  alt = "Paraphernalia",
                  id = session$ns("paraphernalia"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$paraphernalia)))
                )
              )
            )
          ), tags$script(HTML("CannaFrontdesk.enable_icons();")),
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
        input$beverage,
        input$joint, 
        input$ointment, 
        input$tincture, 
        input$paraphernalia, 
        input$soap,
        input$vaporizer
      )
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })
    
    observeEvent(input$edit_images, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script("
                    $('.modal-content').addClass('form-horizontal');"),
        h1("Edit Images"), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
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
            col(9,
                shiny::fileInput(session$ns("photoIdPath"), NULL))
          ),
          div(
            class = "form-group",
            tags$label(
              `for` = session$ns("photoIdPath"),
              class = "control-label control-label-left col-sm-3",
              "Rec",
              span(class = "req", "*")
            ),
            col(9,
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

    observeEvent(input$text_form, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Enter Phone #"),
        div(class = "center",
            shinyCleave::phoneInput(session$ns("text_phone"), NULL, placeholder = "Phone #")),
        footer = tags$button(id = session$ns("send_form"), class = "action-button btn btn-info add-queue-btn", "Send")
      ))
    })
    
    observeEvent(input$send_form, {
      req(input$text_phone)
      url <- httr::modify_url(
        url = paste0(base_url, "reward/"),
        query = list(
          idpatient = patientId(),
          idpatiente = jwt_encode_sig(jwt_claim(idpatient = patientId()), key = gsub("\n  ", "\n", getOption("canna_key")))
        )
      )
      
      phone <- as.numeric(gsub("[ ()]", "", input$text_phone))
      # remove leading 1?
      req(nchar(phone) %in% 10:11,!is.na(phone))
      
      if (substr(phone, 1, 1) == "1") {
        phone <- substr(phone, 2, nchar(phone))
      }
      
      tw_send_message(paste0("+1", phone), msg_service_id = msg_service_sid, body = paste("Please sign-up at the following link: ", url))
      u_f_phone(pool, patientId(), phone)
      removeModal()
      
    })
    
    output$name <- renderUI({
      if (isTruthy(patientId())) {
        h1(
          
            patient_info_returning()$name
          
        )
      } else {
        h1("Please select patient")
      }
    })
    
    output$textButton <- renderUI({
      req(patientId(), !isTruthy(patient_info_returning()$phone))
      actionButton(session$ns("text_form"), "Rewards", class = "btn btn-info add-queue-btn", width = "20%")
    })
    
    # status
    output$expiration <- renderUI({
      if (isTruthy(patient_info_returning()$expirationDate)) {
      if (isTRUE(expired() <= 0)) {
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
      } else {
        if (isTRUE(expired() <= 0)) {
          tagList(h2("STOP! Customer is under 21! STOP!"))
        } else {
          tagList(h2("Customer is", expired() + 21, "years old"))
        }
      }
    })
    
    output$info <- DT::renderDataTable(
      patient_info_returning() %>%
        mutate_(
          emailDeal = ~ if_else(emailDeal == 1, "YES", "NO"),
          textDeal = ~ if_else(textDeal == 1, "YES", "NO"),
          birthday = ~ paste0(format(as.Date(birthday), "%m/%d/%Y"), " (", age, " years old)"),
          recommender = ~ if_else(recommender=="", NA_character_, recommender),
          idExpiration = ~format(as.Date(idExpiration), "%m/%d/%Y"),
          phone = ~ paste0("(", substring(phone, 1, 3), ") ", substring(phone, 4, 6), "-", substring(phone, 7))
        ) %>%
        select_(
          # Name = ~ name,
          DOB = ~ birthday,
          Address = ~ address,
          City = ~ city,
          Zip = ~ zip,
          `ID #` = ~ id,
          `ID Exp` = ~ idExpiration,
          Email = ~ email,
          `Deals by Email` = ~ emailDeal,
          Phone = ~ phone,
          `Deals by Text` = ~ textDeal,
          `Referred By` = ~recommender
        ) %>% 
        t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                     "N/A")),
      options = list(dom = 't', pageLength = 11, columnDefs = list(
        list(
          targets = 0,
          render = JS(
            "function(data, type, row, meta) {
            return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
    }"
          )
          ),
        list(targets = 1, className = "dt-left", render = JS(
          "function(data, type, row, meta) {
          return row[0] === 'Email' ? '<span style = \"word-break: break-all;\" />' + data + '</span>' : row[0] === 'DOB' ? parseInt(data.substring(12, 15)) < 21 ? '<span style = \"color:red\"/>' + data + '</span>' : data : data;
  }"
        ))
        )),
      rownames = TRUE,
      class = "table dt-row", selection = 'none'
      )
    
    output$notes <- renderUI({
      tags$textarea(if_else(is.na(patient_info_returning()$comment), "", patient_info_returning()$comment), readonly = TRUE, rows = 3,
                    style = "width: 100%; border-radius: 5px;color:black", placeholder = "No notes recorded")
    })
    
    observeEvent(input$edit_notes, {
      req(patientId())
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Edit Notes"),
          span(class = "text-modal-wrapper",
          textAreaInput(session$ns("edit_note"), NULL, rows = 3, value = if_else(is.na(patient_info_returning()$comment), "", patient_info_returning()$comment))
        ), footer = actionButton(session$ns("submit_note"), "Submit", class = "btn btn-info add-queue-btn", placeholder = "No notes recorded")
        )
      )
    })
    
    observeEvent(input$submit_note, {
      req(patientId())
      u_f_note(pool, patientId(), input$edit_note)
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })
    
    output$preference <- DT::renderDataTable({
      info <- patient_info_returning()
      data.frame(
        check.names = FALSE,
        Strain = paste0(c("Indica", "Sativa", "Hybrid")[which(c(info$indica ==
                                                                  1, info$sativa == 1, info$hybrid == 1))], collapse = "/"),
        Product = paste0(c(
          "Flower", "Concentrate", "Edible", "Beverage", "Joint", "Ointment", "Tincture", "Paraphernalia", "Soap", "Vaporizer"
        )[which(c(
          info$flower == 1,
          info$concentrate ==
            1,
          info$edible ==
            1,
          info$beverage ==
            1,
          info$joint == 1,
          info$ointment == 1,
          info$tincture == 1,
          info$paraphernalia == 1,
          info$soap == 1,
          info$vaporizer == 1
        ))],
        collapse = "/")
      ) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                         "N/A"))
      
    }, options = list(dom = 't', columnDefs = list(
      list(
        targets = 0,
        render = JS(
          "function(data, type, row, meta) {
          return '<span class = \\'dt-rowname\\' style = \\'line-height: 8vh\\'>' + data + ':<\\span>';
          }"
        )
        ),
      list(targets = 1, className = "dt-left",
           render = JS(
             'function(data, type, row, meta) {
             return meta.row === 2 ? data : data ? data.split("/").map(function(value) {
             return "<img class=\\"product-image\\" src = \\"https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/" + value.toLowerCase() + ".svg\\">";
             }).join("") : data;
             }'
        )
      )
    )), rownames = TRUE, class = "table dt-row", selection = 'none')
    
    output$recommendation <- DT::renderDataTable({
      patient_info_returning() %>%
        mutate_(
          expirationDate = ~ format(as.Date(expirationDate), "%m/%d/%Y"),
          medicalCondition = ~if_else(medicalCondition == "", NA_character_, medicalCondition)
        ) %>%
        select_(
          `Exp Date` = ~ expirationDate,
          Physician = ~ physician,
          `Rec ID #` = ~ recId,
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

        display <- system(sprintf("aws s3 presign s3://%s/%s", bucket, patient_info_returning()$photoPath), intern = TRUE)
        tags$img(
          src = display,
          height = "100%",
          class = "hoverZoomLink",
          width = "100%"
        )
      } else {
        tags$img(src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/noneLight.svg",
                 class = "no-image",
                 height = "100%",
                 width = "100%")
      }
    })
    
    output$recommendation_image_out <- renderUI({
      if (isTruthy(patientId()) &&
          isTruthy(patient_info_returning()$medicalPath)) {
        tags$img(
          src = system(sprintf("aws s3 presign s3://%s/%s", bucket, patient_info_returning()$medicalPath), intern = TRUE),
          height = "100%",
          class = "hoverZoomLink",
          width = "100%"
        )
      } else {
        tags$img(src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/noneLight.svg",
                 class = "no-image",
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
      req(nrow(patient_sales())>0)
      patient_sales() %>% 
        mutate_(type = ~tools::toTitleCase(type)) %>%
        spread_("type", "profit") %>%
        select_(~contains("Flower"), ~contains("Concentrate"),~contains("Edible"),~contains("Beverage"),
                ~contains("Soap"), ~contains("Vaporizer"), ~contains("Tincture"), ~contains("Ointment"),
                ~contains("Joint"), ~contains("Tobacco"), ~contains("Paraphernalia"), ~contains("Misc")) %>% 
        summarise_all(function(x) {x[!is.na(x)][1]}) %>% c3() %>%
        c3_pie(format=DT::JS("function(value,ratio,id) {return '$' + value;}"))
      
    })
    
    output$patient_points <- renderC3({
      req(patientId())
      
      patient_info_returning() %>%
        select_(~points) %>% 
        mutate_(points=~if_else(is.na(points),0,points)) %>%
        c3() %>% 
        c3_gauge(min = 0, max = max_points, label = list(
          format = DT::JS(
            "function(value, ratio) {
            return value;
    }"
          )
          ))
    })
    
    output$no_type <- renderUI({
      req(!isTruthy(patient_sales()$profit))
      h4("No Data Available")
    })
    
    patient_history <- callModule(CannaModules::patientHistory,
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
                col(6,
                  div(
                    div(class = "name-container",
                        uiOutput(ns(
                          "name"
                        )))),
                  box(tableTitle("Basic Info"),
                      DT::dataTableOutput(ns("info"))),
                  box(
                    class = "images",
                    h1("Images"),
                    col(12,
                      col(6,
                        h4("Photo ID"),
                        shiny::imageOutput(ns("new_id_image"), inline = T, height = "auto")
                      ),
                      col(6,
                        h4("Rec"),
                        shiny::imageOutput(
                          ns("new_medical_image"),
                          inline = T,
                          height = "auto"
                        )
                      )
                    )
                  )
                ),
                col(6,
                  div(
                    class = "row",
                    div(
                      class = "add-delete-btn-container",
                      tags$button(id = ns("remove"), "Delete Patient", 
                                  class = "btn btn-info delete-btn action-button", 
                                  style = "width:30%", formnovalidate = NA),
                      parsleyr::submit_form(
                        ns("submit"),
                        "Submit",
                        formId = ns("newPatient"),
                        class = "btn btn-info add-queue-btn",
                        style = "width:30%"
                      ),
                      tags$button(id = ns("text_form"), "Text Form",
                                  class = "btn btn-info add-queue-btn action-button",
                                  style = "width:30%", formnovalidate = NA)
                    )
                  ),
                  col(12, class = "form-horizontal container fluid",
                      div(
                        class = "row",
                        
                        h1("Enter Info", style = "width:100%;margin-bottom:15px;"),
                        div(class = "input-container",
                            input(ns("physician"), placeholder = "Physician", label_width = 4),
                            input(
                              ns("startDate"),
                              "text", `data-date-language` ="en", `data-date-week-start` =0,
                              `data-min-date` = format(Sys.Date() - 366, "%m-%d-%Y"),
                              `data-max-date` = format(Sys.Date(), "%m-%d-%Y"),
                              `data-initial-date` = NA, `data-date-format` = "mm/dd/yyyy",
                              placeholder = "Start Date (MM/DD/YYYY)",
                              label = "Start Date",
                              `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/", label_width = 4
                            ),
                            tags$script(
                              paste0("$('#",ns("startDate"),"').parent('div').addClass('shiny-date-input');")
                            ),
                            input(
                              ns("endDate"),
                              "text", `data-date-language` ="en", `data-date-week-start` =0,
                              `data-min-date` = format(Sys.Date(), "%m-%d-%Y"),
                              `data-max-date` = format(Sys.Date() + 366, "%m-%d-%Y"),
                              `data-initial-date` = NA, `data-date-format` = "mm/dd/yyyy",
                              placeholder = "Exp Date (MM/DD/YYYY)",
                              label = "Exp Date",
                              `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/", label_width = 4
                            ),
                            tags$script(
                              paste0("$('#",ns("endDate"),"').parent('div').addClass('shiny-date-input');")
                            ),
                            input(
                              ns("recId"),
                              "text",
                              placeholder = "Rec #", label_width = 4
                            ),
                            input(ns("plants"), placeholder = "Max Plants", type = "number", label_width = 4, required = TRUE, value = 6),
                            input(ns("smokable"), placeholder = "Max Smokable", type = "number", label_width = 4, required = TRUE, value = 2),
                            tags$script(
                              paste0(
                                "var expDate=new Cleave('#",
                                ns("endDate"),
                                "', {
                                date: true, datePattern: ['m', 'd', 'Y']
})"
              )
                            ),
              tags$script(
                paste0(
                  "var expDate=new Cleave('#",
                  ns("startDate"),
                  "', {
                                date: true, datePattern: ['m', 'd', 'Y']
})"
                )
              )
                              )
                            )),
              col(12, class = "form-horizontal container fluid", div(
                class = "row",
                h1("Upload Images", style = "width:100%;margin-bottom:15px"),
                div(class = "input-container",
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
           reload_patient,
           trigger_patients,
           msg_service_sid,
           base_url,
           docu_log,
           facility) {
    trigger_patient_info_new <- reactiveVal(0)
    patient_info_new <- reactive({
      req(patientId())
      trigger_patient_info_new()
      q_f_patient_info(pool, patientId(), new = TRUE)
    })
    
    observe({
      req(input$startDate)
      date <- paste0(substr(input$startDate, 1, 6), as.numeric(substr(input$startDate, 7, 11)) + 1)
      updateTextInput(session, "endDate", value = date)
    })
    
    observeEvent(input$edit_basic_info, {
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('form-horizontal col-lg-12');
            $('.modal-body').css('height', '300px').css('font-size','110%');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Edit Basic Info"),
          # add parsley
          tags$form(
            id = session$ns("basic_info_form"),
            col(6,
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
                session$ns("id"),
                type = "tel",
                placeholder = "ID #",
                label = "ID #",
                label_width = 4,
                maxlength = 8,
                `data-parsley-californiaid` = I(""),
                value = patient_info_new()$id,
                input_width = 8
              ),
              input(
                session$ns("birthday"),
                placeholder = "DOB",
                label = "DOB",
                label_width = 4,
                `data-parsley-year` = I(""),
                `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
                value = format(as.Date(patient_info_new()$birthday), "%m/%d/%Y"),
                input_width = 8
              ),
              tags$script(
                paste0(
                  'var cleaveDOB = new Cleave("#',
                  session$ns("birthday"),
                  '", {
                  date: true, datePattern: ["m","d", "Y"]
    })'
                )
              )
                ),
            col(6,
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
        input$id,
        input$city,
        input$zip,
        input$state,
        input$birthday
      )
      
      # phone and zip are legit
      # zip
      req(nchar(input$zip) == 5,!is.na(as.integer(input$zip)))
      
      # ID # is legit
      req(is_californiaId(input$id))
      
      req(nchar(input$state) == 2)
      
      # make sure date is date
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", input$birthday))
      
      u_f_edit_info_new(
        pool,
        patientId(),
        first = input$name,
        last = input$name2,
        address = input$address,
        id = input$id,
        city = input$city,
        zip = input$zip,
        state = input$state,
        birthday = input$birthday
      )
      trigger_patient_info_new(trigger_patient_info_new() + 1)
      trigger_patients(trigger_patients() + 1)
      update_option(proxy, value = patientId(), list(
        firstName = input$name, lastName = input$name2, middleName = patient_info_new()$middleName,
        id = input$id, idpatient = patientId(),
        addDate = patient_info_new()$addDate, 
        verified = patient_info_new()$verified, expirationDate = NA,
        label = paste0(input$name, ", ", input$name2, " (", input$id, ")")
      ))
      
      removeModal()
    })
    
    # add patient new patient
    observeEvent(input$submit, {
      # server side form validation
      req(
        patientId(),
        input$startDate,
        input$endDate,
        input$plants,
        input$smokable,
        input$recId,
        input$medicalPath,
        input$photoIdPath,
        input$physician
      )
      
      # validate date
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", input$endDate))
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", input$startDate))
      
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
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Patient has not finished signup form yet. Please wait...")
        ))
      } else if (is.na(patient_info_new()$docuSigned) ||
                 patient_info_new()$docuSigned == 0) {
        docuStatus <- docu_envelope_status(base_url = docu_log[1, 3], envelope_id = patient_info_new()$envelopeId)
        if (docuStatus == "completed") {
          u_docuSign(pool, patientId = patientId())
          id <- patientId()

          medicalS3 <-
            paste0(
              paste("medical", id, Sys.Date(), sep = "_"),
              ".",
              tools::file_ext(input$medicalPath$datapath)
            )

          photoS3 <-
            paste0(
              paste("photo", id, Sys.Date(), sep = "_"),
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
          ######### Add Patient to Metrc ################
          if (getOption("CannaData_state") %in% c("CO","MD","OR")) {
            # metrc_post_patients(facility()$medicalFacilityNumber, input$recId, input$startDate, input$endDate,
            #                     input$plants, input$smokable, Sys.Date())
          }

          # add patient
          u_f_new_patient(pool,
                          id,
                          input$endDate,
                          input$startDate,
                          input$physician,
                          photoS3,
                          medicalS3,
                          input$recId,
                          input$plants,
                          input$smokable)

          ### add to queue?
          lapply(c("date", "physician", "recId"), function(x) {
            updateTextInput(session, x, value = "")
          })

          trigger_files(trigger_files() + 1)
          trigger_new(trigger_new() + 1)
          trigger_returning(trigger_returning() + 1)
          trigger_patients(trigger_patients() + 1)
          session$sendCustomMessage("reset_file_input", list(id = session$ns("medicalPath")))
          session$sendCustomMessage("reset_file_input", list(id = session$ns("photoIdPath")))
          session$sendCustomMessage("reset_parsley", list(id = session$ns("newPatient")))
          ### go to patient info page with new patient there
          reload_patient(list(selected = id, time = Sys.time(), type = "patient"))
          showModal(modalDialog(
            easyClose = TRUE,
            tags$script(
              "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
            ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
            h1("New patient has been added")
          ))
        } else {
        showModal(modalDialog(
          easyClose = TRUE, tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1(
            "Patient finished signup form but did not complete docuSign.\nPlease have patient sign."
          )
        ))
        }
      } else {
        # upload images to S3
        id <- patientId()
        
        medicalS3 <-
          paste0(
            paste("medical", id, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$medicalPath$datapath)
          )
        
        photoS3 <-
          paste0(
            paste("photo", id, Sys.Date(), sep = "_"),
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
        ######### Add Patient to Metrc ################
        if (getOption("CannaData_state") %in% c("OR","CO","MD")) {
          # metrc_post_patients(facility()$medicalFacilityNumber, input$recId, input$startDate, input$endDate,
          #                     input$plants, input$smokable, Sys.Date())
        }
        
        
        # add patient
        u_f_new_patient(pool,
                        id,
                        input$endDate,
                        input$startDate,
                        input$physician,
                        photoS3,
                        medicalS3,
                        input$recId,
                        input$plants,
                        input$smokable)
        
        ### add to queue?
        lapply(c("date", "physician", "recId"), function(x) {
          updateTextInput(session, x, value = "")
        })
        
        trigger_files(trigger_files() + 1)
        trigger_new(trigger_new() + 1)
        trigger_returning(trigger_returning() + 1)
        trigger_patients(trigger_patients() + 1)
        session$sendCustomMessage("reset_file_input", list(id = session$ns("medicalPath")))
        session$sendCustomMessage("reset_file_input", list(id = session$ns("photoIdPath")))
        session$sendCustomMessage("reset_parsley", list(id = session$ns("newPatient")))
        ### go to patient info page with new patient there
        reload_patient(list(selected = id, time = Sys.time(), type = "patient"))
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
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
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Are you sure you want to remove patient?"),
        h1("Data cannot be recovered once removed!"),
        footer = tags$button(id = session$ns("delete"), class = "action-button btn btn-info delete-btn", "Remove")
      ))
    })
    
    observeEvent(input$text_form, {
      req(patientId())
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Enter Phone #"),
        div(class = "center",
        shinyCleave::phoneInput(session$ns("text_phone"), NULL, placeholder = "Phone #")),
        footer = tags$button(id = session$ns("send_form"), class = "action-button btn btn-info add-queue-btn", "Send")
      ))
    })
    
    observeEvent(input$send_form, {
      req(input$text_phone)
      
      url <- httr::modify_url(
        url = paste0(base_url, "signup/"),
        query = list(
          idpatient = patientId(),
          idpatiente = jwt_encode_sig(jwt_claim(idpatient = patientId()), gsub("\n  ", "\n", getOption("canna_key")))
        )
      )
      
      phone <- as.numeric(gsub("[ ()]", "", input$text_phone))
      # remove leading 1?
      req(nchar(phone) %in% 10:11,!is.na(phone))
      
      if (substr(phone, 1, 1) == "1") {
        phone <- substr(phone, 2, nchar(phone))
      }
      
      tw_send_message(paste0("+1", phone), msg_service_id = msg_service_sid, body = paste("Please sign-up at the following link: ", url))
      u_f_phone(pool, patientId(), phone)
      removeModal()
      
    })
    
    observeEvent(input$delete, {
      d_f_patient(pool, patientId())
      # trigger reload of selectize
      trigger_new(trigger_new() + 1)
      trigger_returning(trigger_returning() + 1)
      trigger_patients(trigger_patients() + 1)
      removeModal()
      reload_patient(list(selected = NULL, time = Sys.time()))
    })
    
    output$name <- renderUI({
      if (isTruthy(patientId())) {
        h1(paste(
          patient_info_new()$name
        ))
      } else {
        h1("Select incomplete profile or create new profile")
      }
      
    })
    
    output$info <- DT::renderDataTable({
      patient_info_new() %>%
        mutate_(birthday = ~ paste0(format(as.Date(birthday), "%m/%d/%Y"), " (", age, " years old)"),
                idExpiration= ~ format(as.Date(idExpiration), "%m/%d/%Y")) %>%
        select_(
          #Name = ~name,
          DOB = ~ birthday,
          Address = ~ address,
          City = ~ city,
          Zip = ~ zip,
          State = ~ state,
          `ID #` = ~ id,
          `ID Exp` = ~ idExpiration
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
    list(targets = 1, className = "dt-left", render = JS(
      "function(data, type, row, meta) {
      return row[0] === 'DOB' ? parseInt(data.substring(12, 15)) < 21 ? '<span style = \"color:red\"/>' + data + '</span>' : data : data;
  }"
    ))
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
          col(7,
              shiny::fileInput(
                session$ns("photoIdPath"), NULL, width = "100%"
              ),
              tags$script(HTML(
                '$("#new_patient-photoIdPath").on("change", function(value) {
                if ($(this).parents(\'.input-group\').find(\'.parsley-error\').length > 0) {
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
            "Rec",
            span(class = "req", "*")
          ),
          col(7,
              shiny::fileInput(
                session$ns("medicalPath"), NULL, width = "100%"
              ),
              tags$script(HTML(
                '$("#new_patient-medicalPath").on("change", function(value) {
                if ($(this).parents(\'.input-group\').find(\'.parsley-error\').length > 0) {
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
          class = "hoverZoomLink",
          alt = "New Patient Driver's License"
        )
      } else {
        list(
          src = system.file(package = "CannaFrontdesk", "www", "noneLight.svg"),
          width = "100%",
          height = "100%",
          alt = "Placeholder",
          class = "no-image"
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
          class = "hoverZoomLink",
          alt = "New Patient doctor's recommendation"
        )
      } else {
        list(
          src = system.file(package = "CannaFrontdesk", "www", "noneLight.svg"),
          width = "100%",
          height = "100%",
          alt = "Placeholder",
          class = "no-image"
        )
      }
    }, deleteFile = FALSE)
    
    return(reactive(patient_info_new()))
    
    }

queueUI <- function(id) {
  ns <- NS(id)
  
  tagList(div(
    class = "content",
    col(12,
        box(h1("Online Sales"),
        DT::dataTableOutput(ns("online")))),
    col(12,
        box(tableTitle("Queue", "plus"),
            DT::dataTableOutput(ns(
              "queue"
            )))),
    col(12,
        box(
          tableTitle("In Store", "plus"),
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
           reload_patient,
           trigger_patients,
           trigger_online,
           online,
           state,
           patients,
           trigger_new,
           facility) {
    # queue
    trigger_queue <- reactiveVal(0)
    queue_store <- reactive({
      invalidateLater(5000)
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
    
    queue_proxy <- DT::dataTableProxy("queue", session, deferUntilFlush = FALSE)
    store_proxy <- DT::dataTableProxy("store", session, deferUntilFlush = FALSE)
    online_proxy <- DT::dataTableProxy("online", session, deferUntilFlush = FALSE)

    
    # take patient from queue and let in store
    observeEvent(input$let, {
      u_f_let_in(pool, queue()$idtransaction[input$let$row])
      trigger_queue(trigger_queue() + 1)
      # DT::replaceData(queue_proxy,
      #                 queue() %>% select_(~ -idtransaction, ~ -idpatient))
      # DT::replaceData(store_proxy,
      #                 in_store() %>% select_(~ -idtransaction, ~ -idpatient))
    })
    
    observeEvent(input$removeQ, {
      d_f_queue(pool, queue()$idtransaction[input$removeQ$row])
      trigger_queue(trigger_queue() + 1)
      trigger_patients(trigger_patients() + 1)
      reload(reload() + 1)
      # DT::replaceData(queue_proxy,
      #                 queue() %>% select_(~ -idtransaction, ~ -idpatient))
      # DT::replaceData(store_proxy,
      #                 in_store() %>% select_(~ -idtransaction, ~ -idpatient))
    })
    
    observeEvent(input$removeS, {
      ### need to clear cart or give warning modal with an are you sure option
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Warning!"),
        h2(
          "Cancelling active transaction will remove items from cart!"
        ),
        footer = tags$button(
          id = session$ns("remove_store"),
          "End Transaction",
          class = "btn btn-info delete-btn action-button"
        )
      ))
    })
    
    observeEvent(input$remove_store, {
      d_f_queue(pool, in_store()$idtransaction[input$removeS$row])
      trigger_queue(trigger_queue() + 1)
      trigger_patients(trigger_patients() + 1)
      reload(reload() + 1)
      # DT::replaceData(queue_proxy,
      #                 queue() %>% select_(~ -idtransaction, ~ -idpatient))
      # DT::replaceData(store_proxy,
      #                 in_store() %>% select_(~ -idtransaction, ~ -idpatient))
      removeModal()
    })
    
    observeEvent(input$infoQ, {
      reload_patient(list(selected = queue()$idpatient[input$infoQ$row], time = Sys.time(), type = "patient"))
    })
    
    observeEvent(input$infoS, {
      reload_patient(list(selected = in_store()$idpatient[input$infoS$row], time = Sys.time(), type = "patient"))
    })
    
    observeEvent(input$onlineSale, {
      reload_patient(list(selected = online()$idtransaction[input$onlineSale$row], time = Sys.time(), type = "transaction"))
    })
    
    
    observeEvent(input$new_queue, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');"
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Add Customer to Queue"),
        tags$form(id = session$ns("queue_form"),
          class = "center",
        tagList(textInput(session$ns("queue_name"), "Name"),

           tagList(tags$br(),
           textInput(session$ns("queue_id"), "ID #")),
           tags$br(),
           textInput(session$ns("queue_birthday"), "Birthday"),
            tags$script(
              '$("#frontdesk-queue_id, #frontdesk-queue_name, #frontdesk-queue_birthday").attr("required", true);',
              'var CannaBirthday = new Cleave("#frontdesk-queue_birthday", {
        date: true,
        datePattern: ["m", "d", "Y"]
      });$("#frontdesk-queue_birthday").attr("data-parsley-age","21");'
            ))
        ),
        footer =
          tagList(
            if (isTRUE(facility()$medical == 1))
            parsleyr::submit_form(
          session$ns("queue_med"),
          label = "Add Medical",
          class = "btn btn-info add-queue-btn",
          onclick = I('$("#frontdesk-queue_birthday").attr("data-parsley-age","18");'),
          formId = session$ns("queue_form")
        ),
        if (isTRUE(facility()$recreational == 1))
        parsleyr::submit_form(
          session$ns("queue_rec"),
          label = "Add Recreational",
          class = "btn btn-info add-queue-btn",
          onclick = I('$("#frontdesk-queue_birthday").attr("data-parsley-age","21");'),
          formId = session$ns("queue_form")
        )
      )
            
      ))
    })
    
    
    observeEvent(input$queue_rec, {
      req(input$queue_name, input$queue_birthday, floor(as.numeric(difftime(Sys.Date(), as.Date(input$queue_birthday)))/365) >= 21)
      if (state != "OR") {
        req(input$queue_id, nchar(input$queue_id) %in% 8:9)
        ### check if already in db
        ### if not add them then to db then to queue
        if (input$queue_id %in% patients()$id) {
          id <- patients() %>% filter_(~id == input$queue_id) %>% slice(1) %>% pull("idpatient")
        } else {
          con <- pool::poolCheckout(pool)
          i_f_new_patient(con, input$queue_id, NA,firstName = stringr::str_split(input$queue_name, " ", 2)[[c(1, 1)]],
                          lastName = stringr::str_split(input$queue_name, " ", 2)[[c(1, 2)]], NA, input$queue_birthday, NA, NA, NA, NA, 3)
          id <- last_insert_id(con)
          pool::poolReturn(con)
        }
        i_f_add_queue(pool, id, input$queue_id %in% patients()$id, facilityNumber = facility()$idfacility)
      } else {
        i_f_add_queue(pool, NA, FALSE, input$queue_name, facilityNumber = facility()$idfacility)
      }
      trigger(trigger() + 1)
      trigger_new(trigger_new() + 1)
      removeModal()
    })
    
    observeEvent(input$queue_med, {
      req(input$queue_name, floor(as.numeric(difftime(Sys.Date(), as.Date(input$queue_birthday)))/365) >= 18)
      req(input$queue_id)
      if (input$queue_id %in% patients()$id) {
        i_f_add_queue(pool, patients()$idpatient[patients()$id == input$queue_id], TRUE, facilityNumber = facility()$idfacility)
        trigger(trigger() + 1)
      } else {
        new_row <- data.frame(
          firstName = stringr::str_split(input$queue_name, " ", 2)[[c(1, 1)]],
          lastName = stringr::str_split(input$queue_name, " ", 2)[[c(1, 2)]],
          id = input$queue_id,
          addDate = mySql_date(Sys.Date()),
          verified = 1, birthday = input$queue_birthday
        )
        con <- pool::poolCheckout(pool)
        DBI::dbWriteTable(con, "patient", new_row, append = TRUE, rownames = FALSE)
        id <- last_insert_id(con)
        pool::poolReturn(con)
        trigger(trigger() + 1)
        trigger_new(trigger_new() + 1)
        reload_patient(list(selected = id, time = Sys.time(), type = "patient"))
        removeModal()
        # trigger_new(trigger_new() + 1)
      }
    })
    
    observeEvent(input$new_store, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');"
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Let Customer into Store"),
        tags$form(id = session$ns("store_form"),
          class = "center",
        tagList(textInput(session$ns("store_name"), "Name"),
              tagList(tags$br(),
                      textInput(session$ns("store_id"), "ID #"),
                      tags$br(),
                      textInput(session$ns("store_birthday"), "Birthday"),
                      ### add birthday #####
                      tags$script(
                        '$("#frontdesk-store_id, #frontdesk-store_name, #frontdesk-store_birthday").attr("required", true)',
                        'var CannaBirthday = new Cleave("#frontdesk-store_birthday", {
        date: true,
        datePattern: ["m", "d", "Y"]
      });$("#frontdesk-store_birthday").attr("data-parsley-age","21");'
                      ))
            )       
        ),
        footer = 
          tagList(
            if (isTRUE(facility()$medical == 1))
              parsleyr::submit_form(
                session$ns("store_med"),
                label = "Add Medical",
                class = "btn btn-info add-queue-btn",
                onclick = I('$("#frontdesk-store_birthday").attr("data-parsley-age","18");'),
                formId = session$ns("store_form")
              ),
            if (isTRUE(facility()$recreational == 1))
              parsleyr::submit_form(
                session$ns("store_rec"),
                label = "Add Recreational",
                class = "btn btn-info add-queue-btn",
                onclick = I('$("#frontdesk-store_birthday").attr("data-parsley-age","21");'),
                formId = session$ns("store_form")
              )
          )))
    })
    
    observeEvent(input$store_med, {
      req(input$store_name,input$store_id, nchar(input$store_id) %in% 8:9, input$store_birthday,
          floor(as.numeric(difftime(Sys.Date(), as.Date(input$store_birthday)))/365) >= 18)
      if (input$store_id %in% patients()$id) {
        i_f_let_in(pool, patients()$idpatient[patients()$id == input$store_name], FALSE)
        trigger(trigger() + 1)
      } else {
        new_row <- data.frame(
          firstName = stringr::str_split(input$store_name, " ", 2)[[c(1, 1)]],
          lastName = if (length(stringr::str_split(input$store_name, " ", 2)[[1]]) == 1) "" else stringr::str_split(input$store_name, " ", 2)[[c(1, 2)]],
          id = input$store_id,
          addDate = mySql_date(Sys.Date()),
          verified = 1, birthday = input$store_birthday
        )
        con <- pool::poolCheckout(pool)
        DBI::dbWriteTable(con, "patient", new_row, append = TRUE, rownames = FALSE)
        id <- last_insert_id(con)
        pool::poolReturn(con)
        # i_f_let_in(pool, id, TRUE)
        trigger(trigger() + 1)
        trigger_patients(trigger_patients() + 1)
        trigger_new(trigger_new() + 1)
        reload_patient(list(selected = id, time = Sys.time(), type = "patient"))
        removeModal()
        # trigger_new(trigger_new() + 1)
      }
    })
    
    observeEvent(input$store_rec, {
      req(input$store_name)
      req(input$store_id)
      req(input$store_birthday, floor(as.numeric(difftime(Sys.Date(), as.Date(input$store_birthday)))/365) >= 21)
      if (input$store_id %in% patients()$id) {
        id <- patients() %>% filter_(~id == input$store_id) %>% slice(1) %>% pull("idpatient")
      } else {
        con <- pool::poolCheckout(pool)
        i_f_new_patient(con, id = input$store_id, firstName = stringr::str_split(input$store_name, " ", 2)[[c(1, 1)]],
                        lastName = if (length(stringr::str_split(input$store_name, " ", 2)[[1]]) == 1) "" else stringr::str_split(input$store_name, " ", 2)[[c(1, 2)]],
                        NA, input$store_birthday, NA, NA, NA, NA, NA, verified = 3)
        id <- last_insert_id(con)
        pool::poolReturn(con)
      }
      i_f_let_in(pool, id, !input$store_id %in% patients()$id, facilityNumber = facility()$idfacility)

      trigger(trigger() + 1)
      trigger_patients(trigger_patients() + 1)
      trigger_new(trigger_new() + 1)
      removeModal()
    })
    
    observe({
      req(queue())
      dataTableAjax(session, online() %>% select_(~-idtransaction, ~-email) %>% 
                      mutate_(index = ~row_number(),
                              timeIn = ~as.character(as.POSIXct(
                                hms::as.hms(timeIn)
                              ), "%I:%M %p")) %>% 
                      select_(Name = ~name, Phone = ~phone, Status = ~status, Time = ~timeIn, Total = ~revenue, ` ` = ~index), rownames = TRUE, outputId = "online")
      reloadData(online_proxy, resetPaging = FALSE)
    })
    
    output$online <- DT::renderDataTable({
      isolate(online()) %>% select_(~-idtransaction, ~-email) %>% 
        mutate_(index = ~row_number(),
                timeIn = ~as.character(as.POSIXct(
                  hms::as.hms(hms::as.hms(timeIn))
                ), "%I:%M %p")) %>% 
        select_(Name = ~name, Phone = ~phone, Status = ~status, Time = ~timeIn, Total = ~revenue, ` ` = ~index)
    }, rownames = TRUE, width = "100%", server = TRUE, 
    selection = "none", options = list(
      dom = 'tp',
      drawCallback = JS(
        'function() {
        $(".even").removeClass("even").addClass("odd");
  } '
      ), columnDefs = list(
        list(
          targets = 0:6, className = "dt-center", orderable = FALSE
        ),
        list(
          targets = 0, width = "2.5%"
        ),
        list(
          targets = 1, width = "15%"
        ),
        list(
          targets = 2, width = "15%"
        ),
        list(
          targets = 3, width = "100px",
          render = JS(
            "function(data, type, row, meta) {
              return data === 5 ? '<span =class \"unconfirmed\">Unconfirmed</span>' : '<span =class \"confirmed\">Confirmed</span>'; 
            }"
          )
        ),
        list(
          targets = 4, width = "14%"
        ),
        list(
          targets = 5, width = "10%", render = JS(
            "function(data, type, row, meta) {
              return '$' + data;
            }"
          )
        ),
        list(
          targets = 6, width = "", render = JS(
            paste0(
              'function(data, type, row, meta) {
           return "<button row = \'" + data + "\' class = \'btn btn-info let-in-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
              session$ns("onlineSale"),'\\")\'>Process</button>";
  }'
            )
          )
        )
      )))
    
    observe({
      req(queue())
      dataTableAjax(session, queue() %>% select_( ~ -idtransaction) %>% 
                      mutate_(
                        recreational = ~ if_else(as.logical(recreational), "R", "M"),
                        letIn = ~ row_number(),
                        remove = ~ row_number(),
                        info = ~ if (state == "OR") {
                          NA_integer_
                          } else {
                            if_else(is.na(idpatient), NA_integer_, row_number())
                            },
                        timeIn = ~ as.character(as.POSIXct(
                          hms::as.hms(hms::as.hms(timeIn))
                        ), "%I:%M %p"),
                        name = ~ if_else(is.na(id), name, paste0(name, " (", id, ")"))
                      ) %>%
                      select_(
                        ` ` = ~ recreational,
                        Name =  ~ name,
                        `Time` = ~ timeIn,
                        ~ letIn,
                        ~ info,
                        ~ remove
                      ), rownames = FALSE, outputId = "queue")
      reloadData(queue_proxy, resetPaging = FALSE)
    })
    
    output$queue <- DT::renderDataTable({
      isolate(queue()) %>% select_( ~ -idtransaction) %>% 
        mutate_(
          recreational = ~ if_else(as.logical(recreational), "R", "M"),
          letIn = ~ row_number(),
          remove = ~ row_number(),
          info = ~ if_else(is.na(idpatient), NA_integer_, row_number()),
          timeIn = ~ as.character(as.POSIXct(
            hms::as.hms(hms::as.hms(timeIn))
          ), "%I:%M %p"),
          name = ~ if_else(is.na(id), name, paste0(name, " (", id, ")"))
        ) %>%
        select_(
          ` ` = ~ recreational,
          Name =  ~ name,
          `Time` = ~ timeIn,
          ~ letIn,
          ~ info,
          ~ remove
        )
    }, rownames = FALSE, width = "100%", server = TRUE, options = list(
      dom = 'tp',
      drawCallback = JS(
        'function() {
        $(".even").removeClass("even").addClass("odd");
  } '
),
columnDefs = list(
  list(
    targets = 0:5,
    orderable = FALSE,
    className = "dt-center"
  ),
  list(targets = 0,
       width = "5%"),
  list(targets = 1,
       width = "20%"),
  list(targets = 2,
       width = "14%"),
  list(targets = 3:5,
       width = "18%"),
  list(targets = 3,
       render = JS(
         paste0(
           'function(data, type, row, meta) {
           return "<button row = \'" + data + "\' class = \'btn btn-info let-in-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
           session$ns("let"),'\\")\'>Let In</button>";
  }'
)
         )),
list(targets = 4, visible = state != "OR",
     render = JS(
       paste0(
         'function(data, type, row, meta) {
         return data ? "<button row = \'" + data + "\' class = \'btn btn-info let-in-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
         session$ns("infoQ"),'\\");CannaFrontdesk.change_tab(\\"patientInfo\\");\'>Info</button>" : "";
      }'
       )
       )),
list(targets = 5,
     render = JS(
       paste0(
         'function(data, type, row, meta) {
         return "<button row = \'" + data + "\' class = \'btn btn-info delete-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
         session$ns("removeQ"),'\\")\'>Remove</button>";
  }'
       )
       ))
     )
       ), colnames = c("Name", "Time", "", "", ""),
selection = 'none')
    
    
    observe({
      req(in_store())
      dataTableAjax(session, in_store() %>% select_( ~ -idtransaction) %>% 
                    mutate_(
                      recreational = ~ if_else(as.logical(recreational), "R", "M"),
                      remove = ~ row_number(),
                      info = ~ if_else(is.na(idpatient), NA_integer_, row_number()),
                      timeIn = ~ as.character(as.POSIXct(
                        hms::as.hms(hms::as.hms(timeIn))
                      ), "%I:%M %p"),
                      name = ~ if_else(is.na(id), name, paste0(name, " (", id, ")"))
                    ) %>%
                    select_(
                      ` ` = ~ recreational,
                      Name = ~ name,
                      `Time` = ~ timeIn,
                      ~ info,
                      ~ remove
                    ), rownames = FALSE, outputId = "store")
      reloadData(store_proxy, resetPaging = FALSE)
    })
    
    output$store <- DT::renderDataTable({
      isolate(in_store()) %>% select_( ~ -idtransaction) %>% 
        mutate_(
          recreational = ~ if_else(as.logical(recreational), "R", "M"),
          remove = ~ row_number(),
          info = ~ if_else(is.na(idpatient), NA_integer_, row_number()),
          timeIn = ~ as.character(as.POSIXct(
            hms::as.hms(hms::as.hms(timeIn))
          ), "%I:%M %p"),
          name = ~ if_else(is.na(id), name, paste0(name, " (", id, ")"))
        ) %>%
        select_(
          ` ` = ~ recreational,
          Name = ~ name,
          `Time` = ~ timeIn,
          ~ info,
          ~ remove
        )
    }, rownames = FALSE, server = TRUE, options = list(
      dom = 'tp',
      drawCallback = JS(
        'function() {
        $(".even").removeClass("even").addClass("odd");} '
      ),
      columnDefs = list(
        list(
          targets = 0:4,
          orderable = FALSE,
          className = "dt-center"
        ),
        list(targets = c(0),
             width = "5%"),
        list(targets = 1,
             width = "20%"),
        list(targets = 2,
             width = "14%"),
        list(targets = 3:4,
             width = "27%"),
        list(targets = 3, visible = state != "OR",
             render = JS(paste0(
               'function(data, type, row, meta) {
               return data ? "<button row = \'" + data + "\' class = \'btn btn-info let-in-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
               session$ns("infoS"),'\\");CannaFrontdesk.change_tab(\\"patientInfo\\");\'>Info</button>" : "";
  }'
             ))),
        list(targets = 4,
             render = JS(paste0(
               'function(data, type, row, meta) {
               return "<button row = \'" + data + "\' class = \'btn btn-info delete-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
               session$ns("removeS"),'\\")\'>Remove</button>";
         }'
             )))
             )
    ), colnames = c("Name", "Time", "", ""),
    selection = 'none')
    
    
    return(queue_store)
         }

allPatientsUI <- function(id) {
  ns <- NS(id)
  
  tagList(div(
    class = "content",
    col(12,
          box(
          h1("Incomplete Profiles"),
          DT::dataTableOutput(ns("incomplete"))
        )
        ),
    col(12,
        box(
          h1("All Customers"),
          DT::dataTableOutput(ns("patients"))
        ))
  ))
}

allPatients <-
  function(input,
           output,
           session,
           pool,
           reload_patient,
           trigger_patients) {
    
    patients <- reactive({
      trigger_patients()
      q_f_patients(pool)
    })
    
    new_patients <- reactive({
      trigger_patients()
      q_f_new_patients(pool)
    })
    
    observeEvent(input$info, {
      reload_patient(list(selected = patients()$idpatient[input$info$row], time = Sys.time(), type = "patient"))
    })
    
    observeEvent(input$complete, {
      reload_patient(list(selected = new_patients()$idpatient[input$complete$row], time = Sys.time(), type = "patient"))
    })
    
    output$patients <- DT::renderDataTable({
      patients() %>% 
        mutate_(info = ~row_number(),
                expirationDate = ~ format(as.Date(expirationDate), "%m/%d/%Y"),
                lastTransaction = ~format(as.Date(lastTransaction), "%m/%d/%Y"),
                age = ~ if_else(is.na(birthday), NA_integer_, as.integer(floor(as.numeric(Sys.Date()-as.Date(birthday))/365))),
                name = ~ if_else(is.na(id), name, paste0(name, " (", id, ")"))
        ) %>% 
        select_(Name = ~name, 
                Age = ~age, `Exp Date` =~expirationDate,`Last Transaction` = ~lastTransaction, 
                ~info)
    }, colnames = c("Name",  "Age", "Exp Date", "Last Transaction", 
                    ""),
    options = list(
      dom = 'tp',
      columnDefs = list(list(
        targets = 0:4,
        className = "dt-center"
      ),
      list(
        targets = 0,
        width = "15%"
      ),
      list(
        targets = 1,
        width = "3%",
        render = JS(
          'function(data, type, row, meta) {
          return data < 21 ? "<span style = \'color:red\'/>" + data + "</span>" : data;
  }'
        )
        ),
      list(
        targets = 2,
        width = "7%"
      ),
      list(
        targets = 3,
        width = "7%"
      ),
      list(
        targets = 4,
        width = "8%",
        orderable = FALSE,
        render = JS(
          paste0(
            'function(data, type, row, meta) {
            return "<button row = \'" + data + "\' class = \'btn btn-info let-in-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
            session$ns("info"),'\\");CannaFrontdesk.change_tab(\\"patientInfo\\");\'>Info</button>";
  }'
          )
          )
        )
        ),
      drawCallback = JS(
        'function() {
        $(".even").removeClass("even").addClass("odd");} '
      )
      ),
    selection = 'none', rownames = FALSE)
    
    output$incomplete <- DT::renderDataTable({
      new_patients() %>%
        mutate_(index = ~row_number(),
                addDate = ~format(as.Date(addDate), "%m/%d/%Y")) %>%
        select_(~name, ~id, ~addDate, ~index)
    }, colnames = c("Name", "ID #", "Add Date", ""),
    options = list(
      pageLength = 5,
      dom = 'tp',
      columnDefs = list(list(
        targets = 0:3,
        className = "dt-center"
      ),
      list(
        targets = 0,
        width = "15%"
      ),
      list(
        targets = 1,
        width = "10%"
      ),
      list(
        targets = 2,
        width = "10%"
      ),
      list(
        targets = 3,
        width = "10%",
        orderable = FALSE,
        render = JS(
          paste0(
            'function(data, type, row, meta) {
            return "<button row = \'" + data + "\' class = \'btn btn-info let-in-btn index-btn\' onclick = \'CannaFrontdesk.button(this, \\"',
            session$ns("complete"),'\\");CannaFrontdesk.change_tab(\\"newPatient\\");\'>Complete Profile</button>";
  }'
          )
          )
        )
        ),
      drawCallback = JS(
        'function() {
        $(".even").removeClass("even").addClass("odd");} '
      )
      ),
    selection = 'none', rownames = FALSE)
    
    return(patients)
  }

onlineOrdersUI <- function(id) {
  ns <- NS(id)
  
  tagList(div(class = "content",
              tags$form(
                id = ns("onlineSale"),
                class = "form",
                col(6,
                  div(
                    div(class = "name-container",
                        uiOutput(ns(
                          "name"
                        )))),
                  box(h1("Patient Info", style = "width:100%"),
                      DT::dataTableOutput(ns("patient_info")))
                ),
                col(6,
                  div(
                    class = "row",
                    div(
                      class = "add-delete-btn-container",
                      tags$button(id = ns("cancel"), "Cancel", 
                                  class = "btn btn-info delete-btn action-button", 
                                  style = "width:25%", formnovalidate = NA),
                      parsleyr::submit_form(
                        ns("submit"),
                        "Confirm",
                        formId = ns("onlineSale"),
                        class = "btn btn-info add-queue-btn",
                        style = "width:25%"
                      ),
                      tags$button(id = ns("print"), "Labels",
                                  class = "btn btn-info add-queue-btn action-button",
                                  style = "width: 25%", formnovalidate = NA)
                    )
                  ),
                  box(h1("Order Info", style = "width:100%"),
                      DT::dataTableOutput(ns("order_info")))
                ),
                col(12,
                    box(
                    h1("Cart", style = "width:100%"),
                    DT::dataTableOutput(ns("cart"))
                    ))
              )))
}

onlineOrder <- function(input, output, session, pool, transactionId, order_info, trigger, reload_select, 
                        patients, printers, base_url, msg_service_sid, clientName) {
  
  trigger_order_info <- reactiveVal(0)
  sales <- reactive({
    req(transactionId())
    trigger_order_info()
    q_f_online_sale(pool, transactionId())
  })
  
  discount <- reactive({
    trigger_order_info()
    q_c_active_discounts(pool, transactionId())
  })
  
  observe({
    status <- order_info()$status[1]
    req(status)
    if (status == 5) {
      updateActionButton(session, "submit", label = "Confirm")
    } else {
      updateActionButton(session, "submit", label = "Link")
    }
  })
  
  output$name <- renderUI({
    if (isTruthy(order_info()$name)) {
      h1(order_info()$name[1])
    } else {
      h1("Select an online order")
    }
  })
  
  output$patient_info <- DT::renderDataTable({
    req(order_info())
    order_info() %>% 
      mutate_(phone = ~ paste0("(", substring(phone, 1, 3), ") ", substring(phone, 4, 6), "-", substring(phone, 7))) %>%
      select_(Name = ~name, Phone = ~phone, Email = ~email) %>% slice(1) %>%
      t()
  }, rownames = TRUE, class = "table dt-row", selection = 'none', 
  options = list(dom = "t", 
                 columnDefs = list(
    list(
      targets = 0,
      render = JS(
        "function(data, type, row, meta) {
            return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
    }"
      )
    )
  )))
  
  output$order_info <- DT::renderDataTable({
    req(order_info())
    data.frame(
      Time = as.character(as.POSIXct(
        hms::as.hms(order_info()$timeIn[1])
      ), "%I:%M %p"),
      Status = order_info()$status[1],
      Total = order_info()$revenue[1]
    ) %>% t()
  }, rownames = TRUE, class = "table dt-row", selection = 'none',
  options = list(dom = "t", columnDefs = list(
    list(
      targets = 0,
      render = JS(
        "function(data, type, row, meta) {
            return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
    }"
      )
    ),
    list(targets = 1,
         render = JS(
           "function(data, type, row, meta) {
            return row[0] === 'Status' ? (parseInt(data) === 5 ? '<span =class \"unconfirmed\">Unconfirmed</span>' : '<span =class \"confirmed\">Confirmed</span>') : 
              row[0] === 'Total' ? '$' + data : data;
           }"
         ))
  )))
  
  output$cart <- DT::renderDataTable({
    req(transactionId())
    discount <- discount() %>% mutate_(index = ~if_else(is.na(idsale), row_number(), NA_integer_),
                                       name = ~if_else(is.na(name), reason, name),
                                       discount = ~if_else(unit == "$", discount, 
                                                           if_else(is.na(idsale), sum(sales()$revenue), 
                                                                   if (nrow(sales())>0) {(sales()$revenue[sales()$idsale == idsale])} else {0})  * discount / 100)) %>%
      select_( ~ idsale, ~ iddiscount, ~ name, price = ~ discount, ~ index)
    
    sales() %>%
      mutate_(
        index = ~ row_number()
      ) %>%
      select_( ~ index, ~ name, ~ type, ~ quantity, price =  ~ revenue, ~idsale) %>%
      bind_rows(discount) %>% arrange_(~idsale, ~desc(price)) %>% 
      mutate_(price = ~ format(price, digits = 2, nsmall = 2, scientific = FALSE)) %>%
      select_( ~ index, ~ name, ~ type, ~ quantity, ~ price, ~idsale)

  }, rownames = FALSE, colnames = c('', 'Product', 'Type', 'Quantity', 'Price', ''), 
  selection = "none", options = list(dom = "tp",
                                     rowCallback = JS(
                                       'function(row, data, index) {
                                        if (!data[3]) {
                                          $(row).addClass("discount");
                                        }
                                       }'
                                     ),
                                     drawCallback = JS(
                                       'function() {
        $(".even").removeClass("even").addClass("odd");
  } '),
                                      columnDefs = list(list(
                                        targets = 0:4, className = "dt-center"
                                      ),
                                      list(
                                        targets = 5, visible = FALSE
                                      ),
                                      list(
                                        targets = 0, orderable = FALSE,
                                        width = "2.5%",
                                        render = JS(
                                          "function(data, type, row, meta) {
            return data ? '<a id = \"edit' + data + '\" ><i class = \"fa fa-pencil fa-2x\" row = \"' + data + '\" onclick = \"CannaFrontdesk.edit_item(this)\"></i></a>' : '';
}"
                                        )
                                      ),
                                      list(
                                        targets = 1, width = "25%"
                                      ),
                                      list(
                                        targets = 2,
                                        width = "50px",
                                        render = JS(
                                          'function(data, type, row, meta) {
            return data ? "<img class=\\"product-image cart-image\\" src = \\"https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/" + data.toLowerCase() + ".svg\\">" : "";
  }'
                                        )
                                      ),
                                      list(
                                        targets = 3, width = "50px", render = JS(
                                          "function(data, type, row, meta) {
                                            return data ? ['flower', 'concentrate'].indexOf(row[2]) >= 0 ? data + ' g' : data + ' pkg': '';
                                          }"
                                        )
                                      ),
                                      list(
                                        targets = 4, width = "50px", render = JS(
                                          "function(data, type, row, meta) {
                                            return data ? row[3] ? '$' + data : '<span style = \"color:red\">-$' + Math.abs(data) + '</span>' : '$0';
                                          }"
                                        )
                                      )
                                      )))
  
  observeEvent(input$edit_item, {
    req(input$edit_item$row)
    info <- sales() %>% slice_(~as.numeric(input$edit_item$row))
    dis <- discount()[isTRUE(discount()$idsale ==info$idsale),]
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal move-icon"), `data-dismiss` = "modal", style = "padding: 15px"),
        tags$script(
          "$('.modal-content').css('background-color', '#061726');$('.modal-body').css('overflow','auto');$('.modal-dialog').css('width', '70%');"
        ),
        add_to_cartUI(session$ns("edit_online"), reactive(info$type), reactive(info$name), discount = dis$discount, unit = dis$unit, reason = if (isTruthy(dis$reason)) dis$reason else dis$idcoupon,
                      quantity = info$quantity, price = info$revenue,
                      coupon = coupons(), margin_top = 30, strainType = c("hybrid", "sativa", "indica")[c(
                        as.logical(c(info$hybrid, info$sativa, info$indica))
                      )]),
        footer = tagList(
          actionButton(session$ns("edit"), "Submit", style = "float:left;", class = "btn-info add-queue-btn"),
          actionButton(session$ns("remove"), "Remove", class = "btn-info delete-btn")
        )
      )
    )
  })
  
  observeEvent(input$print, {
    req(transactionId())
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').addClass('table-container');"
        ),
        selectizeInput(session$ns("printer"), "Printer", choices = structure(printers$id, names = printers$name)),
        footer = tags$button(id = session$ns("submit_print"), "Print", class = "btn btn-info add-queue-btn action-button")
      )
    )
  })
  
  observeEvent(input$submit_print, {
    req(input$printer)
    req(transactionId())
    need_labels <- sales() %>% filter_(~type %in% c("flower", "concentrate"))
    
    mcparallel({
      for (i in seq_len(nrow(need_labels))) {
        print_label(
          inventoryId = need_labels$idinventory[i],
          name = paste0(need_labels$name[i], " (", paste0(c("I", "S", "H")[which(c(
            need_labels$indica[i] ==
              1,
            need_labels$sativa[i] == 1,
            need_labels$hybrid[i] == 1
          ))], collapse = "/"), ")"),
          quantity = paste(need_labels$quantity[i], if (need_labels$type[i] %in% c("flower", "concentrate")) "g" else "pkg"), 
          template = system.file(package = "CannaInventory", "templates", "label.html"),
          base_url = base_url,
          width = 1100,
          height = 400,
          printer = input$printer,
          key = gsub("\n  ", "\n", getOption("canna_key"))
        )
      }
    })
    
    removeModal()
  })
  
  coupons <- reactive({
    x <- q_c_coupons(pool)
    reactive(structure(x$id, names = x$name))
  })
  
edited_item <- callModule(add_to_cart, "edit_online", pool, {
  req(input$edit_item$row)
  reactive(sales() %>% slice_(~as.numeric(input$edit_item$row)) %>% pull("type"))
  },
                            {
                              req(input$edit_item$row)
                              reactive(sales() %>% slice_(~input$edit_item$row) %>% pull("idinventory"))
                              }, 
                            {
                              NULL
                              },
                            {
                              NULL
                              }, edit = TRUE, coupon = coupons(), strainType = reactive(c("hybrid", "sativa", "indica")[c(
                                as.logical(c(sales() %>% slice_(~input$edit_item$row) %>% pull("hybrid"),
                                             sales() %>% slice_(~input$edit_item$row) %>% pull("sativa"), 
                                             sales() %>% slice_(~input$edit_item$row) %>% pull("indica")))
                              )]))

  observeEvent(input$edit, {
    ### edit sale
    req(edited_item())
    req(input$edit_item$row)
    info <- sales() %>% slice_(~input$edit_item$row)
    conn <- pool::poolCheckout(pool)
    dbBegin(conn)
    u_c_sale(pool, info$idsale, edited_item()$price, edited_item()$quantity - info$quantity)
    
    if (length(discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)]) > 0 && isTRUE(edited_item()$discount > 0)) {
      u_c_discount(conn, discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)], edited_item()$discount, edited_item()$unit, edited_item()$idcoupon, edited_item()$reason,
                   if (edited_item()$unit == "$") edited_item()$discount else (edited_item()$discount/100) * edited_item()$price)
    } else if (isTRUE(edited_item()$discount > 0)) {
      i_c_discount(conn, transactionId = transactionId(), saleId = info$idsale,
                   discount = edited_item()$discount, unit= edited_item()$unit, reason = edited_item()$reason, couponId = edited_item()$idcoupon,
                   if (edited_item()$unit == "$") edited_item()$discount else (edited_item()$discount/100) * edited_item()$price)
    } else if (length(discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)]) > 0) {
      d_c_discount(conn, discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)])
    }
    
    if (q_c_quantity(conn, info$idinventory) < 0) {
      # should explicitely mention quantity?
      showModal(session = session,
                modalDialog(easyClose = TRUE, fade = FALSE,
                            tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
                            tags$script("$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"),
                            h1("Warning! Not enough inventory")))
      DBI::dbRollback(conn)
    } else {
      DBI::dbCommit(conn)
    }
    
    trigger_order_info(trigger_order_info() + 1)
    removeModal()
  })
  
  observeEvent(input$remove, {
    ### remove sale
    req(input$edit_item$row)
    info <- sales() %>% slice_(~input$edit_item$row)
    d_c_remove_sale(pool, info$idsale, info$idinventory, info$quantity)
    trigger_order_info(trigger_order_info() + 1)
    removeModal()
  })
  
  observeEvent(input$submit, {
    ### check status then do right thing
    if (order_info()$status == 5) {
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ),
        h1("Confirmation Message"),
        span(class = "text-msg",
        textAreaInput(session$ns("confirm_msg"), NULL, value = sprintf("Your order has been confirmed. Remember to bring your ID, and rec with you to %s.", clientName),
                      rows = 3)
      ), footer = actionButton(session$ns("send_confirm"), "Confirm Order", class = "btn-info add-queue-btn")
      )
    )
    } else if (order_info()$status == 6) {
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');"
          ),
          h1("Link Online Sale with Patient"),
          selectizeInput(session$ns("patient"), "Patient", 
                         choices = patients(),
                         options = list(
            onInitialize = I("function() {this.setValue('');}"),
            placeholder = "Patient"
          )),
          footer = actionButton(session$ns("link"), "Link Order", class = "btn-info add-queue-btn")
        )
      )
    }
  })
  
  observeEvent(input$link, {
    req(input$patient)
    u_f_link_order(pool, transactionId(), input$patient)
    reload_select(list(id = NULL))
    removeModal()
  })
  
  observeEvent(input$send_confirm, {
    req(input$confirm_msg)
    con <- pool::poolCheckout(pool)
    DBI::dbBegin(con)
    u_f_confirm_order(con, transactionId(), sales()$idinventory, sales()$quantity)
    
    quantities <- vapply(sales()$idinventory, function(x) {
      q_c_quantity(con, x)
    }, numeric(1))
    
    if (any(quantities < 0)) {
      DBI::dbRollback(con)
      showModal(
        modalDialog(
          easyClose = TRUE, fade = FALSE,
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("There was not enough inventory of ", paste0(sales()$name[quantities < 0], collapse = ", "))
        )
      )
    } else {
      DBI::dbCommit(con)
      showModal(
        modalDialog(
          easyClose = TRUE, fade = FALSE,
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Order confirmed! Remember to link order with patient when they arrive.")
        )
      )
      trigger(trigger() + 1)
      tw_send_message(paste0("+1", order_info()$phone), msg_service_id = msg_service_sid, body = input$confirm_msg)
    }
    pool::poolReturn(con)
  })
  
  observeEvent(input$cancel, {
    ### present cancellation text option
    ### remove transaction
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ),
        h1("Cancellation Message"),
        span(class = "text-msg",
        textAreaInput(session$ns("cancel_msg"), NULL, value = "We are sorry we were unable to process your order. Please contact (xxx)-xxx-xxxx ",
                      rows = 3)
      ),
      footer = actionButton(session$ns("send_cancel"), "Cancel Order", class = "btn-info delete-btn")
      )
    )
  })
  
  
  
  observeEvent(input$send_cancel, {
    req(input$cancel_msg)
    d_f_online_sale(pool, transactionId())
    reload_select(list(id = NULL))
    trigger(trigger() + 1)
    removeModal()
    tw_send_message(paste0("+1", order_info()$phone), msg_service_id = msg_service_sid, body = input$cancel_msg)
  })
  
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