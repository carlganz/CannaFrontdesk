/*
Copyright (C) 2017 CannaData Solutions

This file is part of CannaFrontdesk.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.
*/

CannaFrontdesk = function() {
  return {
    parsley_init: function() {
      // california ID validation
      window.Parsley.addValidator("californiaid", {
        requirementType: 'string',
        validateString: function(value, requirement) {
          if (value.length !== 8) {
            return false;
          }
          if (value.substring(0, 1).search(/^[a-z]+$/i) === -1) {
            return false;
          }

          for (var i = 1; i < 8; i++) {
            if (value.substring(i, i + 1).search(/^[0-9]+$/i) === -1) {
              return false;
            }
          }
          if (value.search(/\W+/g) !== -1) {
            return false;
          }
          return true;
        },
        messages: {
          en: 'The value is not a California ID: one letter followed by 7 numbers'
        }
      });

      window.Parsley.addValidator("year", {
        requirementType: 'string',
        validateString: function(value, requirement) {
          var year = new Date().getFullYear();
          var age = year - parseInt(value.substring(6));
          if (age > 125 || age < 0) {
            return false;
          }
          return true;
        },
        messages: {
          en: 'The year of birth is incorrect'
        }
      });

    },
    read_barcode: function() {
      var pressed = false;
      var chars = [];
      var asc = [];
      var spec = {
        "DBA": "expirationDate",
        "DCS": "lastName",
        "DAC": "firstName",
        "DAD": "middleName",
        "DBB": "birthday",
        "DAG": "address1",
        "DAH": "address2",
        "DAI": "city",
        "DAJ": "state",
        "DAK": "zip",
        "DAQ": "californiaId"
      };
      window.onkeydown = function(e) {
        asc.push(e.which);
        if (e.which === 17) {
          chars.push(String.fromCharCode(10));
        } else if (e.which !== 16) {
          chars.push(String.fromCharCode(e.which));
        }
        if (pressed == false) {
          setTimeout(function() {
            // set to high number so fast typing doesn't trigger
            if (chars.length >= 300) {

              var barcode = chars.join("");
              // need to redo to parse explicitely using regex
              var info = {};
              barcode.split("\n").map(function(row) {
                if (spec[row.substring(1, 4)]) {
                  info[spec[row.substring(1, 4)]] = row.substring(4);
                }
              });
              var today = new Date();
              var dd = today.getDate();
              var mm = today.getMonth() + 1; //January is 0!
              var yyyy = today.getFullYear();
              if (parseInt(info.expirationDate.substring(4)) <= yyyy &&
                parseInt(info.expirationDate.substring(0, 2)) <= mm &&
                parseInt(info.expirationDate.substring(2, 4)) <= dd) {
                alert("Expired ID");
              } else {
                info.time = Date.now();
                // send value to server
                Shiny.onInputChange("read_barcode", info);
              }
            }
            // reset
            chars = [];
            asc = [];
            pressed = false;
            // make small so fast typing doesn't trigger
          }, 2150);
        }
        pressed = true;
      };
    },
    telephone_input: function() {
      var telephoneInputBinding = new Shiny.InputBinding();
      $.extend(telephoneInputBinding, {
        find: function(scope) {
          return $(scope).find('input[type="tel"]');
        },
        getId: function(el) {
          return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function(el) {
          return el.value;
        },
        setValue: function(el, value) {
          el.value = value;
        },
        subscribe: function(el, callback) {
          $(el).on('keyup.telephoneInputBinding input.telephoneInputBinding', function(event) {
            callback(true);
          });
          $(el).on('change.telephoneInputBinding', function(event) {
            callback(false);
          });
        },
        unsubscribe: function(el) {
          $(el).off('.telephoneInputBinding');
        },
        receiveMessage: function(el, data) {
          if (data.hasOwnProperty('value'))
            this.setValue(el, data.value);

          if (data.hasOwnProperty('label'))
            $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

          if (data.hasOwnProperty('placeholder'))
            el.placeholder = data.placeholder;

          $(el).trigger('change');
        },
        getState: function(el) {
          return {
            label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
            value: el.value,
            placeholder: el.placeholder
          };
        },
        getRatePolicy: function() {
          return {
            policy: 'debounce',
            delay: 250
          };
        }
      });

      Shiny.inputBindings.register(telephoneInputBinding);
    },
    reset_file_input: function(param) {
      Shiny.onInputChange(param.id, null);
    },
    sidebar: function() {
      // make shiny navlist the sidebar
      $("#content .nav").parent().attr("id", "sidebar");
      // remove bs classes
      $("#sidebar").removeClass("col-sm-4");
      $("#content").find(".col-sm-8").removeClass("col-sm-8");
      // move sidebar outside of content
      $("#sidebar").detach().prependTo("body");
      // append icons
      $("a[data-value='homepage']").html('<i class="fa fa-home fa-2x"></i><br>Home');
      $("a[data-value='allPatients']").html('<i class="fa fa-users fa-2x"></i><br>All Patients');
      $("a[data-value='patientInfo']").html('<i class="fa fa-user fa-2x"></i><br>Patient Info');
      $("a[data-value='newPatient']").html('<i class="fa fa-user-plus fa-2x"></i><br>New Patient');
      $("a[data-value='preOrders']").html('<i class = "fa fa-shopping-cart"></i><br>Online Sale');
      // add class
      $("#sidebar .nav").wrapAll("<div class = 'icon-bar'/>");
      $("#sidebar .nav").addClass("sidebar-icon-bar");
    },
    // hard coded NS from Shiny Modules
    icon_inputs: function() {
      $("div[data-value='patientInfo']").find("h1:contains('Basic Info')").siblings('i').attr('value', 0).on('click', function() {
        $(this).attr('value', parseInt($(this).attr('value')) + 1);
        Shiny.onInputChange("patient_info-edit_basic_info", $(this).attr('value'));
      });
      $("div[data-value='patientInfo']").find("h1:contains('Notes')").siblings('i').attr('value', 0).on('click', function() {
        $(this).attr('value', parseInt($(this).attr('value')) + 1);
        Shiny.onInputChange("patient_info-edit_notes", $(this).attr('value'));
      });
      $("div[data-value='patientInfo']").find("h1:contains('Medical Info')").siblings('i').attr('value', 0).on('click', function() {
        $(this).attr('value', parseInt($(this).attr('value')) + 1);
        Shiny.onInputChange("patient_info-edit_medical_info", $(this).attr('value'));
      });
      $("div[data-value='patientInfo']").find("h1:contains('Preferences')").siblings('i').attr('value', 0).on('click', function() {
        $(this).attr('value', parseInt($(this).attr('value')) + 1);
        Shiny.onInputChange("patient_info-edit_preferences_info", $(this).attr('value'));
      });
      $("div[data-value='patientInfo']").find("h1:contains('Images')").siblings('i').attr('value', 0).on('click', function() {
        $(this).attr('value', parseInt($(this).attr('value')) + 1);
        Shiny.onInputChange("patient_info-edit_images", $(this).attr('value'));
      });
      $("div[data-value='newPatient']").find("h1:contains('Basic Info')").siblings('i').attr('value', 0).on('click', function() {
        $(this).attr('value', parseInt($(this).attr('value')) + 1);
        Shiny.onInputChange("new_patient-edit_basic_info", $(this).attr('value'));
      });
    },
    reset_parsley: function(params) {
      $("#" + params.id).parsley().reset();
    },
    toggle_expiration: function(params) {
      if (params) {
        $(".countdown-container").addClass("expired").removeClass("notexpired");
      } else {
        $(".countdown-container").addClass("notexpired").removeClass("expired");
      }
    },
    change_tab: function(tab) {
      $("a[data-value='" + tab + "']").trigger('click');
    },
    button: function(it, input) {
      Shiny.onInputChange(input, {row: parseInt($(it).attr("row")), time: Date.now()});
    },
    enable_buttons: function() {
      // init
      $(".checkbox-icons li").each(function(i, val) {
        if ($(val).attr("value") === "true") {
          Shiny.onInputChange($(this).addClass("selected").attr("alt"), true);
        } else {
          Shiny.onInputChange($(this).removeClass("selected").attr("alt"), false);
        }
      });
      // input changes
      $(".checkbox-icons li").on("click", function() {
        if ($(this).attr("value") === "true") {
          Shiny.onInputChange($(this).removeClass("selected").attr("value", "false").attr("alt"), false);
        } else {
          Shiny.onInputChange($(this).addClass("selected").attr("value", "true").attr("alt"), true);
        }
      });
    },
    enable_icons: function() {
      // init
      $(".checkbox-icons li").each(function(i, val) {
        if ($(val).children("img").attr("value") === "true") {
          Shiny.onInputChange($(this).addClass("selected").children("img").attr("id").toLowerCase(), true);
        } else {
          Shiny.onInputChange($(this).removeClass("selected").children("img").attr("id").toLowerCase(), false);
        }
      });
      // input changes
      $(".checkbox-icons li").on("click", function() {
        if ($(this).children("img").attr("value") === "true") {
          Shiny.onInputChange($(this).removeClass("selected").children("img").attr("value", "false").attr("id").toLowerCase(), false);
        } else {
          Shiny.onInputChange($(this).addClass("selected").children("img").attr("value", "true").attr("id").toLowerCase(), true);
        }
      });
    },
    edit_item: function(el) {
      Shiny.onInputChange("online_order-edit_item", {row: parseInt($(el).attr("row")),time: Date.now() });
    },
    require: function(params) {
      $("#" + params.id).prop("required", params.required);
    },
    click_alert: function(param) {
      Shiny.onInputChange("click_alert", {box : param, time : Date.now()});
    }
  };
}();

$(document).ready(function() {
  CannaFrontdesk.parsley_init();
  CannaFrontdesk.read_barcode();
  CannaFrontdesk.telephone_input();
  CannaFrontdesk.sidebar();
  CannaFrontdesk.icon_inputs();
  Shiny.addCustomMessageHandler("reset_file_input", CannaFrontdesk.reset_file_input);
  Shiny.addCustomMessageHandler("reset_parsley", CannaFrontdesk.reset_parsley);
  Shiny.addCustomMessageHandler("toggle_expiration", CannaFrontdesk.toggle_expiration);
  Shiny.addCustomMessageHandler("require", CannaFrontdesk.require);
});