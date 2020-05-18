makeCheckboxButton <- function(checkboxValue, buttonId, buttonLabel, size = "default", style = "default"){
  size <- switch(size, `extra-small` = "btn-xs", small = "btn-sm", 
                 large = "btn-lg", "default")
  style <- paste0("btn-", style)
  
  tags$script(HTML(paste0("
          $(document).ready(function() {
            var inputElements = document.getElementsByTagName('input');
            for(var i = 0; i < inputElements.length; i++){
              var input = inputElements[i];

              if(input.getAttribute('value') == '", checkboxValue, "'){

                var button = document.createElement('button');
                button.setAttribute('id', '", buttonId, "');
                button.setAttribute('type', 'button');
                button.setAttribute('class', '", paste("btn action-button", style , size), "');
                button.appendChild(document.createTextNode('", buttonLabel, "'));

                input.parentElement.parentElement.appendChild(button);
             };
            }
          });
        ")))
}