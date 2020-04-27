#' Load objects into the global environment
#' 
#' @description The global.R file allows us to load libraries, 
#'              functions, data sets etc. into the global 
#'              environment in which the app is created .
#'              
#'              All objects created in this file can be used 
#'              throughout the app

# Step 1: load R packages containing functions used throughout the app
  pkg_files <- list.files("www/1_load_pkgs", 
                          pattern = "\\.R$",
                          full.names = T)
  purrr::map(pkg_files, source)

# Step 2: load local data sets that do not change daily
  local_data_files <- list.files("www/2_load_local_data", 
                                 pattern = "\\.rda$",
                                 full.names = T)
  purrr::map(local_data_files, load, envir = environment())

# Step 3: load external data sets from their respective URL's
  ext_data_files <- list.files("www/3_load_external_data", 
                               pattern = "\\.R$",
                               full.names = T)
  purrr::map(ext_data_files, source)

# Step 4: load user-defined R functions dev'd for CHAD app
  R_code_files <- list.files("www/4_load_functions", 
                             pattern = "\\.R$",
                             full.names = T)
  purrr::map(R_code_files, source)

# Step 5: load extra code (this is non-function & non-data code)
  code_files <- list.files("www/5_load_extra_code", 
                           pattern = "\\.R$",
                           full.names = T)
  purrr::map(code_files, source)
  
# Step 6: include info docs 
  InfoLink = shiny::includeMarkdown("www/6_load_info_docs/InputsInfo.md")
  CalcLink <- includeMarkdown("www/6_load_info_docs/CalcInfo.md")
  SourceLink <- includeMarkdown("www/6_load_info_docs/SourceInfo.md")
  OverviewLink <- includeMarkdown("www/6_load_info_docs/OverviewInfo.md")
  ProjLink <- includeMarkdown("www/6_load_info_docs/ProjInfo.md")
