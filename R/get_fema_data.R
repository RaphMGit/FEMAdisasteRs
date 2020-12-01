# filename: get_fema_data
# GMU team2
# FEMA was created in April 1979; filter data to 1980 or later by fyDeclared, data type=number
# api_params <- "filter=fyDeclared gt 1979"
# update: November 27, 2020 EMC
# How do libraries get loaded in a PKG?
library("rvest")
library("xml2")
library("httr")
library("jsonlite")
library("robotstxt")

#' example-user must supply entity plus desired api params
#' get_fema_data("DisasterDeclarationsSummaries")
#' @title Get FEMA Open API Data
#'
#' @param entity Character. Name of entity to scrape. See function `get_fema_entites()` for details.
#' @param base_url Character. "https://www.fema.gov/api/open" (default),  website Url of FEMAs API.
#' @param api_params named list. extra arguments to pass to api
#'
#' @return A dataset taken from \link{}
#' @import httr
#' @export
#'
#' @see also \link{https://www.fema.gov/about/openfema/api}
# get_fema_data <- function(entity, base_url = "https://www.fema.gov/api/open", api_params = list()) {
#' pass 2 arguments as strings: entity and api_params
#' Example:  YourFEMAdata <- get_fema_data(entity,api_params)
get_fema_data <- function(entity, api_params, base_url = "https://www.fema.gov/api/open") {
  entity_info <- get_fema_entities(entity)
  api_params <- "filter=fyDeclared gt 1979"
  api_url <- paste0(base_url,"/", tolower(entity_info$OpenFEMAAPI), "/", entity_info$Entity_API_string)
  if(length(api_params) != 0) {
    api_params <- check_api_params(api_params)
    api_url <- paste0(api_url, api_params)
  }
  reslt <- httr::GET(api_url)
  # this check to see if the website wasn't found (could be simpler)
  if (httr::http_error(reslt)) {
    if(reslt$status == 404) {
      .err <- "Website not found"
    } else if (reslt$status == 400) {
      .err <- "Params not availible for entity"
    } else {
      .err <- paste0("Error please check the following link
                     in your browser: ", reslt$url)
    }
    stop(.err)
  }
  reslt <- httr::content(reslt, as ="text")
  reslt <- jsonlite::fromJSON(api_url)
  return(reslt[[2]])
}
#' Get FEMA Open Entity Names
#'
#' @param entity Character. Name of entity to scrape.
#' @param entity_url Character. "https://www.fema.gov/about/openfema/data-sets" (default),
#' Website containing FEMA entity names
#' @param verbose Logical. `FALSE`(default), should all available entities be returned?
#'
#' @return
#' @export
#'
#' @examples
get_fema_entities <- function(entity, entity_url = "https://www.fema.gov/about/openfema/data-sets", verbose = FALSE) {
  format_entity <- function(x) {
    x <- gsub("[^A-z]+", "", tools::toTitleCase(tolower(x)))
    return(x)
  }
# .format_entity function or entity_url is problem; "DisasterDeclarations" and lower Disasterdeclarationsummaries
  entity_table <- xml2::read_html(entity_url)
  entity_table <- rvest::html_table(entity_table)
  entity_table <- do.call(rbind, entity_table)
  entity_table[entity_table == "N/A"] <- NA
  entity_table <- entity_table[!is.na(entity_table[,2]), -3]
  names(entity_table) <- c("Entity", gsub(" ", "", names(entity_table)[-1]))
  entity_table$Entity_API_string <- format_entity(entity_table$Entity)

  if(verbose) {
    return(entity_table)
  } else {
 #   found <- match(format_entity(entity), entity_table$Entity_API_string)
    found <- match(entity, entity_table$Entity_API_string)
    # entity_table has Entity UC;  entity_table$Entity has spaces between words.
    if(is.na(found)) {
      stop(paste0(entity_table$Entity_API_string, " Entity not found. Here are availible entities:", "\n\n",
                  paste0(entity_table$Entity_API_string, collapse = "\n"))) #expose input value
    } else {
      return(entity_table[found,])
    }
  }
}

#' this check to make sure that your extra api commands are valid
#' set api_params to one of the api_cmds and it will append url string characters needed.
check_api_params <- function(params) {
  api_cmds <- c("callback", "filename", "filter", 
                "format","inlinecount", "metadata",
                "orderby", "select", "skip", "top")
  if (!all(names(params) %in% api_cmds)) {
     stop(paste0("Invalid api parameter. Availible parameters are: ", 
                paste0(api_cmds, collapse = ";")))
  }
  params <- lapply(params, function(x) {
   x <-  URLencode(as.character(x), reserved = TRUE)
  })
  params <- paste0("$", names(params), "=", params)
  params <- paste0("?", paste0(params, collapse = "&"))
  return(params)
}
# PLOTS   ------------------
# columns for plots: state (or fipsStateCode), fyDeclared, incidentType
# 1) line graph time series, lines represent incident type
# 2) stacked bar chart by state  or facet wrap by incident type
# create a new function map_fema_data which gets passed the result data object from get_fema_data
# PLOTS

# Dont really need this anymore
# fema_state_codes <- function() {
# c("AL","AK","AS","AZ","AR","CA","CO","CT","DE","DC","FL","GA","GU","HI",
#   "ID","IL","IN","IA","KS","KY","LA","ME","MD","MH","MA","MI","FM","MN",
#   "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","MP","OH","OK",
#   "OR","PW","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","VI","WA",
#   "WV","WI","WY")
# }
