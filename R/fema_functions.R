#' Get FEMA Open API Data
#'
#' @param entity Character. Name of entity to scrape. See function \code{\link{get_fema_entites}} 
#'  documentation for details.
#' @param api_params Named list. Extra arguments to pass to api. See \url{https://www.fema.gov/about/openfema/api}.
#' Parameters are validated using the \code{\link{check_api_params}} function.
#' @param base_url Character. "https://www.fema.gov/api/open" (default),  website Url of FEMAs API.
#'
#' @return A dataset taken from \url{https://www.fema.gov/api/open}
#'
#' @seealso \code{\link{get_fema_entites}} \code{\link{get_fema_data_all}} 
#' @examples 
#' 
#' ## Get first 1000 records from FEMA Web Disasters
#' get_fema_data("FEMA Web Disasters", api_params = list(skip = 1000))
#' 
#' ## Get first 1000 records from FEMA Web Disasters after skipping first 1000 records
#' get_fema_data("FEMA Web Disasters", api_params = list(skip = 1000))
#' 
#' ## Get first 1000 records ordered by state from Disaster Declarations Summaries after skipping first 1000 records
#' get_fema_data("Disaster Declarations Summaries", 
#' api_params = list(orderby = state, skip = 1000))
#' 
#' @import httr
#' @export
get_fema_data <- function(entity, api_params = list(), base_url = "https://www.fema.gov/api/open") {
  entity_info <- get_fema_entities(entity)
  api_url <- paste0(base_url,"/", tolower(entity_info$OpenFEMAAPI), "/", entity_info$Entity_API_string)
  if(length(api_params) != 0) {
    api_params <- check_api_params(api_params)
    api_url <- paste0(api_url, api_params)
  }
  .res <- query_fema(api_url)
  return(.res)
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
  .format_entity <- function(x) {
    if (all(grepl("^[a-z]+$|^[A-Z]+$", x))) {
      stop("enitity must contain spaces or a variety of lower and uppercase letters")
    } else {
      x <- strsplit(x, split = "(\\s+)|(?<=.)(?=[A-Z][a-z])", perl = T)
      x <- sapply(x, paste0, collapse = " ")
      x <- gsub("[^A-z]+", "", tools::toTitleCase(tolower(x)))
    }
    return(x)
  }
  entity_table <- xml2::read_html(entity_url)
  entity_table <- rvest::html_table(entity_table)
  entity_table <- do.call(rbind, entity_table)
  entity_table[entity_table == "N/A"] <- NA
  entity_table <- entity_table[!is.na(entity_table[,2]), -3]
  names(entity_table) <- c("Entity", gsub(" ", "", names(entity_table)[-1]))
  entity_table$Entity_API_string <- .format_entity(entity_table$Entity)
  if(verbose) {
    return(entity_table)
  } else {
    found <- match(.format_entity(entity), entity_table$Entity_API_string)
    if(is.na(found)) {
      stop(paste0("Entity not found. Here are availible entities:", "\n\n",
                  paste0(entity_table$Entity_API_string, collapse = "\n")))
    } else {
      return(entity_table[found,])
    }
  }
}
#' Validate API parameters
#'
#' @param .api_params Named list taken from \code{api_params} argument in
#' \code{\link{get_fema_data}} \code{\link{get_fema_data_all}}
#' @export 
check_api_params <- function(.api_params) {
  if (!inherits(.api_params, "list")) {
    stop("api parameters must be a named list")
  }
  api_cmds <- c("callback", "filename", "filter", 
                "format","inlinecount", "metadata",
                "orderby", "select", "skip", "top")
  
  if (!all(names(.api_params) %in% api_cmds)) {
     stop(paste0("Invalid api parameter. Availible parameters are: ", 
                paste0(api_cmds, collapse = ";")))
  }
  if ("filter" %in% names(.api_params)) {
    .filter_cmd <- c("contains", "geo\\.intersects", "startswith", "endswith", "substringof",
                     "eq", "ne", "gt", "ge", "lt", "le", "and", "or", "not")
    .filter_test <- !grepl(paste0(sprintf("\\b%s\\b", .filter_cmd), collapse = "|"), .api_params[["filter"]])
    if (.filter_test || length(.filter_test) != 1) {
      stop("api filter syntax incorrect or not a single string, 
           please see api documentation for details")
    }
  }
  .api_params <- lapply(.api_params, function(x) {
   x <-  URLencode(as.character(x), reserved = TRUE)
  })
  .api_params <- paste0("$", names(.api_params), "=", .api_params)
  .api_params <- paste0("?", paste0(.api_params, collapse = "&"))
  return(.api_params)
}
#' Get FEMA Open API All Data
#'
#' @inheritParams get_fema_data
#' @param max_limit To load and stop at a certain number. 
#' @param wait Number of seconds to process the data.
#'
#' @return
#' @export
#'
#' @examples
get_fema_data_all <- function(entity, api_params = list(),
                              max_limit = 5000, wait = 1, base_url = "https://www.fema.gov/api/open") {
  api_params[["inlinecount"]] <- "allpages"
  first_page <- get_fema_data(entity, base_url, api_params)
  all_data_url <- paste0(gsub("\\/api\\/open", "", base_url), first_page$metadata$url)
  if (first_page$metadata$count < 1000L) {
    stop("Not enough records to download try get_fema_data")
  }
  skips <- seq(0, first_page$metadata$count, 1000)[seq(0, first_page$metadata$count, 1000) <= max_limit]
  api_urls <- paste0(all_data_url,"&$skip=", skips)[-1]
  res <- lapply(api_urls, query_fema, .wait = wait)
  res <- c(first_page, res)
  return(res)
}

#' Title
#'
#' @param api_url Character. Website URL or FEMA's API
#' @param .wait Number of seconds to process the data.
#'
#' @return
#' @export
#'
#' @examples
query_fema <- function(api_url, .wait = 1) {
  Sys.sleep(.wait)
  .res <- httr::GET(api_url)
# this check to see if the website wasn't found (could be simpler)
if (httr::http_error(.res)) {
  if(.res$status == 404) {
    .err <- "Website not found"
  } else if (.res$status == 400) {
    .err <- "Params not availible for entity"
  } else {
    .err <- paste0("Error please check the following link
                     in your browser: ", .res$url)
  }
  stop(.err)
}
  .res <- httr::content(.res, as ="text")
  .res <- jsonlite::fromJSON(api_url)
return(.res)
}
