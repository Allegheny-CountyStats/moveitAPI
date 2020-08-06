#' Authenticate User
#'
#' @param baseUrl Base URL for Move It server
#' @param payload Authentication header
#'
#' @return List of auth tokens for MoveIt access
#' @export
#'
#' @examples
#' \dontrun{
#' tokens <- authMoveIt("someurl.com", "grant_type=password&username=USERNAME&password=PASSWORD")
#' }
authMoveIt <- function(baseUrl, payload) {
  # Check function dependancies
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Build URL
  url <- paste0("https://moveit.", baseUrl, "/api/v1/token")
  # Post Auth
  g <- httr::POST(url, body = payload, httr::add_headers('Content-Type' = 'application/json'))
  # Get tokens
  tokens <- httr::content(g)
  # Return auth token list
  return(tokens)
}

#' Function to download a file from the FTP server
#'
#' @param baseUrl Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#' @param id file id
#' @param fileType csv or xlsx
#'
#' @return A dataframe of the identified file
#' @export
#'
#' @examples
#' \dontrun{
#' df <- readMoveItFile("someurl.com", 626235843, tokens, "csv")
#' }
readMoveItFile <- function(baseUrl, tokens, id, fileType = "csv") {
  # Check function dependancies
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (fileType == "csv") {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("Package \"readr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  } else if (fileType == "excel") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package \"readxl\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  # Create Temp file for download
  tmp <- tempfile()
  token <- tokens$access_token
  # Build file URL
  url <- paste0("https://moveit.", baseUrl, "/api/v1/files/", id, "/download")
  # Request
  if (fileType == "excel") {
    cType <- "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  } else {
    cType  <- "text/csv"
  }
  g <- httr::GET(url,
                 httr::add_headers(Authorization = paste("Bearer", token)),
                 httr::content_type(cType),
                 httr::write_disk(tmp))

  # Read tmp file by file type
  if (fileType == "csv") {
    data <- readr::read_csv(tmp, guess_max = 5000)
  } else if (fileType == "excel") {
    data <- readxl::read_excel(tmp)
  }
  # Delete Temp File
  file.remove(tmp)
  # Return data to user
  return(data)
}

#' Available Files
#'
#' @param baseUrl Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#'
#' @return Dataframe of available files and their details
#' @export
#'
#' @examples
#' \dontrun{
#' files <- availableFiles("someurl.com", tokens)
#' }
availableFiles <- function(baseUrl, tokens) {
  # Check function dependancies
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Read access Token
  token <- paste("Bearer", tokens$access_token)
  # Build URL
  url <- paste0("https://moveit.", baseUrl, "/api/v1/files")
  # Send Get request
  g <- httr::GET(url,
                 httr::add_headers(Authorization = token))
  # Files list
  files <- jsonlite::fromJSON(httr::content(g, "text"))

  # Files dataframe
  items <- files$items
  # Page Number & total pages for while loop
  page <- as.numeric(files$paging$page)
  totalPages <- as.numeric(files$paging$totalPages)

  # Iterate over additional pages
  while (page != totalPages) {
    # Dependency Check
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package \"dplyr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    # Next Page number
    nextPage <- page + 1
    # Build URL
    urlPage <- paste0(url, "?page=", nextPage)

    # Get page of files
    g <- httr::GET(urlPage,
                   httr::add_headers(Authorization = token))

    # Files list
    files <- jsonlite::fromJSON(httr::content(g, "text"))

    # Bind to previous pages
    items <- dplyr::bind_rows(items, files$items)

    # Get current page
    page <- as.numeric(files$paging$page)
    # Check totalPages
    totalPages <- as.numeric(files$paging$totalPages)
  }
  return(items)
}

#' List Available Folder
#'
#' @param baseUrl Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#'
#' @return Dataframe of available folders and their details
#' @export
#'
#' @examples
#' \dontrun{
#' folders <- availableFolders("someurl.com", tokens)
#' }
availableFolders <- function(baseUrl, tokens) {
  # Check dependancies
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Load Auth token
  token <- paste("Bearer", tokens$access_token)
  # Build URL
  url <- paste0("https://moveit.", baseUrl, "/api/v1/folders")

  # Get request for folders
  g <- httr::GET(url,
                 httr::add_headers(Authorization = token))

  # List of folders
  folders <- jsonlite::fromJSON(httr::content(g, "text"), flatten = TRUE)

  # Folder details as dataframe
  items <- folders$items

  # Page Number & total pages for while loop
  page <- as.numeric(folders$paging$page)
  totalPages <- as.numeric(folders$paging$totalPages)

  # Iterate over additional pages
  while (page != totalPages) {
    # Dependency Check
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package \"dplyr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    # Next Page number
    nextPage <- page + 1
    # Build URL
    urlPage <- paste0(url, "?page=", nextPage)

    # Get page of files
    g <- httr::GET(urlPage,
                   httr::add_headers(Authorization = token))
    # Folders list
    folders <- jsonlite::fromJSON(httr::content(g, "text"), flatten = TRUE)

    # Bind to previous pages
    items <- dplyr::bind_rows(items, folders$items)
    # Get current page and total pages
    page <- folders$paging$page
    totalPages <- folders$paging$totalPages
  }
  return(items)
}

#' Upload file to move it server
#'
#' @param baseUrl Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#' @param folderId ID for destination folder
#' @param filePath Path to file for upload
#' @param fileType POST file type ie "text/csv"
#' @param chunked TRUE or FALSE value, TRUE forces chunked upload. Function will automatically chunk if file is too big
#'
#' @return POST content
#' @export
#'
#' @examples
#' \dontrun{
#' uploadMoveItFile("someurl.com", tokens, 2346247, "some.csv", "text/csv")
#' }
uploadMoveItFile <- function(baseUrl, tokens, folderId, filePath, fileType, chunked=FALSE) {
  # Check dependency
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Fix Chunked on Linux
  if(missing(chunked)) {
    chunked <- FALSE
  }

  # Load Auth token
  token <- paste("Bearer", tokens$access_token)

  # Build URL
  url <- paste0("https://moveit.", baseUrl, "/api/v1/folders/", folderId, "/files")

  size <- file.size(filePath)

  if (size >= 40000000 | chunked) {
    # Send Request
    return <- httr::POST(url = url,
               httr::add_headers(Authorization = token,
                                 accept = "application/json",
                                 `Content-Type` = "multipart/form-data",
                                 `Transfer-Encoding` = "chunked"),
               body = list(file =  httr::upload_file(filePath, fileType))
               )
  } else {
    return <- httr::POST(url = url,
                         httr::add_headers(Authorization = token,
                                           accept = "application/json",
                                           `Content-Type` = "multipart/form-data"),
                         body = list(file =  httr::upload_file(filePath, fileType))
    )
  }
  if (!return$status_code %in% c(201, 200)) {
    stop(return$status_code)
  }
}
