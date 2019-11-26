#' Authenticate User
#'
#' @param base_url Base URL for Move It server
#' @param payload Authentication header
#'
#' @return List of auth tokens for MoveIt access
#' @export
#'
#' @examples
#' tokens <- authMoveIt("someurl.com", "auth=grant_type=password&username=USERNAME&password=PASSWORD")
authMoveIt <- function(base_url, payload) {
  # Build URL
  url <- paste0("https://moveit.", base_url, ".us/api/v1/token")
  # Post Auth
  g <- POST(url, body = payload)
  # Get tokens
  tokens <- content(g)
  # Return auth token list
  return(tokens)
}

#' Function to download a file from the FTP server
#'
#' @param base_url Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#' @param id file id
#' @param file_type XSV or XLSX
#'
#' @return A dataframe of the identified file
#' @export
#'
#' @examples
#' df <- readMoveItFile("someurl.com", 626235843, tokens, "csv")
readMoveItFile <- function(base_url, tokens, id, file_type = "csv") {
  # Create Temp file for download
  tmp <- tempfile()
  token <- tokens$access_token
  # Build file URL
  url <- paste0("https://moveit.", base_url, "/api/v1/files/", id, "/download")
  # Request
  g <- httr::GET(url,
                 httr::add_headers(Authorization = paste("Bearer", token)),
                 httr::content_type("text/csv"),
                 write_disk(tmp))
  # Read tmp file by file type
  if (file_type == "csv") {
    data <- readr::read_csv(tmp)
  } else if (file_type == "excel") {
    data <- readxl::read_excel(tmp)
  }
  # Delete Temp File
  file.remove(tmp)
  # Return data to user
  return(data)
}

#' Available Files
#'
#' @param base_url Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#'
#' @return Dataframe of available files and their details
#' @export
#'
#' @examples
#' files <- availableFiles("someurl.com", tokens)
availableFiles <- function(base_url, tokens) {
  # Read access Token
  token <- paste("Bearer", tokens$access_token)
  # Build URL
  url <- paste0("https://moveit.", base_url, "/api/v1/files")
  # Send Get request
  g <- httr::GET(url,
                 httr::add_headers(Authorization = token))
  # Files list
  files <- jsonlite::fromJSON(content(g, "text"))

  # Files dataframe
  items <- files$items
  # Page Number & total pages for while loop
  page <- as.numeric(files$paging$page)
  totalPages <- as.numeric(files$paging$totalPages)

  # Iterate over additional pages
  while (page != totalPages) {
    # Next Page number
    nextPage <- page + 1
    # Build URL
    urlPage <- paste0(url, "?page=", nextPage)

    # Get page of files
    g <- httr::GET(urlPage,
                   httr::add_headers(Authorization = token))

    # Files list
    files <- jsonlite::fromJSON(content(g, "text"))

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
#' @param base_url Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#'
#' @return Dataframe of available folders and their details
#' @export
#'
#' @examples
#' folders <- availableFiles("someurl.com", tokens)
availableFolders <- function(base_url, tokens) {
  # Load Auth token
  token <- paste("Bearer", tokens$access_token)
  # Build URL
  url <- paste0("https://moveit.", base_url, "/api/v1/folders")

  # Get request for folders
  g <- httr::GET(url,
                 httr::add_headers(Authorization = token))

  # List of folders
  folders <- jsonlite::fromJSON(content(g, "text"), flatten = TRUE)

  # Folder details as dataframe
  items <- folders$items

  # Page Number & total pages for while loop
  page <- as.numeric(folders$paging$page)
  totalPages <- as.numeric(folders$paging$totalPages)

  # Iterate over additional pages
  while (page != totalPages) {
    # Next Page number
    nextPage <- page + 1
    # Build URL
    urlPage <- paste0(url, "?page=", nextPage)

    # Get page of files
    g <- httr::GET(urlPage,
                   httr::add_headers(Authorization = token))
    # Folders list
    folders <- jsonlite::fromJSON(content(g, "text"), flatten = TRUE)

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
#' @param base_url Base URL for Move It server
#' @param tokens List of auth tokens for MoveIt access
#' @param folderId ID for destination folder
#' @param filePath Location of file locally
#'
#' @return POST content
#' @export
#'
#' @examples
#' uploadMoveItFile("someurl.com", tokens, 2346247, "some.csv", "text/csv")
uploadMoveItFile <- function(base_url, tokens, folderId, filePath, filTtype) {
  # Load Auth token
  token <- paste("Bearer", tokens$access_token)

  # Build URL
  url <- paste0("https://moveit.", base_url, "/api/v1/folders/", folderId, "/files")

  # Send Request
  httr::POST(url = url,
             httr::add_headers(Authorization = token,
                               accept = "application/json",
                               `Content-Type` = "multipart/form-data"),
             body = list(file =  httr::upload_file(filePath, fileType))
             )
}
