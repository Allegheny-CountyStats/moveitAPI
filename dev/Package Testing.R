require(moveitAPI)
<<<<<<< Updated upstream
requrie(dplyr)
=======
require(dplyr)
>>>>>>> Stashed changes

payload <- paste0("grant_type=password&username=", Sys.getenv('MI_USER'), "&password=", Sys.getenv('MI_PASS'))
moveit_url <- "alleghenycounty.us"

<<<<<<< Updated upstream
filename <- "Voter_Registration.TXT"

tokens <- authMoveIt(baseUrl, payload)

file <- availableFiles(moveit_url, tokens) %>%
  filter(name == filename) %>%
  pull(id)

createPackage <- function(baseUrl,
                          token,
                          note,
                          subject,
                          recipients,
                          files,
                          expires = Sys.Date() + 7,
                          restrict_ips = NULL) {

  url <- paste0("https://moveit.", baseUrl, "/api/v1/packages")

  for (recipient in recipients) {
    body_list <- list(
      bd = note,
      subject = subject,
      recipients = recipient,
      files = files,
      expires = expires,
      type = "General"
    )

    if (!is.null(restrict_ips)) {
      body_list$restrictIPs <- restrict_ips
    }

    return <- httr::POST(
      url,
      add_headers(
        Authorization = paste("Bearer", tokens$access_token),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
    )

    content(return)

    if (!return$status_code %in% c(201, 200)) {
      stop(return$status_code)
    }
=======
filePath <- "ranger_contacts.csv"

getUsers <- function(baseUrl,
                     tokens) {
  url <- paste0("https://moveit.", baseUrl, "/api/v1/users")

  users <- GET(url,
               add_headers(
                 Authorization = paste("Bearer", tokens$access_token),
                 "Content-Type" = "application/json")
  ) %>%
    content()
}

packageReqs <- function(baseUrl, tokens) {
  url <- paste0("https://moveit.", baseUrl, "/api/v1/packages")

  packs <- GET(url,
              add_headers(
                Authorization = paste("Bearer", tokens$access_token),
                "Content-Type" = "application/json")
  ) %>%
    content()
}

packageReqs <- function(baseUrl, tokens) {
  url <- paste0("https://moveit.", baseUrl, "/api/v1/packages/requirements")

  reqs <- GET(url,
               add_headers(
                 Authorization = paste("Bearer", tokens$access_token),
                 "Content-Type" = "application/json")
  ) %>%
    content()
}

users <- getUsers(moveit_url, tokens)

tokens <- authMoveIt(baseUrl, payload)

uploadMoveItAttachment <- function(baseUrl, tokens, filePath, chunked=FALSE) {
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
  url <- paste0("https://moveit.", baseUrl, "/api/v1/packages/attachments")

  size <- file.size(filePath)
  fileType <- tools::file_ext(filename)

  if (size >= 40000000 | chunked) {
    # Send Request
    return <- httr::POST(url = url,
                         httr::add_headers(Authorization = token,
                                           accept = "application/json",
                                           `Content-Type` = "multipart/form-data",
                                           `Transfer-Encoding` = "chunked"),
                         body = list(path = "/PARKS",
                                     file =  httr::upload_file(filePath, fileType))
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
  return(content(return)$id)
}

file <- uploadMoveItAttachment(moveit_url, tokens, filePath)

recipients <- list(list(identifer = "geoffrey.lloyd.arnold@gmail.com"))

note <- "A note"
subject <- "Test package send"
expires <- Sys.Date() + 7

createPackage <- function(baseUrl,
                          tokens,
                          note,
                          subject,
                          recipients,
                          file,
                          expires = Sys.Date() + 7,
                          restrict_ips = NULL) {

  url <- paste0("https://moveit.", baseUrl, "/api/v1/packages/")

  body_list <- list(
    body = note,
    subject = subject,
    recipients = recipients,
    attachments = list(list(id = as.character(file))),
    expires = expires,
    deliveryReceipts = T,
    packageClassificationTypeId = 1,
    NoReply = "AllowAll",
    composerType = "General",
    type = "General"
  )

  if (!is.null(restrict_ips)) {
    body_list$restrictIPs <- restrict_ips
  }

  return <- httr::POST(
    url,
    add_headers(
      Authorization = paste("Bearer", tokens$access_token),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE, pretty = TRUE)
  )

  content(return)

  if (!return$status_code %in% c(201, 200)) {
    stop(return$status_code)
>>>>>>> Stashed changes
  }
}

createPackage(
  moveit_url,
  auth_token = tokens,
  note = "Here are the requested files.",
  subject = "Requested Files",
<<<<<<< Updated upstream
  recipients = c(To = "geoffrey.arnold@allegehnycounty.us", To = "daniel.andrus@alleghenycounty.us"),
=======
  recipients = list(identifier = "geoffrey.arnold@allegehnycounty.us"),
>>>>>>> Stashed changes
  files = file
)
