require(moveitAPI)
requrie(dplyr)

payload <- paste0("grant_type=password&username=", Sys.getenv('MI_USER'), "&password=", Sys.getenv('MI_PASS'))
moveit_url <- "alleghenycounty.us"

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
  }
}

createPackage(
  moveit_url,
  auth_token = tokens,
  note = "Here are the requested files.",
  subject = "Requested Files",
  recipients = c(To = "geoffrey.arnold@allegehnycounty.us", To = "daniel.andrus@alleghenycounty.us"),
  files = file
)
