#' Create the header for EOT style document
#'
#' @param x The document to change
#' @param title Character of length 4 giving the title for the header
#' @param status character defining the status of the document
#' @param version character defining the version, e.g. "0.1"
#' @param date The date to include in the header
#'
#' @return The document with the header altered as specified
#' @importFrom officer headers_replace_all_text
#' @export
header <- function(x,
                   title = c("Title 1", "Title 2", "Title 3", "Title 4"),
                   status = c("Draft", "Review", "Final"), version = "0.1",
                   date = format(Sys.Date(), "%d %b %Y")) {
  status <- match.arg(status)
  names(title) <- paste0("#header", seq_along(title))
  for (i in seq_along(title)) {
    x <- x %>% headers_replace_all_text(names(title)[i], title[i], warn = FALSE)
  }

  x <- x %>% headers_replace_all_text("#version", version, warn = FALSE)

  x <- x %>% headers_replace_all_text("#status", status, warn = FALSE)

  x <- x %>% headers_replace_all_text("#date", date, warn = FALSE)

  return(x)
}
