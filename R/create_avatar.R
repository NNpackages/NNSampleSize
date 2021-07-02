#' Creates the avatar for the person who logged in.
#'
#' @param user The user name the avatar is based upon
#' @param overwrite Should a new avatar be created if it allready exists
#' @param col The background color.
#' @param type should the avatars be squares or circles
#' @param save_png should a png be save
#' @param file If saved the path to the file name.
#' @param text.size Size of text used for
#' @param height The height of the image if saved as png
#' @param edge color of the edge in \code{type} equal to \code{circle}
#' @param edge.size size of the edge for \code{type} equal to \code{circle}
#'
#' @return Saves a png file that can be used in the comment section
#' @export
#'
#' @examples
#' create_avatar("sffl")
#' online <- c("cdn", "sffl")
#'
#' users <- c("cdn", "agtr", "abiu", "sffl", "kfsm")
#'
#' online_col <- (2:3)[users %in% online + 1]
#' create_avatar(c(users), edge = online_col, text.size = 128 / 12)
#' create_avatar(c(users), edge = online_col, text.size = 128 / 12, edge.size = 0.8)
#'
#' @importFrom ggplot2 ggplot aes_string geom_rect geom_rect geom_text theme margin element_rect element_blank element_text coord_fixed
#'              scale_linetype_manual scale_fill_manual scale_color_manual
#' @importFrom grid unit
# #' @importFrom Cairo CairoPNG
#' @importFrom ggforce geom_circle
#' @importFrom grDevices dev.off png
create_avatar <- function(user,
                          col = sample(nncol$company[-1], length(user), replace = length(user) >= length(nncol$company)),
                          edge = col,
                          edge.size = 0.3,
                          height = 128,
                          text.size = height / 9,
                          type = c("circle", "square"),
                          save_png = FALSE, overwrite = FALSE,
                          file = paste0(paste(tolower(user), collapse = "_"), ".png")
                          ) {

  type <- match.arg(type)
  user_up <- toupper(user)
  r = 0.4

  user_data <- data.frame(user = user_up,
                        xmin = seq_along(user) - 1 - r,
                        xmax = seq_along(user) - 1 + r,
                        ymax = r,
                        ymin = -r,
                        x0 = seq_along(user) - 1,
                        y0 = rep(0, length(user_up)),
                        r = r)

  # initiate the plot
  p <- ggplot(data = user_data)

  if (type == "square")
    p <- p + geom_rect(aes_string(xmin = "xmin", ymin = "ymin", xmax = "xmax", ymax = "ymax", fill = "user", color = "user"))

  if (type == "circle")
    p <- p + ggforce::geom_circle(aes_string(x0 = "x0", y0 = "y0", r = "r", fill = "user", color = "user"), size = edge.size)

  # add text
  p <- p + geom_text(label = user_up, aes_string(x = "x0", y = "y0"), size = text.size, col = "white") +
    coord_fixed(1) + scale_fill_manual(values = structure(col, names = user_up)) +
    scale_color_manual(values = structure(edge, names = user_up))

  # Add the empty theme
  p <- p + theme(
    legend.position = "none",
    legend.margin = margin(t = 0, unit = 'cm'),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.margin = grid::unit(c(0,0,0,0), "mm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "transparent",colour = NA), #001f3f navy
    strip.text.x = element_text(colour = "white"),
    strip.text.y = element_text(colour = "white")
  )


  if (save_png)
    if (overwrite | !file.exists(file)) {
      grDevices::png(file,
               width = height * (r*2 + length(user) - 1) / (r*2), height = height,
               bg = ifelse(length(user) == 1 & type == "square", unname(col), "transparent"))
      print(p)
      grDevices::dev.off()
    }

  return(p)
}
