
NNcol <- list()

NNcol$company = c(
  white         = "#FFFFFF", #                      (CMYK:   0, 0,  0,  0)
  dark_blue     = "#001965", # Pantone 280 C        (CMYK: 100,75,  0, 35)
  light_blue    = "#009FDA", # Pantone Process Cyan (CMYK: 100, 5,  5,  0)
  lava_red      = "#E64A0E", # Pantone 173 C        (CMYK:   5,90,100,  5)
  granite_grey  = "#82786F", # Pantone Warm Grey 9C (CMYK:   0,14, 19, 60)
  concrete_grey = "#AEA79F", # Pantone Warm Grey 5C (CMYK:  11,13, 14, 32)
  marble_grey   = "#C7C2BA", # Pantone Warm Grey 3C (CMYK:   9,12, 12, 20)
  pearl_grey    = "#E0DED8", # Pantone Warm Grey 1C (CMYK:   3, 3,  6,  7)
  black         = "#001423",         #                      (CMYK:  60, 0,  0,100)
  forest_green  = "#3F9C35", # Pantone 362 C        (CMYK:  80, 0,100,  2)
  grass_green   = "#739600", # Pantone 377 C        (CMYK:  56, 1,100, 22)
  lime_green    = "#C9DD03", # Pantone 381 C        (CMYK:  29, 0, 96,  0)
  ocean_blue    = "#007C92", # Pantone 3145 C       (CMYK: 100,11, 28, 20)
  sky_blue      = "#72B5CC", # Pantone 7458 C       (CMYK:  53, 3,  7,  9)
  misty_blue    = "#C2DEEA", # Pantone 290 C        (CMYK:  23, 0,  1,  0)
  sunset_orange = "#D47600", # Pantone 1385 C       (CMYK:   2,60,100,  3)
  golden_yellow = "#EAAB00", # Pantone 124 C        (CMYK:   0,32,100,  0)
  granite_gray  = "#82786F", # Pantone Warm Grey 9C (CMYK:   0,14, 19, 60)
  concrete_gray = "#AEA79F", # Pantone Warm Grey 5C (CMYK:  11,13, 14, 32)
  marble_gray   = "#C7C2BA", # Pantone Warm Grey 3C (CMYK:   9,12, 12, 20)
  pearl_gray    = "#E0DED8") # Pantone Warm Grey 1C (CMYK:   3, 3,  6,  7)


#' Vector containing company colours
#'
#' A vector containing company colours
#'
#' @details This vector is used by the shiny application
#' \code{\link{runSampleSizereview}} and the function \code{\link{create_avatar}}.
#'
#' @examples
#' # show the colour
#' nncol$company
#' @export
nncol <- NNcol
