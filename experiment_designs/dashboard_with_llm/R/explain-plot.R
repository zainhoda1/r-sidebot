library(ggplot2)

#' Convert a plot object to a PNG data URI
#'
#' @param p The plot object; currently, plotly and ggplot2 are supported. Note
#'     that plotly requires Python, {reticulate}, and the PyPI packages {plotly}
#'     and {kaleido}.
plot_to_img_content <- function(p) {
  UseMethod("plot_to_img_content", p)
}

# We write the PNG at the size we want the model to see and then pass
# resize = "none". Any other `resize` value sends ellmer through {magick}, which
# is only a Suggests of ellmer and so never lands in the deployment bundle —
# rsconnect builds its manifest from the packages this app actually calls. On
# shinyapps.io that turned into "magick needed to resize images", the image was
# dropped, and the assistant answered from the prompt text alone. Resizing was a
# no-op anyway: ellmer's "high" only shrinks images past 2000x768, and these are
# 800x600, comfortably inside Anthropic's 1568px recommendation.
plot_to_img_content.plotly <- function(p) {
  # Create a temporary file
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  # Save the plot as an image
  save_image(p, tmp, width = 800, height = 600)
  ellmer::content_image_file(tmp, resize = "none")
}

plot_to_img_content.ggplot <- function(p) {
  # Create a temporary file
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  ggsave(tmp, p, width = 800, height = 600, units = "px", dpi = 100)
  ellmer::content_image_file(tmp, resize = "none")
}
