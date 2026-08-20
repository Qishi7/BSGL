# Create the BSGL hex logo.
#
# Run from the repository root:
#   Rscript scripts/create_hex_logo.R
#
# Required only for regenerating the logo:
#   install.packages(c("hexSticker", "ggplot2"))

required <- c("hexSticker", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Install the following packages to regenerate the logo: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

repo_root <- normalizePath(getwd(), mustWork = TRUE)
out_dir <- file.path(repo_root, "man", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
rplots_file <- file.path(repo_root, "Rplots.pdf")
had_rplots_file <- file.exists(rplots_file)

library(ggplot2)

set.seed(20260820)

grid <- expand.grid(
  x = seq(-1.15, 1.15, length.out = 180),
  y = seq(-1.15, 1.15, length.out = 180)
)

grid$r <- sqrt(grid$x^2 + grid$y^2)
grid$z <- with(
  grid,
  0.85 * exp(-3.2 * ((x + 0.35)^2 + (y - 0.30)^2)) -
    0.65 * exp(-4.8 * ((x - 0.48)^2 + (y + 0.38)^2)) +
    0.34 * sin(3.1 * x + 1.2 * y) +
    0.18 * cos(4.4 * y - 0.7 * x)
)
grid$alpha <- ifelse(grid$r <= 1.08, 1, 0)

site_points <- data.frame(
  x = c(-0.78, -0.50, -0.18, 0.18, 0.50, 0.75, -0.58, 0.02, 0.58),
  y = c(0.48, -0.12, 0.72, -0.60, 0.16, -0.30, -0.60, 0.06, 0.62)
)

surface_plot <- ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = z, alpha = alpha), interpolate = TRUE) +
  geom_contour(aes(z = z), bins = 8, color = "white", linewidth = 0.22, alpha = 0.68) +
  geom_point(
    data = site_points,
    aes(x, y),
    inherit.aes = FALSE,
    size = 1.25,
    color = "#062A3A",
    fill = "#F8FAFC",
    shape = 21,
    stroke = 0.25,
    alpha = 0.92
  ) +
  annotate(
    "text",
    x = 0.02,
    y = -0.02,
    label = "beta(s)",
    parse = TRUE,
    family = "sans",
    fontface = "bold",
    color = "#F8FAFC",
    size = 5.3,
    alpha = 0.95
  ) +
  scale_fill_gradientn(
    colours = c("#073B5A", "#0E6F88", "#24A88E", "#CDEB73", "#F8D45C")
  ) +
  scale_alpha_identity() +
  coord_equal(xlim = c(-1.12, 1.12), ylim = c(-1.12, 1.12), expand = FALSE) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

make_logo <- function(filename, dpi) {
  hexSticker::sticker(
    subplot = surface_plot,
    package = "BSGL",
    filename = filename,
    s_x = 1.00,
    s_y = 0.89,
    s_width = 0.76,
    s_height = 0.68,
    p_x = 1.00,
    p_y = 1.50,
    p_size = 10.5,
    p_color = "#F8FAFC",
    p_family = "sans",
    p_fontface = "bold",
    h_fill = "#0B2533",
    h_color = "#2CC6A3",
    h_size = 1.35,
    url = "Bayesian SGL",
    u_x = 1.00,
    u_y = 0.39,
    u_angle = 0,
    u_size = 2.25,
    u_color = "#D5F3EA",
    u_family = "sans",
    white_around_sticker = FALSE,
    dpi = dpi
  )
}

logo_file <- file.path(out_dir, "logo.png")
logo_file_2x <- file.path(out_dir, "logo@2x.png")

make_logo(logo_file, dpi = 320)
make_logo(logo_file_2x, dpi = 640)

if (!had_rplots_file && file.exists(rplots_file)) {
  unlink(rplots_file)
}

message("Created: ", logo_file)
message("Created: ", logo_file_2x)
