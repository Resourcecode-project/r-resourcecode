#' Plot a Triangular Mesh with Smooth Coloring (Boundary Handling)
#'
#' Rasterizes a triangular mesh with smooth color interpolation, rotated 90° clockwise
#' for geospatial plotting (longitude on x-axis, latitude on y-axis, north-up).
#' Supports optional triangle edges, colorbar, z-scale transformations, NA handling,
#' and optional map boundary points via \code{mapsta}.
#'
#' @param points_df A data.frame with columns \code{lon}, \code{lat}, \code{z},
#'    and optionally \code{mapsta}.
#' @param tri_mat A 3 x n matrix of integers defining triangles (indices into points_df).
#' @param n Integer. Raster resolution (n x n). Ignored if \code{nx} and \code{ny} are provided.
#' @param nx Number of pixels in x-direction. Overrides \code{n} if provided.
#' @param ny Number of pixels in y-direction. Overrides \code{n} if provided.
#' @param draw_edges Logical. If TRUE, triangle edges are drawn.
#' @param palette Viridis palette: "viridis", "magma", "plasma", "cividis", etc.
#' @param add_colorbar Logical. If TRUE, adds a colorbar to the plot.
#' @param z_trans Function. Optional transformation of the z values for coloring
#'        (e.g., \code{sqrt}, \code{log}).
#' @param na_color Color for pixels outside any triangle. Can be "transparent" or any valid R color.
#' @param mapsta Optional numeric vector in \code{points_df} identifying boundary vertices
#'              (1 = boundary, 0 = interior).
#' @param ... Additional arguments passed to \code{image()}
#'       (e.g., \code{main}, \code{asp}, \code{xlab}, \code{ylab}, \code{zlim}).
#'
#' @return Invisibly returns a list with rasterized grid and triangle vertices.
#' @examples
#'# Create a simple grid of points
#'points_df <- expand.grid(lon = seq(0, 1, length.out = 5),
#'                         lat = seq(0, 1, length.out = 5))
#'points_df$z <- sqrt(points_df$lon^2+points_df$lat^2) *sin(2*pi * points_df$lon) * cos(2*pi * points_df$lat)
#'
#'# Define a simple triangular mesh manually (each square split into 2 triangles)
#'tri_mat <- matrix(nrow = 3, ncol = 32)  # 4x4 squares * 2 triangles per square
#'count <- 1
#'for(i in 1:4){
#'  for(j in 1:4){
#'    p1 <- (i-1)*5 + j
#'    p2 <- p1 + 1
#'    p3 <- p1 + 5
#'    p4 <- p3 + 1
#'    # Triangle 1
#'    tri_mat[, count] <- c(p1, p2, p4); count <- count + 1
#'    # Triangle 2
#'    tri_mat[, count] <- c(p1, p4, p3); count <- count + 1
#'  }
#'}
#'
#'# Define boundary points: all points on the edge of the grid
#'points_df$mapsta <- 0
#'points_df$mapsta[points_df$lon == 0 | points_df$lon == 1 |
#'                   points_df$lat == 0 | points_df$lat == 1] <- 1
#'
#'# Plot using plot_mesh
#'plot_mesh(points_df, tri_mat, n = 200,
#'          palette = "viridis",
#'          draw_edges = TRUE,
#'          add_colorbar = TRUE,
#'          na_color = "transparent",
#'          mapsta = points_df$mapsta,
#'          xlab = "Longitude", ylab = "Latitude", main = "Simple Map Example")
#' \dontrun{
#'  # Example on Resourcecode data
#'  plot_mesh(points_df = data.frame(lon = resourcecodedata::rscd_field$longitude,
#'            lat = resourcecodedata::rscd_field$latitude,
#'             z = resourcecodedata::rscd_field$depth),
#'             tri_mat = resourcecodedata::rscd_triangles,
#'             z_trans = log ,draw_edges = T, n=5000,
#'             asp = 1.5,
#'             zlim = c(-0.70,9),
#'             main = "Bathymetry map (sqrt scale)"
#'             )
#' }
#' @export
plot_mesh <- function(
  points_df,
  tri_mat,
  n = 500,
  nx = NULL,
  ny = NULL,
  draw_edges = FALSE,
  palette = "viridis",
  add_colorbar = TRUE,
  z_trans = NULL,
  na_color = "transparent",
  mapsta = NULL,
  ...
) {
  if (is.null(nx)) {
    nx <- n
  }
  if (is.null(ny)) {
    ny <- n
  }

  if (!all(c("lon", "lat", "z") %in% names(points_df))) {
    stop("points_df must have columns: lon, lat, z")
  }

  # Rasterize triangles
  res <- rasterize_triangles(
    tri_mat,
    points_df$lon,
    points_df$lat,
    points_df$z,
    nx = nx,
    ny = ny,
    draw_edges = draw_edges
  )

  # Apply optional z transformation
  zvals <- res$gridZ
  if (!is.null(z_trans) && is.function(z_trans)) {
    zvals <- z_trans(zvals)
  }

  # Fill boundary triangle edges if mapsta is provided
  if (!is.null(mapsta)) {
    if (length(mapsta) != nrow(points_df)) {
      stop(
        "mapsta must have the same length as the number of points in points_df"
      )
    }
    boundary_vertices <- which(mapsta != 0)
    boundary_triangles <- apply(tri_mat, 2, function(tri) {
      any(tri %in% boundary_vertices)
    })

    # For simplicity, fill NAs with triangle vertex mean
    if (any(boundary_triangles)) {
      mean_boundary <- mean(points_df$z[unlist(tri_mat[, boundary_triangles])])
      zvals[is.na(zvals)] <- mean_boundary
    }
  }

  # Rotate 90° clockwise
  z_rot <- t(apply(zvals, 2, rev)) # rotate 90° clockwise

  # Define colors
  cols <- viridisLite::viridis(100, option = palette)

  # Handle NA pixels
  if (!is.null(na_color) && na_color != "transparent") {
    z_rot_na <- is.na(z_rot)
    z_rot[z_rot_na] <- min(z_rot, na.rm = TRUE) - 1
  }

  # Plot with image.plot
  fields::imagePlot(
    x = seq(res$xmin, res$xmax, length.out = ncol(z_rot)),
    y = seq(res$ymin, res$ymax, length.out = nrow(z_rot)),
    z = z_rot,
    col = cols,
    useRaster = TRUE,
    ...
  )

  # Optional triangle edges
  if (draw_edges) {
    lines(res$triY, res$triX, col = "black", lwd = 0.5)
  }

  invisible(res)
}
