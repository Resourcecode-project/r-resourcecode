#' Value Matching
#'
#' @param x value to search
#' @param table table of values
#'
#' @return the opposite of x %in% table
#' @export
#'
#' @examples
#' 1:10 %in% c(1, 3, 5, 9)
#' 1:10 %nin% c(1, 3, 5, 9)
"%nin%" <- function(x, table) {
  !(x %in% table)
}


#' Find the closest point of the FIELD grid to the specified position
#'
#' @param x vector of coordinates in the form longitude/latitude data frame
#' @param lat alternatively, x and lat can be vector of the same length
#' @param closest an integer to specify the number of point to output.
#' @param ... currently unused
#'
#' @return a list with two components: the closest point(s) of the grid and the distance (s).
#' @export
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `resourcecode` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("resourcecodedata", quietly = TRUE)) {
#'   semrev_west <- closest_point_field(c(-2.786, 47.239))
#'   semrev_west
#' }
closest_point_field <- function(x, lat = NULL, closest = 1L, ...) {
  has_data()

  if (!is.null(lat)) {
    stopifnot(length(x) == length(lat))
    return(closest_point_field(cbind(x, lat)))
  }

  stopifnot(is.integer(closest) & closest >= 1L)
  dist <- geosphere::distGeo(x, resourcecodedata::rscd_field[, c(2, 3)])
  ind_min <- order(dist)[1:closest]

  return(list(
    point = ind_min,
    distance = dist[ind_min]
  ))
}

#' Find the closest point of the SPEC grid to the specified position
#'
#' @param x vector of coordinates in the form longitude/latitude data frame
#' @param lat alternatively, x and lat can be vector of the same length
#' @param closest an integer to specify the number of point to output.
#' @param ... currently unused
#'
#' @return a list with two components: the closest point(s) of the grid and the distance (s).
#' @export
#'
#' @examplesIf requireNamespace("resourcecodedata", quietly = TRUE)
#' semrev_west <- closest_point_spec(c(-2.786, 47.239))
#' semrev_west
closest_point_spec <- function(x, lat = NULL, closest = 1L, ...) {
  has_data()

  if (!is.null(lat)) {
    stopifnot(length(x) == length(lat))
    return(closest_point_spec(cbind(x, lat)))
  }

  stopifnot(is.integer(closest) & closest >= 1L)
  dist <- geosphere::distGeo(x, resourcecodedata::rscd_spectral[, c(1, 2)])
  ind_min <- order(dist)[1:closest]

  return(list(
    point = ind_min,
    distance = dist[ind_min]
  ))
}

#' Vector conversion
#'
#' Converts wind or current zonal and meridional velocity components to
#' magnitude and direction according to meteorological convention.
#'
#' @param u zonal velocity (1D vector) or matrix with zonal and meridional velocity (Nx2 matrix)
#' @param v meridional velocity (1D vector)
#' @param names names to construct the resulting data.frame
#'
#' @return a Nx2 data.frame with the norm and direction (meteorological convention)
#' @export
#'
#' @examples
#' u <- matrix(rnorm(200), nrow = 100, ncol = 2)
#' vdir <- zmcomp2metconv(u)
zmcomp2metconv <- function(u, v = NULL, names = c("wspd", "wdir")) {
  if (is.vector(u)) {
    stopifnot(length(v) == length(u))
    u <- cbind(u, v)
  }

  stopifnot(is.matrix(u) & dim(u)[2] == 2)

  speed <- sqrt(u[, 1]^2 + u[, 2]^2)
  direction <- (270 - atan2(u[, 2], u[, 1]) * 180 / pi) %% 360
  out <- data.frame(speed, direction)
  names(out) <- names
  return(out)
}

#' JONWSAP spectrum
#'
#' Creates a JONWSAP density spectrum (one-sided), defined by its integral parameters.
#'
#' Reference :
#'   - O.G.Houmb and T.Overvik, "Parametrization of Wave Spectra and Long Term
#'        Joint Distribution of Wave Height and Period,"
#'        in Proceedings, First International Conference
#'        on Behaviour of Offshore Structures (BOSS), Trondheim 1976.
#'        23rd International Towing Tank Conference, vol. II, pp. 544-551
#'   - ITTC Committee, 2002, "The Specialist Committee on Waves -
#'        Final Report and Recommendations to the 23rd ITTC",
#'        Proc. ITTC, vol. II, pp. 505-736.
#'
#' @param hs Hs (default: 5m)
#' @param tp Period (default: 10s)
#' @param fmax higher frequency of the spectrum or
#'             vector of frequencies (default to resourcecode frequency vector)
#' @param df frequency step (unused if fmax=vector of frequencies)
#' @param gam peak enhancement factor (default: 3.3)
#'
#' @return Density spectrum with corresponding parameters
#' @export
#'
#' @examples
#' S1 <- jonswap(tp = 15)
#' S2 <- jonswap(tp = 15, fmax = 0.95, df = 0.003)
#' plot(S1, type = "l", ylim = c(0, 72))
#' lines(S2, col = "red")
#' abline(v = 1 / 15)
jonswap <- function(hs = 5, tp = 15, fmax = rscd_freq, df = NULL, gam = 3.3) {
  if (length(fmax) > 1) {
    # Case when the frequency vector if given
    freq <- fmax
    fmin <- min(freq)
    fmax <- max(freq)
    df <- min(diff(freq))
    # generate a uniform-sampling to ease computations:
    frq <- seq(from = fmin, to = fmax + df, by = df)
  } else {
    if (is.null(df)) {
      stop("df must be provided when fmax is a single value")
    }
    fmin <- df
    frq <- freq <- seq(from = df, to = fmax + df, by = df)
  }
  nptsp <- length(frq)

  # Compute the parameters of the spectrum
  fm <- 1 / tp
  lgam <- log(gam)
  fr <- frq * tp
  ifm <- trunc(fm / df)

  # sigma = c(.07*ones(1,ifm),.09*ones(1,nptsp-ifm))
  # sigma=rep(c(0.07,0.09),c(ifm,nptsp-ifm))
  sigma <- c(0.07 * rep(1, ifm), 0.09 * rep(1, nptsp - ifm))
  frm4 <- fr^-4
  sp <- (1 - fr) / sigma
  sp <- exp(-0.5 * (sp^2))
  sp <- (frm4 / fr) * exp(-1.25 * frm4) * exp(lgam * sp)

  sp <- sp * (hs / 4)^2 / sum(sp * df)
  sp <- Re(c(0, sp))
  sp <- sp[1:nptsp]
  fr <- c(0, frq)
  fr <- fr[1:nptsp]

  sp <- stats::approx(frq, sp, freq)
  names(sp) <- c("freq", "spec")
  attr(sp, "Note") <- paste0(
    "JONSWAP Spectrum, Hs=",
    hs,
    ", Tp=",
    tp,
    ", gamma=",
    gam
  )
  return(tibble::as_tibble(sp))
}

#' Mean Direction
#'
#' Function for computing the (weighted) arithmetic mean of
#' directional data in \strong{meteorological convention}.
#'
#' @param directions numeric vector of directions, in degree, 0° being the North
#' @param weights numeric vector, usually wind speed of wave height.
#'
#' @returns
#' The (weighted) mean of the values in \code{directions} is computed.
#'
#' @export
#'
#' @examples
#' # Test with some wind directions (unweighted)
#' wind_directions <- c(10, 20, 350, 5, 15) # Directions mostly around North
#' mean_dir <- mean_direction(wind_directions)
#' cat("Mean wind direction (unweighted):", round(mean_dir, 1), "degrees\n")
# Test with wind speeds as weights
#' wind_directions <- c(350, 10, 20, 340, 30) # Directions around North
#' wind_speeds <- c(15, 5, 2, 12, 3) # Higher speeds for directions closer to North
#' mean_dir_weighted <- mean_direction(wind_directions, wind_speeds)
#' cat("Mean wind direction (weighted):", round(mean_dir_weighted, 1), "degrees\n")
#'
#' # Compare weighted vs unweighted for the same data
#' mean_dir_unweighted <- mean_direction(wind_directions)
#' cat("Same data unweighted:", round(mean_dir_unweighted, 1), "degrees\n")
mean_direction <- function(directions, weights = NULL) {
  # If weights provided, check they have the same length as directions
  if (!is.null(weights)) {
    if (length(directions) != length(weights)) {
      stop("Length of 'directions' and 'speeds' must be equal")
    }
    valid_indices <- !is.na(directions) & !is.na(weights)
    directions <- directions[valid_indices]
    weights <- weights[valid_indices]
  } else {
    # Remove NA values from directions only
    directions <- directions[!is.na(directions)]
  }

  # Check for negative weights (which would cause issues with weighting)
  if (any(weights < 0)) {
    warning("Negative weights detected. Using absolute values.")
    weights <- abs(weights)
  }

  # Ensure directions are in [0, 360) range
  directions <- directions %% 360

  # Check if we have any valid directions
  if (length(directions) == 0) {
    return(NA)
  }

  # Set default weights (equal weighting) if weights not provided
  if (is.null(weights)) {
    weights <- rep(1, length(directions))
  }

  # Convert degrees to radians
  radians <- directions * pi / 180

  # Convert to unit vectors (x = sin, y = cos for meteorological convention)
  # In meteorological convention: North = 0°, East = 90°
  x_components <- sin(radians)
  y_components <- cos(radians)

  # Calculate weighted mean components
  mean_x <- stats::weighted.mean(x_components, w = weights)
  mean_y <- stats::weighted.mean(y_components, w = weights)

  # Calculate mean direction in radians
  mean_radians <- atan2(mean_x, mean_y)

  # Convert back to degrees
  mean_degrees <- mean_radians * 180 / pi

  # Ensure result is in [0, 360) range using modulo
  mean_degrees <- mean_degrees %% 360

  return(mean_degrees)
}

#' Directional binning
#'
#' Cuts direction vector into directional bins
# with North bin always centred on 0 degrees.
#'
#' @param directions vector of directions to be binned, in degree, 0° being the North.
#' @param n_bins number of bins, default: 8 sectors.
#' @param labels optional character vector giving the sectors names.
#'
#' @returns
#' a factor vector the same size as \code{directions} with the values binned into sectors.
#'
#' @export
#'
#' @examples
#' # Example usage and demonstration
#' set.seed(123)
#' directions <- runif(20, 0, 360)
#'
#' # Test with different numbers of bins
#' cat("Original directions:\n")
#' print(round(directions, 1))
#'
#' cat("\n8 bins (default):\n")
#' bins_8 <- cut_directions(directions, n_bins = 8)
#' print(bins_8)
#'
#' cat("\n4 bins:\n")
#' bins_4 <- cut_directions(directions, n_bins = 4)
#' print(bins_4)
cut_directions <- function(directions, n_bins = 8, labels = NULL) {
  # Validate inputs
  if (!is.numeric(directions)) {
    stop("directions must be numeric")
  }

  if (n_bins < 2) {
    stop("n_bins must be at least 2")
  }

  # Normalize directions to 0-360 range
  directions <- directions %% 360

  # Calculate bin width
  bin_width <- 360 / n_bins
  half_bin <- bin_width / 2

  # Create breaks - North bin is centered on 0
  # So breaks go from -half_bin to 360-half_bin
  breaks <- seq(-half_bin, 360 - half_bin, by = bin_width)

  # Adjust directions for the split North bin
  # Values in the upper part of North bin (360-half_bin to 360)
  # need to be mapped to negative values (-half_bin to 0)
  adjusted_directions <- ifelse(
    directions > (360 - half_bin),
    directions - 360,
    directions
  )

  # Create labels if not provided
  if (is.null(labels)) {
    # Calculate center angles for each bin
    centers <- seq(0, 360 - bin_width, by = bin_width)

    # Create descriptive labels based on number of bins
    if (n_bins == 4) {
      labels <- c("N", "E", "S", "W")
    } else if (n_bins == 8) {
      labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    } else if (n_bins == 16) {
      labels <- c(
        "N",
        "NNE",
        "NE",
        "ENE",
        "E",
        "ESE",
        "SE",
        "SSE",
        "S",
        "SSW",
        "SW",
        "WSW",
        "W",
        "WNW",
        "NW",
        "NNW"
      )
    } else {
      # Generic labels with center angles
      labels <- paste0("Bin_", sprintf("%.0f", centers))
    }
  }
  # Cut the adjusted directions
  result <- cut(
    adjusted_directions,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  )

  return(result)
}
