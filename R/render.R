# Rendering plots to PNG for comparison
#
# Plot inputs (ggplot objects, plotting functions and recorded plots) are
# rendered to a temporary PNG file so they can be compared with odiff.

#' Plot Rendering Options
#'
#' Create a set of options controlling how plot inputs (ggplot objects,
#' plotting functions and recorded plots) are rendered to PNG before being
#' compared by [compare_images()], [expect_images_match()],
#' [expect_images_differ()] and [expect_snapshot_image()].
#'
#' Plots are rendered with [ragg::agg_png()] when the ragg package is
#' installed (recommended: its output is consistent across platforms), and
#' with [grDevices::png()] otherwise. When comparing a plot against a stored
#' baseline PNG, render it with the same options (and the same graphics
#' device) that were used to create the baseline, or the comparison will fail
#' with a layout difference.
#'
#' @param width,height Plot dimensions, in `units`. Default is 7 x 5 inches.
#' @param units Units for `width` and `height`: one of `"in"`, `"cm"`,
#'   `"mm"` or `"px"`. Default is `"in"`.
#' @param res Resolution in pixels per inch. Default is 96.
#' @param bg Background colour. Default is `"white"`.
#'
#' @return A list of class `odiffr_plot_options`, to be passed as the
#'   `plot_options` argument.
#'
#' @seealso [compare_images()], [expect_snapshot_image()]
#'
#' @export
#'
#' @examples
#' plot_options(width = 4, height = 3, res = 72)
plot_options <- function(width = 7, height = 5, units = "in", res = 96,
                         bg = "white") {
  is_pos_num <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x > 0
  }
  if (!is_pos_num(width)) {
    stop("width must be a single positive number.", call. = FALSE)
  }
  if (!is_pos_num(height)) {
    stop("height must be a single positive number.", call. = FALSE)
  }
  if (!is_pos_num(res)) {
    stop("res must be a single positive number.", call. = FALSE)
  }
  if (!is.character(units) || length(units) != 1 ||
      !units %in% c("in", "cm", "mm", "px")) {
    stop("units must be one of \"in\", \"cm\", \"mm\" or \"px\".",
         call. = FALSE)
  }
  if (length(bg) != 1 || is.na(bg)) {
    stop("bg must be a single colour.", call. = FALSE)
  }
  structure(
    list(width = width, height = height, units = units, res = res, bg = bg),
    class = "odiffr_plot_options"
  )
}

#' @export
print.odiffr_plot_options <- function(x, ...) {
  cat(sprintf("<odiffr plot options> %g x %g %s, %g dpi, bg = %s\n",
              x$width, x$height, x$units, x$res, format(x$bg)))
  invisible(x)
}

# Internal: validate/normalise a plot_options argument into a plain list
.as_plot_options <- function(x) {
  if (is.null(x)) {
    return(unclass(plot_options()))
  }
  if (inherits(x, "odiffr_plot_options")) {
    return(unclass(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    known <- names(formals(plot_options))
    if (length(x) > 0 &&
        (is.null(names(x)) || any(!nzchar(names(x))) ||
         any(!names(x) %in% known))) {
      stop("plot_options must be created with plot_options(), or be a ",
           "named list with elements from: ",
           paste(known, collapse = ", "), ".", call. = FALSE)
    }
    return(unclass(do.call(plot_options, x)))
  }
  stop("plot_options must be NULL or created with plot_options().",
       call. = FALSE)
}

# Internal: is `x` a ggplot object (including ggplot2 >= 4.0 S7 objects)?
.is_ggplot <- function(x) {
  if (inherits(x, "ggplot") || inherits(x, "ggplot2::ggplot")) {
    return(TRUE)
  }
  if (isNamespaceLoaded("ggplot2")) {
    ns <- asNamespace("ggplot2")
    if (exists("is_ggplot", envir = ns, inherits = FALSE)) {
      return(isTRUE(get("is_ggplot", envir = ns)(x)))
    }
  }
  FALSE
}

# Internal: is `x` a recorded base graphics plot?
.is_recorded_plot <- function(x) {
  inherits(x, "recordedplot")
}

# Internal: is `x` a plot input (ggplot, recorded plot or plotting function)?
.is_plot_input <- function(x) {
  .is_ggplot(x) || .is_recorded_plot(x) || is.function(x)
}

# Internal: can function `f` be called with no arguments? (every formal
# argument is `...` or has a default value)
.callable_without_args <- function(f) {
  fmls <- formals(f)
  if (is.null(fmls)) {
    return(TRUE)
  }
  missing_default <- vapply(fmls, function(v) {
    is.symbol(v) && !nzchar(as.character(v))
  }, logical(1))
  all(!missing_default | names(fmls) == "...")
}

# Internal: is the ragg package available?
.has_ragg <- function() {
  requireNamespace("ragg", quietly = TRUE)
}

# Internal: open a PNG graphics device, preferring ragg
.open_png_device <- function(path, width, height, units, res, bg) {
  if (.has_ragg()) {
    ragg::agg_png(filename = path, width = width, height = height,
                  units = units, res = res, background = bg)
  } else {
    grDevices::png(filename = path, width = width, height = height,
                   units = units, res = res, bg = bg)
  }
  invisible(grDevices::dev.cur())
}

# Internal: render a plot input to a PNG file and return the path.
# - ggplot objects are printed
# - functions are called with no arguments (a returned ggplot or lattice
#   plot is printed, and a returned grob is drawn)
# - recorded plots are replayed with grDevices::replayPlot()
# The device is always closed (even on error) and the previously active
# device is restored.
.render_plot_to_png <- function(x, path = tempfile(fileext = ".png"),
                                width = 7, height = 5, units = "in",
                                res = 96, bg = "white") {
  if (!.is_plot_input(x)) {
    stop("x must be a ggplot object, a function that draws a plot, ",
         "or a recorded plot.", call. = FALSE)
  }
  if (is.function(x) && !.callable_without_args(x)) {
    stop("A plotting function must be callable with no arguments.",
         call. = FALSE)
  }

  prev_dev <- grDevices::dev.cur()
  dev <- .open_png_device(path, width, height, units, res, bg)
  on.exit({
    if (dev %in% grDevices::dev.list()) {
      grDevices::dev.off(dev)
    }
    if (prev_dev > 1 && prev_dev %in% grDevices::dev.list()) {
      grDevices::dev.set(prev_dev)
    }
  }, add = TRUE)

  if (.is_recorded_plot(x)) {
    grDevices::replayPlot(x)
  } else if (.is_ggplot(x)) {
    print(x)
  } else {
    value <- x()
    # Plot objects returned by the function only appear when printed/drawn
    if (.is_ggplot(value) || inherits(value, "trellis")) {
      print(value)
    } else if (inherits(value, "grob") || inherits(value, "gList")) {
      grid::grid.newpage()
      grid::grid.draw(value)
    }
  }

  path
}

# Internal: render a plot input using a plot_options specification
.render_plot_with_options <- function(x, plot_options = NULL,
                                      path = tempfile(fileext = ".png")) {
  opts <- .as_plot_options(plot_options)
  tmp <- path
  ok <- FALSE
  on.exit(if (!ok) unlink(tmp), add = TRUE)
  .render_plot_to_png(x, path = tmp, width = opts$width,
                      height = opts$height, units = opts$units,
                      res = opts$res, bg = opts$bg)
  if (!file.exists(tmp)) {
    stop("Rendering the plot did not produce a PNG file.", call. = FALSE)
  }
  ok <- TRUE
  tmp
}
