#' Add Error Bars To Plot
#'
#' Adds errorbars to a plot.
#'
#' @param data A data frame from compute_grouped_mean_sd
#' @param p A ggplot object to add error bars to
#' @param reference_dose Reference dose value for comparison calculations
#' @param error_bars Type of errorbars to use (ci, se, sd, null)
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return A ggplot2 object with error bars added
add_error_bars_to_plot <- function(
  data,
  p,
  reference_dose,
  error_bars,
  conf_int
) {
  # no reference dose error bars here
  if (is.null(reference_dose)) {
    if (!is.null(error_bars)) {
      if (error_bars == "CI") {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high)
          )
        caption <- paste0("errorbars represent ", round(conf_int * 100), "% CI")
      } else if (error_bars == "SE") {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_dv - .data$se,
              ymax = .data$mean_dv + .data$se
            )
          )
        caption <- paste0("errorbars represent SE")
      } else if (error_bars == "SD") {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_dv - .data$sd,
              ymax = .data$mean_dv + .data$sd,
              y = .data$mean_dv
            )
          )
        caption <- paste0("errorbars represent SD")
      }
    } else {
      caption <- paste0("")
    }
  } else {
    # reference dose error bars
    if (!is.null(error_bars)) {
      if (error_bars == "CI") {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(ymin = .data$ci_low_delta, ymax = .data$ci_up_delta)
          )
        caption <- paste0("errorbars represent ", round(conf_int * 100), "% CI")
      } else if (error_bars == "SE") {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_delta_dv - .data$delta_se,
              ymax = .data$mean_delta_dv + .data$delta_se
            )
          )
        caption <- paste0("errorbars represent SE")
      } else if (error_bars == "SD") {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_delta_dv - .data$delta_sd,
              ymax = .data$mean_delta_dv + .data$delta_sd
            )
          )
        caption <- paste0("errorbars represent SD")
      }
    } else {
      caption <- paste0("")
    }
  }

  #should add label/annotation for thresholds if they aren't null...
  p <- p +
    ggplot2::labs(
      caption = paste(caption, "\n")
    )

  return(p)
}

#' Add Horizontal References
#'
#' Adds horizontal reference lines to plot.
#'
#' @param p A ggplot object
#' @param reference_threshold Numeric/vector of numerics for horizontal lines
#'
#' @return A ggplot2 object with horizontal reference lines added

#'
#' @export
#'
#' @examples
#' eda_mean_dv_over_time(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   NTLD,
#'   DOSEF,
#'   group_col = TRTG,
#'   secondary_data_col = CONC,
#'   reference_dose = "0 mg"
#' ) |>
#'   add_horizontal_references(
#'     reference_threshold = c(-10, 10)
#'   )
add_horizontal_references <- function(p, reference_threshold) {
  if (is.null(reference_threshold) || length(reference_threshold) == 0) {
    return(p)
  }

  for (threshold in reference_threshold) {
    ref_line <- ggplot2::geom_hline(
      yintercept = threshold,
      color = "black",
      linetype = "dashed",
      linewidth = 0.5
    ) |>
      ggstylekit::series_layer(
        name = paste0("Reference ", threshold),
        legend_channel = "color"
      )
    p <- p + ref_line
  }

  p
}


#' Add Secondary Data
#'
#' Adds secondary data to a plot.
#'
#' @param primary_data Dataframe of primary data plotted
#' @param secondary_data Dataframe containing data you'd like to add to plot
#' @param reference_threshold Reference threshold values
#' @param p A ggplot2 object to add data to
#' @param y_data String of column name in secondary data to use for plotting
#' @param group Grouping column
#' @param scale Multiplicative scaling factor
#' @param shift Additive shifting factor
#' @param sec_ylabel Secondary y axis label
#' @param ylabel Primary y axis label
#'

#'
#' @return A ggplot2 object with secondary data layer and dual y-axis
add_secondary_data <- function(
  primary_data,
  secondary_data,
  reference_threshold = NULL,
  p,
  y_data,
  group,
  scale = NULL,
  shift = NULL,
  sec_ylabel,
  ylabel
) {
  group <- rlang::enquo(group)

  checkmate::assert_number(scale, null.ok = TRUE)
  checkmate::assert_number(shift, null.ok = TRUE)

  checkmate::assert(
    if (inherits(ylabel, "glue")) length(as.character(ylabel)) == 1 else TRUE,
    "ylabel must evaluate to a single value"
  )
  checkmate::assert(
    if (inherits(sec_ylabel, "glue")) length(as.character(sec_ylabel)) == 1 else
      TRUE,
    "sec_ylabel must evaluate to a single value"
  )

  # This is from https://finchstudio.io/blog/ggplot-dual-y-axes
  max_first <- max(primary_data[[y_data]])
  min_first <- min(primary_data[[y_data]])
  if (!is.null(reference_threshold)) {
    max_first <- max(max_first, max(c(reference_threshold)))
    min_first <- min(min_first, min(c(reference_threshold)))
  }

  max_second <- max(secondary_data[[y_data]])
  min_second <- min(secondary_data[[y_data]])

  # update args to have scale and shift if not provided
  if (is.null(scale)) {
    scale <- (max_second - min_second) / (max_first - min_first)
  }
  if (is.null(shift)) {
    shift <- min_first - min_second
  }

  p <- p +
    ggplot2::geom_line(
      data = secondary_data,
      ggplot2::aes(
        x = .data$time,
        y = inv_scale_function(.data[[y_data]], scale, shift),
        group = .data$grouping,
        color = .data$grouping,
      )
    ) +
    ggplot2::geom_point(
      data = secondary_data,
      ggplot2::aes(
        x = .data$time,
        y = inv_scale_function(.data[[y_data]], scale, shift),
        group = .data$grouping,
        color = .data$grouping
      )
    )

  p <- p +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        ~ scale_function(., scale, shift),
        name = sec_ylabel
      )
    )

  return(p)
}

# function to scale secondary axis
scale_function <- function(x, scale, shift) {
  return((x) * scale - shift)
}

# function to scale secondary variable values
inv_scale_function <- function(x, scale, shift) {
  return((x + shift) / scale)
}

combine_panels <- function(
  plots,
  nrow = NULL,
  ncol = NULL,
  legend_position = "right",
  title = NULL
) {
  combined <- do.call(
    ggstylekit::combine_styled_plots,
    c(plots, list(nrow = nrow, ncol = ncol))
  )
  combined +
    patchwork::plot_annotation(
      title = title,
      theme = ggplot2::theme(legend.position = legend_position)
    )
}
