# Shared data loading, labelling, and display helpers for the showcase site.
# Base R + DT/ggplot2/plotly (loaded per page). All CSVs live in data/.

DATA_DIR <- "data"

## ---- ordered factor levels + display labels -------------------------------
rung_levels <- c("L1_param", "L2_smooth", "L3_adaptive", "RF_shallow",
                 "L4_aggressive", "L5_nondonsker", "L6_deployable", "L7_hal")
rung_labels <- c(L1_param      = "L1: GLM",
                 L2_smooth     = "L2: + smooth learners",
                 L3_adaptive   = "L3: + random forest",
                 RF_shallow    = "L3b: shallow RF",
                 L4_aggressive = "L4: deep RF (interpolating)",
                 L5_nondonsker = "L5: non-Donsker library",
                 L6_deployable = "L6: deployable library",
                 L7_hal        = "L7: highly adaptive lasso")

# short axis ticks (full names stay in tables/captions) to avoid label truncation
rung_short  <- c(L1_param = "L1", L2_smooth = "L2", L3_adaptive = "L3", RF_shallow = "L3b",
                 L4_aggressive = "L4", L5_nondonsker = "L5", L6_deployable = "L6", L7_hal = "L7")

method_levels <- c("Non-Aware", "Partially-Aware", "Fully-Aware",
                   "Fully-Aware-CV", "Fully-Aware-CF")

scenario_labels <- c(standard = "Design A (many PSUs/stratum)",
                     R1       = "Design B (2 PSUs/stratum, NHANES-like)",
                     DesignC  = "Design C (high design effect)")

ex_labels <- c(E1 = "E1: Short sleep → obesity",
               E2 = "E2: Food insecurity → depression",
               E3 = "E3: E-cigarette use → hypertension",
               E4 = "E4: GDM history → hypertension")

# outcome-/propensity-model specification codes (double-robustness factorial)
spec_labels <- c(C = "correct", W = "wrong")

# arm / method codes -> readable labels. Covers the standard five plus the
# table-specific codes (isolation 2x2, double-robustness, informative, AIPW).
arm_labels <- c(
  "Non-Aware"           = "Non-Aware",
  "Partially-Aware"     = "Partially-Aware",
  "Fully-Aware"         = "Fully-Aware",
  "Fully-Aware-CV"      = "Fully-Aware-CV",
  "Fully-Aware-CF"      = "Fully-Aware-CF",
  "CF-U"                = "Cross-fit, unweighted",
  "CF-W"                = "Cross-fit, weighted",
  "SF-U"                = "Single-fit, unweighted",
  "SF-W"                = "Single-fit, weighted",
  "CF-u"                = "Cross-fit, unweighted",
  "CF-w"                = "Cross-fit, weighted",
  "FA-w"                = "Fully-Aware (single-fit)",
  "Fully-Aware-CF-unwt" = "Fully-Aware-CF (unweighted OOF)",
  "Fully-Aware-CF-wt"   = "Fully-Aware-CF (weighted OOF)",
  "AIPW-SF"             = "AIPW (single-fit)",
  "AIPW-CV"             = "AIPW (internal CV)",
  "AIPW-CF"             = "AIPW (cross-fit)",
  "IPW-svyglm"          = "IPW (svyglm)"
)

estimator_palette <- c(
  "Non-Aware"       = "#999999",
  "Partially-Aware" = "#E69F00",
  "Fully-Aware"     = "#D55E00",
  "Fully-Aware-CV"  = "#009E73",
  "Fully-Aware-CF"  = "#0072B2"
)

# colour-blind-safe palette keyed by the readable arm labels
arm_palette <- c(
  "Non-Aware"                       = "#999999",
  "Partially-Aware"                 = "#E69F00",
  "Fully-Aware"                     = "#D55E00",
  "Fully-Aware-CV"                  = "#009E73",
  "Fully-Aware-CF"                  = "#0072B2",
  "Cross-fit, weighted"             = "#0072B2",
  "Cross-fit, unweighted"           = "#56B4E9",
  "Single-fit, weighted"            = "#D55E00",
  "Single-fit, unweighted"          = "#E69F00",
  "Fully-Aware (single-fit)"        = "#D55E00",
  "Fully-Aware-CF (weighted OOF)"   = "#0072B2",
  "Fully-Aware-CF (unweighted OOF)" = "#56B4E9",
  "AIPW (cross-fit)"                = "#332288",
  "AIPW (internal CV)"              = "#117733",
  "AIPW (single-fit)"               = "#CC6677",
  "IPW (svyglm)"                    = "#AA4499"
)

# colour-blind-safe (Okabe-Ito) palette for the rung-coloured plots
rung_palette <- c(
  "L1: GLM"                     = "#E69F00",
  "L2: + smooth learners"       = "#009E73",
  "L3: + random forest"         = "#0072B2",
  "L4: deep RF (interpolating)" = "#CC79A7"
)

## ---- generic raw reader ---------------------------------------------------
read_csv_app <- function(file)
  read.csv(file.path(DATA_DIR, file), check.names = FALSE, stringsAsFactors = FALSE)

## ---- generic browsable table ----------------------------------------------
# cols: optional named vector c("New label" = "old_col", ...) to select+rename.
# cover: column name to colour-shade by coverage (red < .90 < amber < .935 < green).
# Coded factor values (scenario / rung / example / spec / arm) and logicals are
# recoded to readable labels for display only -- no recomputation.
dt_show <- function(file, cols = NULL, digits = 3, cover = NULL,
                    page = 10, caption = NULL, sort = NULL, ex = NULL, keep = NULL) {
  suppressMessages(library(DT))
  d <- read_csv_app(file)
  if (!is.null(ex) && "example" %in% names(d)) d <- d[d$example %in% ex, , drop = FALSE]
  if (!is.null(keep)) for (cn in names(keep))                 # row filter, e.g. keep=list(kind="jack")
    if (cn %in% names(d)) d <- d[d[[cn]] %in% keep[[cn]], , drop = FALSE]
  if (!is.null(cols)) {
    keep <- unname(cols)[unname(cols) %in% names(d)]
    d <- d[, keep, drop = FALSE]
    names(d) <- names(cols)[match(keep, unname(cols))]
  }
  code_maps <- list(scenario_labels, rung_labels, ex_labels, spec_labels, arm_labels)
  d[] <- lapply(d, function(col) {
    if (is.logical(col)) return(ifelse(col, "Yes", "No"))
    if (is.character(col) || is.factor(col)) {
      v <- as.character(col)
      for (m in code_maps)
        if (length(v) && all(v %in% names(m))) return(unname(m[v]))
    }
    col
  })
  if (!is.null(sort)) {                       # optional default sort (logical rung/arm order)
    keys <- lapply(sort, function(cn) {
      if (!cn %in% names(d)) return(NULL)
      col <- as.character(d[[cn]])
      if (all(col %in% unname(rung_labels))) return(factor(col, levels = unname(rung_labels)))
      if (all(col %in% method_levels))       return(factor(col, levels = method_levels))
      d[[cn]]
    })
    keys <- keys[!vapply(keys, is.null, logical(1))]
    if (length(keys)) d <- d[do.call(order, keys), , drop = FALSE]
  }
  fracnum <- names(d)[vapply(d, function(x)
    is.numeric(x) && any(abs(x - round(x)) > 1e-9, na.rm = TRUE), logical(1))]
  dt <- datatable(d, filter = "top", rownames = FALSE, caption = caption,
                  options = list(pageLength = page, scrollX = TRUE,
                                 order = list(), autoWidth = FALSE))
  if (length(fracnum)) dt <- formatRound(dt, fracnum, digits)
  if (!is.null(cover) && cover %in% names(d))
    dt <- formatStyle(dt, cover,
                      backgroundColor = styleInterval(
                        c(0.90, 0.935), c("#F6D9C9", "#FFF3CD", "#D6F0DB")))
  dt
}

## ---- labelled loaders for the headline plots ------------------------------
load_sim <- function() {
  d <- read_csv_app("sim_full_summary.csv")
  d$Design <- factor(unname(scenario_labels[d$scenario]), levels = unname(scenario_labels))
  d$Rung   <- factor(unname(rung_labels[d$rung]),         levels = unname(rung_labels))
  d$Arm    <- factor(d$method, levels = method_levels)
  d[order(d$Design, d$Rung, d$Arm), ]
}

load_nhanes <- function() {
  n <- read_csv_app("R06_mi_summary.csv")
  n$Arm     <- factor(n$method, levels = method_levels)
  n$Example <- factor(unname(ex_labels[n$example]), levels = unname(ex_labels))
  n[order(n$Example, n$Arm), ]
}

load_largem <- function() {
  d <- read_csv_app("R02_largem_sweep_summary.csv")
  d <- d[d$scenario == "standard", ]
  d$Rung <- factor(unname(rung_labels[d$rung]), levels = unname(rung_labels))
  d$Arm  <- factor(d$method, levels = method_levels)
  d
}

load_ratesweep <- function() {
  d <- read_csv_app("R19_rate_sweep_summary.csv")
  d$Rung <- factor(unname(rung_labels[d$rung]), levels = unname(rung_labels))
  d
}

## ---- reusable "metric by x, coloured by arm" figure -----------------------
# metric = the y column (coverage, reject_rate, ...); xvar = "rung" (discrete) or
# a numeric column (rho, frac, base_m); ref = dashed reference line; facet = an
# optional column; facet_labels/xlab override display; arms/ex restrict rows.
coverage_fig <- function(file, metric = "coverage", xvar = "rung", facet = NULL,
                         arms = NULL, ex = NULL, ylab = "95% CI coverage", ref = 0.95,
                         xlab = NULL, facet_labels = NULL) {
  suppressMessages({ library(ggplot2); library(plotly) })
  d <- read_csv_app(file)
  if (!is.null(ex) && "example" %in% names(d)) d <- d[d$example %in% ex, ]
  if (!is.null(arms)) d <- d[d$method %in% arms, ]
  # A line must not connect two designs at the same x. If a second scenario is
  # present and not the facet, show one design in the figure (the table keeps both).
  subt <- NULL
  if ((is.null(facet) || facet != "scenario") && "scenario" %in% names(d) &&
      length(unique(d$scenario)) > 1) {
    keep_s <- if ("standard" %in% d$scenario) "standard" else sort(unique(d$scenario))[1]
    d <- d[d$scenario == keep_s, , drop = FALSE]
    subt <- paste0("Shown: ", scenario_labels[[keep_s]], " — both designs in the table")
  }
  d$Arm <- factor(unname(arm_labels[d$method]), levels = unique(unname(arm_labels)))
  d <- d[!is.na(d$Arm), , drop = FALSE]; d$Arm <- droplevels(d$Arm)
  d$.y <- d[[metric]]
  if (!is.null(facet) && !is.null(facet_labels) && facet %in% names(d))
    d[[facet]] <- factor(unname(facet_labels[as.character(d[[facet]])]), levels = unname(facet_labels))
  discrete <- identical(xvar, "rung")
  if (discrete) d$.x <- droplevels(factor(unname(rung_short[d$rung]), levels = unname(rung_short)))
  else          d$.x <- d[[xvar]]
  p <- ggplot(d, aes(.x, .y, colour = Arm, group = Arm,
                     text = sprintf("%s\n%s = %.3f", Arm, metric, .y))) +
    geom_line(linewidth = 0.6) + geom_point(size = 2) +
    scale_colour_manual(values = arm_palette) +
    labs(x = if (!is.null(xlab)) xlab else if (discrete) NULL else xvar,
         y = ylab, colour = "Arm", subtitle = subt) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  if (discrete) p <- p + theme(axis.text.x = element_text(angle = 0))
  if (!is.null(ref))        p <- p + geom_hline(yintercept = ref, linetype = "dashed", colour = "grey55")
  if (metric == "coverage") p <- p + scale_y_continuous(limits = c(0, 1))
  if (!is.null(facet))      p <- p + facet_wrap(stats::as.formula(paste("~", facet)))
  ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.2), margin = list(b = 80))
}

## ---- double-robustness figure: bias by nuisance specification -------------
dr_fig <- function(file = "R14_dr_factorial_summary.csv", arm = "CF-w") {
  suppressMessages({ library(ggplot2); library(plotly) })
  d <- read_csv_app(file)
  d <- d[d$method == arm, ]
  d$Rung <- droplevels(factor(unname(rung_labels[d$rung]), levels = unname(rung_labels)))
  d$Spec <- factor(paste0("Q ", spec_labels[d$q_spec], " / g ", spec_labels[d$g_spec]),
                   levels = c("Q correct / g correct", "Q correct / g wrong",
                              "Q wrong / g correct", "Q wrong / g wrong"))
  d$Sampling <- ifelse(d$sampling == "info", "informative sampling", "non-informative")
  p <- ggplot(d, aes(Spec, bias, colour = Spec,
                     text = sprintf("%s\n%s\nbias = %.3f", Spec, Rung, bias))) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_point(size = 2.6) +
    facet_grid(Rung ~ Sampling) +
    scale_colour_manual(values = c("#0072B2", "#009E73", "#E69F00", "#D55E00")) +
    labs(x = NULL, y = "Bias of the cross-fitted estimate", colour = NULL) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 22, hjust = 1), legend.position = "none")
  ggplotly(p, tooltip = "text")
}

## ---- single-example NHANES forest (for the per-question tabs) --------------
forest_fig <- function(ex) {
  suppressMessages({ library(ggplot2); library(plotly) })
  n <- load_nhanes(); n <- n[n$example == ex, ]
  n$cert <- n$Arm == "Fully-Aware-CF"
  p <- ggplot(n, aes(b, Arm, colour = Arm,
                     text = sprintf("%s\nRD = %.3f (%.3f, %.3f)", Arm, b, lcl, ucl))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_errorbarh(aes(xmin = lcl, xmax = ucl, linewidth = cert), height = 0.22) +
    geom_point(aes(size = cert)) +
    scale_linewidth_manual(values = c(`FALSE` = 0.6, `TRUE` = 1.2), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 2.2, `TRUE` = 3.4), guide = "none") +
    scale_colour_manual(values = estimator_palette) +
    labs(x = "Risk difference (95% CI)", y = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "none")
  ggplotly(p, tooltip = "text")
}

## ---- Table 1 balance (Love) plot: |SMD| per covariate for one example -------
smd_fig <- function(ex) {
  suppressMessages({ library(ggplot2); library(plotly) })
  d <- read_csv_app("nhanes_table1.csv")
  d <- d[d$example == ex, c("characteristic", "smd")]
  d$smd_n <- suppressWarnings(as.numeric(d$smd))
  d <- d[!is.na(d$smd_n), , drop = FALSE]
  d$lab <- factor(trimws(d$characteristic), levels = trimws(d$characteristic)[order(d$smd_n)])
  p <- ggplot(d, aes(smd_n, lab, text = sprintf("%s\nSMD = %.3f", trimws(characteristic), smd_n))) +
    geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey55") +
    geom_segment(aes(x = 0, xend = smd_n, yend = lab), colour = "#0072B2", linewidth = 0.5) +
    geom_point(size = 2.4, colour = "#0072B2") +
    labs(x = "Standardized mean difference between exposure groups", y = NULL,
         subtitle = "Dashed line: |SMD| = 0.1 (common imbalance threshold).") +
    theme_minimal(base_size = 12)
  ggplotly(p, tooltip = "text")
}

## ---- per-scenario diagnostic: min propensity by rung, single-fit vs cross-fit
diag_fig <- function() {
  suppressMessages({ library(ggplot2); library(plotly) })
  dg <- read_csv_app("sim_full_diagnostics.csv")
  dg$Design <- factor(unname(scenario_labels[dg$scenario]), levels = unname(scenario_labels))
  dg$Rung   <- factor(unname(rung_short[dg$rung]), levels = unname(rung_short))
  long <- rbind(
    data.frame(Design = dg$Design, Rung = dg$Rung, Fit = "Single-fit", gmin = as.numeric(dg$g_fa_min)),
    data.frame(Design = dg$Design, Rung = dg$Rung, Fit = "Cross-fit",  gmin = as.numeric(dg$g_cf_min)))
  long$Fit <- factor(long$Fit, levels = c("Single-fit", "Cross-fit"))
  p <- ggplot(long, aes(Rung, gmin, colour = Fit, group = Fit,
                        text = sprintf("%s\n%s\nmin propensity = %.3f", Fit, Rung, gmin))) +
    geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey55") +
    geom_line(linewidth = 0.7) + geom_point(size = 2.4) + facet_wrap(~Design) +
    scale_colour_manual(values = c("Single-fit" = "#D55E00", "Cross-fit" = "#0072B2")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = "Minimum estimated propensity", colour = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.2))
}
