# =====================================================================
# Figures additionnelles pour la section Modelisation
# Lit les sorties du pipeline principal et produit 6 figures
# Toutes les figures restent dans le perimetre du cours dispense.
# =====================================================================

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package ggplot2 est requis pour les figures. Installe-le avec install.packages('ggplot2').")
  }
})

# ---------------------------------------------------------------------
# Configuration des chemins
# ---------------------------------------------------------------------
find_input_path <- function() {
  env_path <- Sys.getenv("INPUT_PATH", unset = "")
  candidates <- c(
    env_path,
    "student-mat.csv",
    "studentmat.csv"
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("Fichier de donnees introuvable. Place student-mat.csv dans le dossier courant ou definis INPUT_PATH.")
  }
  found[1]
}

INPUT_PATH <- find_input_path()
OUT_DIR <- "outputs_modelisation"
TAB_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

if (file.exists("SaveFigure.r")) {
  source("SaveFigure.r", local = FALSE)
}

FIG_WIDTH_CM <- 15
FIG_HEIGHT_CM <- 11.25
FIG_DPI <- 600

theme_report <- function() {
  ggplot2::theme_bw(base_size = 10, base_family = "serif") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank()
    )
}

save_pdf_figure <- function(fname, plot_object) {
  if (exists("SaveFigure", mode = "function")) {
    return(SaveFigure(
      plot_object = plot_object,
      fname = fname,
      folder = FIG_DIR,
      dpi = FIG_DPI,
      width_cm = FIG_WIDTH_CM,
      height_cm = FIG_HEIGHT_CM,
      apply_theme = FALSE
    ))
  }

  full_path <- file.path(FIG_DIR, basename(fname))
  ggplot2::ggsave(
    filename = full_path,
    plot = plot_object,
    width = FIG_WIDTH_CM / 2.54,
    height = FIG_HEIGHT_CM / 2.54,
    units = "in",
    device = "pdf"
  )

  invisible(full_path)
}

# ---------------------------------------------------------------------
# Chargement et preparation identique au pipeline principal
# ---------------------------------------------------------------------

df <- read.csv(INPUT_PATH, sep = ";", stringsAsFactors = FALSE)
df$passed <- as.integer(df$G3 >= 10)

TERMS_QUANT <- c(
  "G1", "G2", "age", "Medu", "Fedu", "traveltime", "studytime",
  "failures", "famrel", "freetime", "goout", "Dalc", "Walc",
  "health", "absences"
)
TERMS_QUALI <- c(
  "school", "sex", "address", "famsize", "Pstatus", "Mjob", "Fjob",
  "reason", "guardian", "schoolsup", "famsup", "paid", "activities",
  "nursery", "higher", "internet", "romantic"
)

for (v in TERMS_QUALI) {
  df[[v]] <- factor(df[[v]])
}

set_ref <- function(data, var, ref) {
  if (!ref %in% levels(data[[var]])) {
    stop(sprintf("Reference '%s' absente pour la variable '%s'.", ref, var))
  }
  data[[var]] <- stats::relevel(data[[var]], ref = ref)
  data
}

df <- set_ref(df, "school", "GP")
df <- set_ref(df, "sex", "F")
df <- set_ref(df, "address", "R")
df <- set_ref(df, "famsize", "GT3")
df <- set_ref(df, "Pstatus", "T")
df <- set_ref(df, "Mjob", "other")
df <- set_ref(df, "Fjob", "other")
df <- set_ref(df, "reason", "course")
df <- set_ref(df, "guardian", "mother")
df <- set_ref(df, "schoolsup", "no")
df <- set_ref(df, "famsup", "no")
df <- set_ref(df, "paid", "no")
df <- set_ref(df, "activities", "no")
df <- set_ref(df, "nursery", "no")
df <- set_ref(df, "higher", "no")
df <- set_ref(df, "internet", "no")
df <- set_ref(df, "romantic", "no")

make_formula <- function(response, terms) {
  stats::as.formula(paste(response, "~", paste(stats::na.omit(terms), collapse = " + ")))
}

suppress_logit_boundary_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("^glm\\.fit: .*0 (or|ou) 1", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# ---------------------------------------------------------------------
# Reconstruction des modeles cles
# Priorite : termes generes par Modelisation.R
# Fallback : termes fixes correspondant au script Python fourni.
# ---------------------------------------------------------------------

terms_file <- file.path(TAB_DIR, "3.10_termes_retenus_par_modele.csv")
if (file.exists(terms_file)) {
  terms_summary <- read.csv(terms_file, stringsAsFactors = FALSE)
  TERMS_SEL <- stats::na.omit(terms_summary$modele_lineaire_avec_G1_G2)
  TERMS_SEL_HN <- stats::na.omit(terms_summary$modele_lineaire_sans_G1_G2)
  TERMS_LOGIT_SEL <- stats::na.omit(terms_summary$modele_logistique_selectionne)
} else {
  TERMS_SEL <- c(
    "G1", "G2", "age", "famrel", "Walc", "absences",
    "school", "activities", "romantic"
  )
  TERMS_SEL_HN <- c(
    "age", "Medu", "studytime", "failures", "freetime", "goout", "absences",
    "sex", "famsize", "Mjob", "schoolsup", "famsup", "romantic"
  )
  TERMS_LOGIT_SEL <- c(
    "G1", "G2", "age", "Fedu", "studytime", "famrel", "goout", "Walc",
    "school", "sex", "famsize", "Pstatus", "Mjob", "Fjob", "internet", "romantic"
  )
}

mod_sel <- stats::lm(make_formula("G3", TERMS_SEL), data = df)
mod_sel_hn <- stats::lm(make_formula("G3", TERMS_SEL_HN), data = df)
mod_logit_sel <- suppress_logit_boundary_warning(
  stats::glm(
    make_formula("passed", TERMS_LOGIT_SEL),
    data = df,
    family = stats::binomial(link = "logit")
  )
)

# Mapping noms longs -> labels lisibles
short_label <- function(term) {
  if (term == "(Intercept)") return("Constante")
  factor_vars <- TERMS_QUALI[order(nchar(TERMS_QUALI), decreasing = TRUE)]
  for (v in factor_vars) {
    if (startsWith(term, v) && term != v) {
      lev <- substring(term, nchar(v) + 1)
      return(paste0(v, " = ", lev))
    }
  }
  term
}

# =====================================================================
# Figure 1 - Histogramme des residus + courbe normale superposee
# =====================================================================
cat("[1/6] Histogramme des residus avec densite normale\n")

residus <- stats::residuals(mod_sel)
mu <- mean(residus)
sigma <- stats::sd(residus)

residus_df <- data.frame(residu = residus)
normal_label <- sprintf("N(%.2f, %.2f^2)", mu, sigma)
plot_hist_normale <- ggplot2::ggplot(residus_df, ggplot2::aes(x = residu)) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density), fill = "Densite empirique"),
    bins = 35,
    color = "black",
    linewidth = 0.25
  ) +
  ggplot2::stat_function(
    ggplot2::aes(color = normal_label),
    fun = stats::dnorm,
    args = list(mean = mu, sd = sigma),
    linewidth = 0.7
  ) +
  ggplot2::geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4) +
  ggplot2::scale_fill_manual(NULL, values = c("Densite empirique" = "steelblue")) +
  ggplot2::scale_color_manual(NULL, values = stats::setNames("firebrick", normal_label)) +
  ggplot2::labs(
    x = "Residus e_i",
    y = "Densite",
    title = "Distribution des residus du modele lineaire selectionne"
  ) +
  theme_report() +
  ggplot2::theme(legend.position = "top")
save_pdf_figure("3.7_histogramme_residus_avec_normale.pdf", plot_hist_normale)

# =====================================================================
# Figure 2 - Forest plot des coefficients du modele structurel
# =====================================================================
cat("[2/6] Forest plot des coefficients du modele structurel\n")

ci <- suppressMessages(stats::confint.default(mod_sel_hn, level = 0.95))
coefs <- data.frame(
  term = names(stats::coef(mod_sel_hn)),
  estimate = as.numeric(stats::coef(mod_sel_hn)),
  ci_low = ci[, 1],
  ci_high = ci[, 2],
  p_value = summary(mod_sel_hn)$coefficients[, 4],
  stringsAsFactors = FALSE
)
coefs <- coefs[coefs$term != "(Intercept)", ]
coefs$label <- vapply(coefs$term, short_label, character(1))
coefs <- coefs[order(coefs$estimate), ]
coefs$label <- factor(coefs$label, levels = coefs$label)
coefs$significatif <- factor(
  ifelse(coefs$p_value < 0.05, "p < 0.05", "p >= 0.05"),
  levels = c("p < 0.05", "p >= 0.05")
)

plot_coefs <- ggplot2::ggplot(coefs, ggplot2::aes(y = label)) +
  ggplot2::geom_vline(xintercept = 0, color = "firebrick", linetype = "dashed", linewidth = 0.4) +
  ggplot2::geom_segment(
    ggplot2::aes(x = ci_low, xend = ci_high, yend = label),
    color = "gray50",
    linewidth = 0.45
  ) +
  ggplot2::geom_point(ggplot2::aes(x = estimate, color = significatif), size = 1.9) +
  ggplot2::scale_color_manual(NULL, values = c("p < 0.05" = "firebrick", "p >= 0.05" = "gray40")) +
  ggplot2::labs(
    x = "Coefficient beta_hat_j avec IC a 95%",
    y = NULL,
    title = "Coefficients du modele structurel sans G1, G2"
  ) +
  theme_report() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 8),
    legend.position = "bottom"
  )
save_pdf_figure("3.8_forest_plot_coefficients_structurel.pdf", plot_coefs)

# =====================================================================
# Figure 3 - Forest plot des odds ratios en echelle log
# =====================================================================
cat("[3/6] Forest plot des odds ratios en echelle log\n")

ci_logit <- suppressMessages(stats::confint.default(mod_logit_sel, level = 0.95))
or_df <- data.frame(
  term = names(stats::coef(mod_logit_sel)),
  OR = exp(stats::coef(mod_logit_sel)),
  OR_low = exp(ci_logit[, 1]),
  OR_high = exp(ci_logit[, 2]),
  p_value = summary(mod_logit_sel)$coefficients[, 4],
  stringsAsFactors = FALSE
)
or_df <- or_df[or_df$term != "(Intercept)", ]
or_df$label <- vapply(or_df$term, short_label, character(1))

TRUNC <- 1000
or_df$OR_plot <- pmin(pmax(or_df$OR, 1 / TRUNC), TRUNC)
or_df$OR_low_plot <- pmax(or_df$OR_low, 1 / TRUNC)
or_df$OR_high_plot <- pmin(or_df$OR_high, TRUNC)
or_df$truncated <- or_df$OR_high > TRUNC | or_df$OR_low < 1 / TRUNC
or_df <- or_df[order(or_df$OR), ]
or_df$label <- factor(or_df$label, levels = or_df$label)
or_df$significatif <- factor(
  ifelse(or_df$p_value < 0.05, "p < 0.05", "p >= 0.05"),
  levels = c("p < 0.05", "p >= 0.05")
)

plot_or <- ggplot2::ggplot(or_df, ggplot2::aes(y = label)) +
  ggplot2::geom_vline(xintercept = 1, color = "firebrick", linetype = "dashed", linewidth = 0.4) +
  ggplot2::geom_segment(
    ggplot2::aes(x = OR_low_plot, xend = OR_high_plot, yend = label),
    color = "gray50",
    linewidth = 0.45
  ) +
  ggplot2::geom_point(ggplot2::aes(x = OR_plot, color = significatif), size = 1.9) +
  ggplot2::geom_segment(
    data = or_df[or_df$OR_high > TRUNC, ],
    ggplot2::aes(x = TRUNC / 2, xend = TRUNC * 0.95, y = label, yend = label),
    inherit.aes = FALSE,
    color = "gray40",
    linewidth = 0.35,
    arrow = grid::arrow(length = grid::unit(0.08, "in"))
  ) +
  ggplot2::scale_x_log10(
    limits = c(1 / TRUNC, TRUNC),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  ) +
  ggplot2::scale_color_manual(NULL, values = c("p < 0.05" = "firebrick", "p >= 0.05" = "gray40")) +
  ggplot2::labs(
    x = "Odds Ratio, echelle logarithmique, IC a 95%",
    y = NULL,
    title = "Odds ratios du modele logistique selectionne"
  ) +
  theme_report() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 8),
    legend.position = "bottom"
  )
save_pdf_figure("3.9_forest_plot_odds_ratios.pdf", plot_or)

# =====================================================================
# Figure 4 - Distribution des probabilites predites par classe reelle
# =====================================================================
cat("[4/6] Distribution des probabilites predites par classe\n")

proba <- stats::predict(mod_logit_sel, newdata = df, type = "response")
n_fail <- sum(df$passed == 0)
n_pass <- sum(df$passed == 1)

classe_fail <- sprintf("Echec (n = %d)", n_fail)
classe_pass <- sprintf("Reussite (n = %d)", n_pass)
proba_df <- data.frame(
  proba = proba,
  classe = factor(
    ifelse(df$passed == 0, classe_fail, classe_pass),
    levels = c(classe_fail, classe_pass)
  )
)
seuil_df <- data.frame(seuil = "Seuil pi_hat = 0.5", xintercept = 0.5)

plot_proba <- ggplot2::ggplot(proba_df, ggplot2::aes(x = proba, fill = classe)) +
  ggplot2::geom_histogram(
    breaks = seq(0, 1, length.out = 36),
    position = "identity",
    alpha = 0.65,
    color = "black",
    linewidth = 0.25
  ) +
  ggplot2::geom_vline(
    data = seuil_df,
    ggplot2::aes(xintercept = xintercept, linetype = seuil),
    color = "black",
    linewidth = 0.4
  ) +
  ggplot2::scale_fill_manual(NULL, values = stats::setNames(
    c(rgb(0.70, 0.13, 0.13, 0.65), rgb(0.27, 0.51, 0.71, 0.65)),
    c(classe_fail, classe_pass)
  )) +
  ggplot2::scale_linetype_manual(NULL, values = c("Seuil pi_hat = 0.5" = "dashed")) +
  ggplot2::coord_cartesian(xlim = c(0, 1)) +
  ggplot2::labs(
    x = "Probabilite predite de reussite pi_hat_i",
    y = "Nombre d'eleves",
    title = "Probabilites predites par le modele logistique selectionne"
  ) +
  theme_report() +
  ggplot2::theme(legend.position = "top")
save_pdf_figure("3.9_distribution_probabilites_predites.pdf", plot_proba)

# =====================================================================
# Figure 5 - Observe vs ajuste pour le modele lineaire selectionne
# =====================================================================
cat("[5/6] Observe vs ajuste pour le modele lineaire selectionne\n")

fitted_values <- stats::fitted(mod_sel)
mask_zero <- df$G3 == 0

zero_label <- sprintf("G3 = 0 (n = %d)", sum(mask_zero))
obs_df <- data.frame(
  G3_ajuste = fitted_values,
  G3 = df$G3,
  groupe = factor(ifelse(mask_zero, zero_label, "G3 > 0"), levels = c("G3 > 0", zero_label))
)
diagonale_df <- data.frame(ligne = "Diagonale y = y_hat", intercept = 0, slope = 1)

plot_obs_ajuste <- ggplot2::ggplot(obs_df, ggplot2::aes(x = G3_ajuste, y = G3)) +
  ggplot2::geom_abline(
    data = diagonale_df,
    ggplot2::aes(intercept = intercept, slope = slope, linetype = ligne),
    color = "black",
    linewidth = 0.4
  ) +
  ggplot2::geom_point(ggplot2::aes(color = groupe, shape = groupe), alpha = 0.65, size = 1.7) +
  ggplot2::scale_color_manual(NULL, values = c("G3 > 0" = "steelblue", stats::setNames("firebrick", zero_label))) +
  ggplot2::scale_shape_manual(NULL, values = c("G3 > 0" = 16, stats::setNames(4, zero_label))) +
  ggplot2::scale_linetype_manual(NULL, values = c("Diagonale y = y_hat" = "dashed")) +
  ggplot2::coord_fixed(ratio = 1, xlim = c(-2, 22), ylim = c(-2, 22)) +
  ggplot2::labs(
    x = "Valeurs ajustees G3_hat",
    y = "Valeurs observees G3",
    title = "Note observee contre note ajustee"
  ) +
  theme_report() +
  ggplot2::theme(legend.position = "top")
save_pdf_figure("3.7_observe_vs_ajuste.pdf", plot_obs_ajuste)

# =====================================================================
# Figure 6 - Evolution de l'AIC durant la selection
# =====================================================================
cat("[6/6] Evolution de l'AIC durant la selection\n")

log_file <- file.path(TAB_DIR, "3.6_log_selection_AIC_avec_notes.csv")
if (!file.exists(log_file)) {
  stop("Le fichier 3.6_log_selection_AIC_avec_notes.csv est introuvable. Lance Modelisation.R avant ce script.")
}
proc_file <- file.path(TAB_DIR, "3.6_comparaison_procedures_AIC_avec_notes.csv")
if (!file.exists(proc_file)) {
  stop("Le fichier 3.6_comparaison_procedures_AIC_avec_notes.csv est introuvable. Lance Modelisation.R avant ce script.")
}

proc_table <- read.csv(proc_file, stringsAsFactors = FALSE)
best_proc <- proc_table$procedure[which.min(proc_table$AIC)]

log_sel <- read.csv(log_file, stringsAsFactors = FALSE)
log_sel <- subset(log_sel, procedure == best_proc & is.finite(AIC))

if (nrow(log_sel) == 0) {
  stop("Aucune iteration AIC valide pour la procedure retenue : ", best_proc)
}

last <- nrow(log_sel)
aic_labels <- log_sel[c(1, last), ]
aic_labels$label <- sprintf("AIC = %.0f", aic_labels$AIC)
aic_labels$vjust <- c(-0.8, 1.6)

plot_aic <- ggplot2::ggplot(log_sel, ggplot2::aes(x = iteration, y = AIC)) +
  ggplot2::geom_line(color = "black", linewidth = 0.4) +
  ggplot2::geom_point(color = "black", size = 1.8) +
  ggplot2::geom_text(
    data = aic_labels,
    ggplot2::aes(label = label, vjust = vjust),
    size = 3,
    family = "serif"
  ) +
  ggplot2::labs(
    x = "Iteration de la procedure AIC retenue",
    y = "AIC",
    title = paste("Evolution de l'AIC pendant la selection :", best_proc)
  ) +
  theme_report()
save_pdf_figure("3.6_evolution_AIC_selection.pdf", plot_aic)

cat("\n[OK] Six figures additionnelles generees dans : ", normalizePath(FIG_DIR), "\n", sep = "")
for (fig in sort(list.files(FIG_DIR, pattern = "\\.pdf$"))) {
  cat("  ", fig, "\n", sep = "")
}
