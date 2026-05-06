# =====================================================================
# Modelisation - ACTU-F-4001 - Student Performance
# =====================================================================
# Objectif : modeliser la performance finale (G3) a partir des autres
# variables quantitatives et qualitatives, en utilisant des modeles
# lineaires et logistiques, et en comparant les procedures de selection
# de modeles.
# =====================================================================

# =====================================================================
# 1. Configuration
# =====================================================================
rm(list = ls())
suppressPackageStartupMessages({
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Le package MASS est requis. Installer avec install.packages('MASS').")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package ggplot2 est requis. Installer avec install.packages('ggplot2').")
  }
})
set.seed(42)
options(stringsAsFactors = FALSE)
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
TERMS_FULL <- c(TERMS_QUANT, TERMS_QUALI)
TERMS_HORS_NOTES <- setdiff(TERMS_FULL, c("G1", "G2"))
TERMS_SANS_G1 <- setdiff(TERMS_FULL, "G1")
TERMS_SANS_G2 <- setdiff(TERMS_FULL, "G2")
FACTOR_REFERENCES <- c(
  school = "GP",
  sex = "F",
  address = "R",
  famsize = "GT3",
  Pstatus = "T",
  Mjob = "other",
  Fjob = "other",
  reason = "course",
  guardian = "mother",
  schoolsup = "no",
  famsup = "no",
  paid = "no",
  activities = "no",
  nursery = "no",
  higher = "no",
  internet = "no",
  romantic = "no"
)

resolve_input_path <- function() {
  candidates <- c(
    Sys.getenv("STUDENT_DATA_PATH", unset = ""),
    Sys.getenv("INPUT_PATH", unset = ""),
    "student-mat.csv",
    "studentmat.csv"
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop(
      "Fichier de donnees introuvable. ",
      "Definir STUDENT_DATA_PATH ou INPUT_PATH, ou placer student-mat.csv ici."
    )
  }
  found[1]
}
INPUT_PATH <- resolve_input_path()
OUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "outputs_modelisation")
TAB_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR <- file.path(OUT_DIR, "figures")
FIG_WIDTH_CM <- 15
FIG_HEIGHT_CM <- 11.25
FIG_DPI <- 600

if (file.exists("SaveFigure.r")) {
  source("SaveFigure.r", local = FALSE)
}
# =====================================================================
# 2. Chargement
# =====================================================================
df <- utils::read.csv(INPUT_PATH, sep = ";", stringsAsFactors = FALSE)
n_obs <- nrow(df)
cat(sprintf("[Chargement] %d observations, %d variables.\n", n_obs, ncol(df)))
required_cols <- unique(c("G3", TERMS_FULL))
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Colonnes manquantes : ", paste(missing_cols, collapse = ", "))
}
# =====================================================================
# 3. Preparation
# =====================================================================

df$passed <- as.integer(df$G3 >= 10)

cat(sprintf(
  "[Preparation] Reussite : %d admis (%.1f%%) sur %d eleves.\n",
  sum(df$passed),
  mean(df$passed) * 100,
  n_obs
))

for (var in TERMS_QUALI) {
  df[[var]] <- factor(df[[var]])
}

for (var in names(FACTOR_REFERENCES)) {
  ref <- FACTOR_REFERENCES[[var]]

  if (!ref %in% levels(df[[var]])) {
    stop(sprintf("Reference '%s' absente pour la variable '%s'.", ref, var))
  }

  df[[var]] <- stats::relevel(df[[var]], ref = ref)
}

# =====================================================================
# 4. Fonctions utilitaires
# =====================================================================

make_formula <- function(response, terms) {
  stats::as.formula(paste(response, "~", paste(terms, collapse = " + ")))
}

theme_report <- function() {
  ggplot2::theme_bw(base_size = 10, base_family = "serif") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank()
    )
}

suppress_logit_boundary_warning <- function(expr, contexte = "modele logit") {
  warns <- character(0)
  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      boundary_warning <- grepl("^glm\\.fit: .*0 (or|ou) 1", msg)

      if (boundary_warning) {
        warns <<- c(warns, msg)
        invokeRestart("muffleWarning")
      }
    }
  )

  if (length(warns) > 0L) {
    cat(sprintf(
      "  [info] %s : %d avertissement(s) glm.fit signalent que certaines\n         probabilites predites atteignent 0 ou 1.\n",
      contexte, length(warns)
    ))
  }

  result
}

fit_model <- function(formula, data, family = c("ols", "logit"),
                     contexte = "modele logit") {
  family <- match.arg(family)
  fit_args <- list(formula = formula, data = data)
  if (family == "ols") {
    return(do.call("lm", fit_args, envir = asNamespace("stats")))
  }
  fit_args$family <- stats::binomial(link = "logit")
  suppress_logit_boundary_warning(
    do.call("glm", fit_args, envir = asNamespace("stats")),
    contexte = contexte
  )
}

step_aic_select <- function(data, response, terms, family = c("ols", "logit"),
                            direction = c("backward", "forward", "both"),
                            start = c("full", "null"), verbose = FALSE,
                            contexte = "stepAIC logit") {
  family <- match.arg(family)
  direction <- match.arg(direction)
  start <- match.arg(start)

  full_formula <- make_formula(response, terms)
  null_formula <- stats::as.formula(paste(response, "~ 1"))

  start_formula <- if (start == "full") full_formula else null_formula
  start_model <- fit_model(
    start_formula, data, family = family,
    contexte = paste(contexte, "-", direction, start, "- modele de depart")
  )

  trace_value <- if (isTRUE(verbose)) 1 else 0
  scope_value <- list(lower = null_formula, upper = full_formula)

  final_model <- if (family == "logit") {
    suppress_logit_boundary_warning(
      MASS::stepAIC(
        start_model,
        scope = scope_value,
        direction = direction,
        trace = trace_value
      ),
      contexte = paste(contexte, "-", direction, start, "- iterations")
    )
  } else {
    MASS::stepAIC(
      start_model,
      scope = scope_value,
      direction = direction,
      trace = trace_value
    )
  }

  list(
    model = final_model,
    terms = attr(stats::terms(final_model), "term.labels"),
    log = clean_step_log(
      final_model,
      procedure = paste(direction, start, sep = "_")
    ),
    direction = direction,
    start = start
  )
}

clean_step_log <- function(model, procedure) {
  anova_table <- model$anova
  final_terms <- attr(stats::terms(model), "term.labels")
  if (is.null(anova_table)) {
    return(data.frame(
      procedure = procedure,
      iteration = 0,
      action = "modele final",
      terme = NA_character_,
      n_termes_final = length(final_terms),
      AIC = round(stats::AIC(model), 4),
      stringsAsFactors = FALSE
    ))
  }
  step_col <- if ("Step" %in% names(anova_table)) {
    as.character(anova_table$Step)
  } else {
    rep("", nrow(anova_table))
  }
  step_col[is.na(step_col) | step_col == ""] <- "modele de depart"
  action_type <- ifelse(
    grepl("^\\+ ", step_col),
    "ajout",
    ifelse(grepl("^- ", step_col), "retrait", "depart")
  )
  terme <- gsub("^[-+] ", "", step_col)
  terme[action_type == "depart"] <- NA_character_
  aic_col <- if ("AIC" %in% names(anova_table)) {
    anova_table$AIC
  } else {
    rep(NA_real_, nrow(anova_table))
  }
  data.frame(
    procedure = procedure,
    iteration = seq_len(nrow(anova_table)) - 1,
    action = action_type,
    terme = terme,
    n_termes_final = length(final_terms),
    AIC = round(aic_col, 4),
    stringsAsFactors = FALSE
  )
}

compare_step_aic <- function(data, response, terms, family = c("ols", "logit"),
                             verbose = FALSE) {
  family <- match.arg(family)

  configs <- data.frame(
    procedure = c(
      "backward_full",
      "forward_null",
      "stepwise_null",
      "stepwise_full"
    ),
    direction = c("backward", "forward", "both", "both"),
    start = c("full", "null", "null", "full"),
    stringsAsFactors = FALSE
  )

  fits <- vector("list", nrow(configs))

  for (i in seq_len(nrow(configs))) {
    fits[[i]] <- step_aic_select(
      data = data,
      response = response,
      terms = terms,
      family = family,
      direction = configs$direction[i],
      start = configs$start[i],
      verbose = verbose,
      contexte = paste("stepAIC", family, "sur", response)
    )
  }

  comparison <- do.call(rbind, lapply(seq_along(fits), function(i) {
    model_i <- fits[[i]]$model
    terms_i <- fits[[i]]$terms

    out <- data.frame(
      procedure = configs$procedure[i],
      n_termes = length(terms_i),
      p = length(stats::coef(model_i)),
      AIC = round(stats::AIC(model_i), 4),
      formule = paste(deparse(stats::formula(model_i)), collapse = " "),
      stringsAsFactors = FALSE
    )

    if (inherits(model_i, "lm") && !inherits(model_i, "glm")) {
      out$R2 <- round(summary(model_i)$r.squared, 4)
      out$R2_ajuste <- round(summary(model_i)$adj.r.squared, 4)
    }

    out
  }))

  best_idx <- order(comparison$AIC, comparison$p)[1]

  log_all <- do.call(rbind, lapply(seq_along(fits), function(i) {
    fits[[i]]$log
  }))

  list(
    model = fits[[best_idx]]$model,
    terms = fits[[best_idx]]$terms,
    procedure = comparison$procedure[best_idx],
    table = comparison,
    log = log_all,
    all = fits
  )
}

coef_table <- function(model, alpha = 0.05) {
  coef_summary <- summary(model)$coefficients
  ci <- suppressMessages(stats::confint.default(model, level = 1 - alpha))
  stat_name <- if (inherits(model, "glm")) "z_value" else "t_value"

  out <- data.frame(
    estimate = coef_summary[, 1],
    std_error = coef_summary[, 2],
    statistic = coef_summary[, 3],
    p_value = coef_summary[, 4],
    ci_low = ci[, 1],
    ci_high = ci[, 2],
    check.names = FALSE
  )

  names(out)[3] <- stat_name
  round(out, 4)
}

or_table <- function(model, alpha = 0.05) {
  coef_summary <- summary(model)$coefficients
  ci <- suppressMessages(stats::confint.default(model, level = 1 - alpha))

  out <- data.frame(
    estimate = coef_summary[, 1],
    std_error = coef_summary[, 2],
    z_value = coef_summary[, 3],
    p_value = coef_summary[, 4],
    OR = exp(stats::coef(model)),
    OR_ci_low = exp(ci[, 1]),
    OR_ci_high = exp(ci[, 2]),
    check.names = FALSE
  )

  round(out, 4)
}

model_summary <- function(model, name) {
  common_cols <- data.frame(
    modele = name,
    p = length(stats::coef(model)),
    n = stats::nobs(model),
    stringsAsFactors = FALSE
  )

  if (inherits(model, "lm") && !inherits(model, "glm")) {
    model_stats <- summary(model)

    return(cbind(
      common_cols,
      data.frame(
        R2 = round(model_stats$r.squared, 4),
        R2_ajuste = round(model_stats$adj.r.squared, 4),
        AIC = round(stats::AIC(model), 2),
        stringsAsFactors = FALSE
      )
    ))
  }

  cbind(
    common_cols,
    data.frame(AIC = round(stats::AIC(model), 2), stringsAsFactors = FALSE)
  )
}

accuracy <- function(model, data, target = "passed", threshold = 0.5) {
  proba <- stats::predict(model, newdata = data, type = "response")
  pred <- as.integer(proba >= threshold)

  mean(pred == data[[target]])
}

add_rownames_column <- function(x, row_label = "terme") {
  out <- data.frame(x, check.names = FALSE)
  cbind(setNames(data.frame(rownames(out), stringsAsFactors = FALSE), row_label), out)
}

pad_terms <- function(x, n) {
  c(x, rep(NA_character_, n - length(x)))
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

write_csv <- function(x, fname, row.names = FALSE) {
  utils::write.csv(x, file.path(TAB_DIR, fname), row.names = row.names)
}

# =====================================================================
# 5. Modeles lineaires
# =====================================================================

cat("\n[3.4] Estimation du modele lineaire complet\n")

formula_full <- make_formula("G3", TERMS_FULL)
mod_full <- fit_model(formula_full, df, family = "ols")
mod_full_summary <- summary(mod_full)

f_stat <- unname(mod_full_summary$fstatistic["value"])
df_num <- unname(mod_full_summary$fstatistic["numdf"])
df_den <- unname(mod_full_summary$fstatistic["dendf"])
f_pval <- stats::pf(f_stat, df_num, df_den, lower.tail = FALSE)

cat(sprintf("  Test F global : F = %.3f, p = %.3e\n", f_stat, f_pval))
cat(sprintf(
  "  R2 = %.4f | R2_aju = %.4f | AIC = %.2f\n",
  mod_full_summary$r.squared,
  mod_full_summary$adj.r.squared,
  stats::AIC(mod_full)
))

tab_full <- coef_table(mod_full)

test_f_table <- data.frame(
  modele = "Complet avec G1, G2",
  F_statistique = round(f_stat, 4),
  df_model = df_num,
  df_resid = df_den,
  p_valeur = f_pval,
  R2 = round(mod_full_summary$r.squared, 4),
  R2_ajuste = round(mod_full_summary$adj.r.squared, 4),
  AIC = round(stats::AIC(mod_full), 2),
  stringsAsFactors = FALSE
)

cat("\n[3.5] Multicolinearite descriptive\n")
cor_mat <- round(stats::cor(df[, TERMS_QUANT], use = "pairwise.complete.obs"), 3)

cat("\n[3.8] Modele structurel sans G1, G2\n")

formula_hn <- make_formula("G3", TERMS_HORS_NOTES)
mod_full_hn <- fit_model(formula_hn, df, family = "ols")

cat(sprintf(
  "  Complet hors notes : R2 = %.4f | AIC = %.2f\n",
  summary(mod_full_hn)$r.squared,
  stats::AIC(mod_full_hn)
))

# =====================================================================
# 6. Selection
# =====================================================================

cat("\n[3.6] Comparaison des procedures de selection AIC sur M_full\n")

sel <- compare_step_aic(df, "G3", TERMS_FULL, family = "ols", verbose = FALSE)
mod_sel <- sel$model
terms_sel <- sel$terms
log_sel <- sel$log
table_sel_procedures <- sel$table

cat(sprintf(
  "  M_sel : procedure retenue = %s | %d termes | AIC = %.2f\n",
  sel$procedure,
  length(terms_sel),
  stats::AIC(mod_sel)
))

cat("\n[3.6.1] Comparaison des procedures de selection AIC sans G1\n")

sel_sans_g1 <- compare_step_aic(df, "G3", TERMS_SANS_G1, family = "ols", verbose = FALSE)
mod_sel_sans_g1 <- sel_sans_g1$model
terms_sel_sans_g1 <- sel_sans_g1$terms
log_sel_sans_g1 <- sel_sans_g1$log
table_sel_sans_g1_procedures <- sel_sans_g1$table

cat(sprintf(
  "  Selectionne sans G1 : procedure retenue = %s | %d termes | AIC = %.2f\n",
  sel_sans_g1$procedure,
  length(terms_sel_sans_g1),
  stats::AIC(mod_sel_sans_g1)
))

cat("\n[3.6.2] Comparaison des procedures de selection AIC sans G2\n")

sel_sans_g2 <- compare_step_aic(df, "G3", TERMS_SANS_G2, family = "ols", verbose = FALSE)
mod_sel_sans_g2 <- sel_sans_g2$model
terms_sel_sans_g2 <- sel_sans_g2$terms
log_sel_sans_g2 <- sel_sans_g2$log
table_sel_sans_g2_procedures <- sel_sans_g2$table

cat(sprintf(
  "  Selectionne sans G2 : procedure retenue = %s | %d termes | AIC = %.2f\n",
  sel_sans_g2$procedure,
  length(terms_sel_sans_g2),
  stats::AIC(mod_sel_sans_g2)
))

cat("\n[3.8] Comparaison des procedures de selection AIC hors G1, G2\n")

sel_hn <- compare_step_aic(df, "G3", TERMS_HORS_NOTES, family = "ols", verbose = FALSE)
mod_sel_hn <- sel_hn$model
terms_sel_hn <- sel_hn$terms
log_sel_hn <- sel_hn$log
table_sel_hn_procedures <- sel_hn$table

cat(sprintf(
  "  Selectionne hors notes : procedure retenue = %s | %d termes | AIC = %.2f\n",
  sel_hn$procedure,
  length(terms_sel_hn),
  stats::AIC(mod_sel_hn)
))

cat("\n[3.6.3] Tableau comparatif des six modeles lineaires\n")

table_lin <- do.call(rbind, list(
  model_summary(mod_full, "Complet avec G1, G2"),
  model_summary(mod_sel, paste0("Selectionne avec G1, G2 - ", sel$procedure)),
  model_summary(mod_sel_sans_g1, paste0("Selectionne sans G1 - ", sel_sans_g1$procedure)),
  model_summary(mod_sel_sans_g2, paste0("Selectionne sans G2 - ", sel_sans_g2$procedure)),
  model_summary(mod_full_hn, "Complet sans G1, G2"),
  model_summary(mod_sel_hn, paste0("Selectionne sans G1, G2 - ", sel_hn$procedure))
))

print(table_lin, row.names = FALSE)

# =====================================================================
# 7. Diagnostics
# =====================================================================

cat("\n[3.7] Diagnostics de M_sel\n")

residus <- stats::residuals(mod_sel)
ajustes <- stats::fitted(mod_sel)
shapiro <- stats::shapiro.test(residus)

cat(sprintf(
  "  Shapiro-Wilk : W = %.4f, p = %.3e\n",
  shapiro$statistic,
  shapiro$p.value
))

shapiro_table <- data.frame(
  modele = "M_sel avec G1, G2",
  test = "Shapiro-Wilk",
  W = round(unname(shapiro$statistic), 4),
  p_valeur = shapiro$p.value,
  n = length(residus),
  stringsAsFactors = FALSE
)

diag_df <- data.frame(
  obs_id = seq_along(residus),
  G3 = df$G3,
  G3_ajuste = as.numeric(ajustes),
  residu = as.numeric(residus),
  passed = df$passed
)
diag_df[, c("G3_ajuste", "residu")] <- round(diag_df[, c("G3_ajuste", "residu")], 4)

mask_g3_zero <- df$G3 == 0
diag_g3_zero <- diag_df[mask_g3_zero, ]

cat(sprintf(
  "  Observations a G3=0 : %d cas, residu moyen = %.3f\n",
  sum(mask_g3_zero),
  mean(diag_g3_zero$residu)
))

plot_residus <- ggplot2::ggplot(diag_df, ggplot2::aes(x = G3_ajuste, y = residu)) +
  ggplot2::geom_point(color = "black", alpha = 0.55, size = 1.5) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
  ggplot2::labs(
    x = "Valeurs ajustees G3_hat",
    y = "Residus e_i",
    title = "Residus vs valeurs ajustees - modele lineaire selectionne"
  ) +
  theme_report()

qq_df <- data.frame(
  theorique = stats::qnorm(stats::ppoints(length(residus))),
  observe = sort(residus)
)

qq_quartiles <- stats::quantile(residus, probs = c(0.25, 0.75), names = FALSE)
qq_theorique <- stats::qnorm(c(0.25, 0.75))
qq_slope <- diff(qq_quartiles) / diff(qq_theorique)
qq_intercept <- qq_quartiles[1] - qq_slope * qq_theorique[1]

plot_qq <- ggplot2::ggplot(qq_df, ggplot2::aes(x = theorique, y = observe)) +
  ggplot2::geom_point(color = "black", alpha = 0.75, size = 1.4) +
  ggplot2::geom_abline(
    intercept = qq_intercept,
    slope = qq_slope,
    color = "red",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  ggplot2::labs(
    x = "Quantiles theoriques",
    y = "Quantiles observes",
    title = "QQ-plot des residus - modele lineaire selectionne"
  ) +
  theme_report()

plot_hist_residus <- ggplot2::ggplot(diag_df, ggplot2::aes(x = residu)) +
  ggplot2::geom_histogram(bins = 30, fill = "gray85", color = "black", linewidth = 0.25) +
  ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
  ggplot2::labs(
    x = "Residus e_i",
    y = "Frequence",
    title = "Distribution des residus - modele lineaire selectionne"
  ) +
  theme_report()

# =====================================================================
# 8. Logistique
# =====================================================================

cat("\n[3.9] Regression logistique pass/fail\n")

formula_logit_full <- make_formula("passed", TERMS_FULL)
mod_logit_full <- fit_model(
  formula_logit_full, df, family = "logit",
  contexte = "logit complet (M_logit_full)"
)

cat(sprintf("  Logit complet : AIC = %.2f\n", stats::AIC(mod_logit_full)))

sel_logit <- compare_step_aic(df, "passed", TERMS_FULL, family = "logit", verbose = FALSE)
mod_logit_sel <- sel_logit$model
terms_logit_sel <- sel_logit$terms
log_logit_sel <- sel_logit$log
table_logit_procedures <- sel_logit$table

cat(sprintf(
  "  Logit selectionne : procedure retenue = %s | %d termes | AIC = %.2f\n",
  sel_logit$procedure,
  length(terms_logit_sel),
  stats::AIC(mod_logit_sel)
))

acc_full <- accuracy(mod_logit_full, df)
acc_sel <- accuracy(mod_logit_sel, df)

cat(sprintf("  Accuracy complet : %.4f\n", acc_full))
cat(sprintf("  Accuracy selectionne : %.4f\n", acc_sel))

proba_sel <- stats::predict(mod_logit_sel, newdata = df, type = "response")
pred_sel <- as.integer(proba_sel >= 0.5)
cm <- addmargins(table(Reel = df$passed, Predit = pred_sel))

print(cm)

table_logit <- data.frame(
  modele = c("Complet", "Selectionne"),
  p = c(length(stats::coef(mod_logit_full)), length(stats::coef(mod_logit_sel))),
  AIC = c(round(stats::AIC(mod_logit_full), 2), round(stats::AIC(mod_logit_sel), 2)),
  taux_classification_correct = c(round(acc_full, 4), round(acc_sel, 4)),
  stringsAsFactors = FALSE
)

# =====================================================================
# 9. Exports
# =====================================================================

cat("\n[3.10] Synthese finale et exports\n")

synthese <- data.frame(
  modele = c(
    "Lineaire selectionne avec G1, G2",
    "Lineaire selectionne sans G1",
    "Lineaire selectionne sans G2",
    "Lineaire selectionne sans G1, G2",
    "Logistique selectionnee"
  ),
  p = c(
    length(stats::coef(mod_sel)),
    length(stats::coef(mod_sel_sans_g1)),
    length(stats::coef(mod_sel_sans_g2)),
    length(stats::coef(mod_sel_hn)),
    length(stats::coef(mod_logit_sel))
  ),
  metrique_1_nom = c("R2_ajuste", "R2_ajuste", "R2_ajuste", "R2_ajuste", "Accuracy"),
  metrique_1_valeur = c(
    round(summary(mod_sel)$adj.r.squared, 4),
    round(summary(mod_sel_sans_g1)$adj.r.squared, 4),
    round(summary(mod_sel_sans_g2)$adj.r.squared, 4),
    round(summary(mod_sel_hn)$adj.r.squared, 4),
    round(acc_sel, 4)
  ),
  metrique_2_nom = c("AIC", "AIC", "AIC", "AIC", "AIC"),
  metrique_2_valeur = c(
    round(stats::AIC(mod_sel), 2),
    round(stats::AIC(mod_sel_sans_g1), 2),
    round(stats::AIC(mod_sel_sans_g2), 2),
    round(stats::AIC(mod_sel_hn), 2),
    round(stats::AIC(mod_logit_sel), 2)
  ),
  note = c(
    "reference predictive",
    "sensibilite sans G1",
    "sensibilite sans G2",
    "facteurs amont",
    "classification pass/fail"
  ),
  stringsAsFactors = FALSE
)

max_len <- max(
  length(terms_sel),
  length(terms_sel_sans_g1),
  length(terms_sel_sans_g2),
  length(terms_sel_hn),
  length(terms_logit_sel)
)

terms_summary <- data.frame(
  modele_lineaire_avec_G1_G2 = pad_terms(terms_sel, max_len),
  modele_lineaire_sans_G1 = pad_terms(terms_sel_sans_g1, max_len),
  modele_lineaire_sans_G2 = pad_terms(terms_sel_sans_g2, max_len),
  modele_lineaire_sans_G1_G2 = pad_terms(terms_sel_hn, max_len),
  modele_logistique_selectionne = pad_terms(terms_logit_sel, max_len),
  stringsAsFactors = FALSE
)

dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

write_csv(add_rownames_column(tab_full, "terme"), "3.4_coefficients_modele_complet.csv")
write_csv(test_f_table, "3.4_test_F_global.csv")
write_csv(cor_mat, "3.5_correlation_matrix_quanti.csv", row.names = TRUE)

write_csv(
  add_rownames_column(coef_table(mod_sel), "terme"),
  "3.6_coefficients_modele_selectionne.csv"
)
write_csv(table_sel_procedures, "3.6_comparaison_procedures_AIC_avec_notes.csv")
write_csv(log_sel, "3.6_log_selection_AIC_avec_notes.csv")

write_csv(
  add_rownames_column(coef_table(mod_sel_sans_g1), "terme"),
  "3.6.1_coefficients_modele_selectionne_sans_G1.csv"
)
write_csv(table_sel_sans_g1_procedures, "3.6.1_comparaison_procedures_AIC_sans_G1.csv")
write_csv(log_sel_sans_g1, "3.6.1_log_selection_AIC_sans_G1.csv")

write_csv(
  add_rownames_column(coef_table(mod_sel_sans_g2), "terme"),
  "3.6.2_coefficients_modele_selectionne_sans_G2.csv"
)
write_csv(table_sel_sans_g2_procedures, "3.6.2_comparaison_procedures_AIC_sans_G2.csv")
write_csv(log_sel_sans_g2, "3.6.2_log_selection_AIC_sans_G2.csv")

write_csv(table_lin, "3.6.3_comparaison_modeles_lineaires.csv")

write_csv(shapiro_table, "3.7_test_normalite_shapiro.csv")
write_csv(diag_df, "3.7_residus_et_valeurs_ajustees.csv")
write_csv(diag_g3_zero, "3.7_observations_G3_zero.csv")

write_csv(
  add_rownames_column(coef_table(mod_full_hn), "terme"),
  "3.8_coefficients_modele_complet_horsG12.csv"
)
write_csv(
  add_rownames_column(coef_table(mod_sel_hn), "terme"),
  "3.8_coefficients_modele_structurel.csv"
)
write_csv(table_sel_hn_procedures, "3.8_comparaison_procedures_AIC_horsG12.csv")
write_csv(log_sel_hn, "3.8_log_selection_AIC_horsG12.csv")

write_csv(
  add_rownames_column(or_table(mod_logit_full), "terme"),
  "3.9_odds_ratios_modele_complet.csv"
)
write_csv(
  add_rownames_column(or_table(mod_logit_sel), "terme"),
  "3.9_odds_ratios_modele_selectionne.csv"
)
write_csv(table_logit_procedures, "3.9_comparaison_procedures_AIC_logistique.csv")
write_csv(log_logit_sel, "3.9_log_selection_AIC_logistique.csv")
write_csv(as.data.frame.matrix(cm), "3.9_matrice_confusion_modele_selectionne.csv",
          row.names = TRUE)
write_csv(table_logit, "3.9_comparaison_modeles_logistiques.csv")

write_csv(synthese, "3.10_synthese_modeles_retenus.csv")
write_csv(terms_summary, "3.10_termes_retenus_par_modele.csv")

save_pdf_figure("3.7_residus_vs_ajustes.pdf", plot_residus)
save_pdf_figure("3.7_qqplot_residus.pdf", plot_qq)
save_pdf_figure("3.7_histogramme_residus.pdf", plot_hist_residus)

print(synthese, row.names = FALSE)

cat("\n[OK] Pipeline termine. Outputs dans : ", normalizePath(OUT_DIR), "\n", sep = "")
cat(sprintf("  %d tableaux CSV dans %s\n", length(list.files(TAB_DIR, pattern = "\\.csv$")), TAB_DIR))
cat(sprintf("  %d figures PDF dans %s\n", length(list.files(FIG_DIR, pattern = "\\.pdf$")), FIG_DIR))
