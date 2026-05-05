# =====================================================================
# SaveFigure - Sauvegarde standardisee de figures R
# Supporte ggplot, patchwork, recordedplot et fonctions de tracage base R.
# Formats supportes : pdf, png, svg, jpg, jpeg, tiff.
# =====================================================================

SaveFigure <- function(plot_object,
                       fname,
                       folder = "Image",
                       dpi = 600,
                       width_cm = 15,
                       height_cm = width_cm * 0.75,
                       apply_theme = TRUE,
                       font_family = "serif") {
  # Validation minimale
  if (missing(plot_object)) {
    stop("Argument 'plot_object' manquant.")
  }

  if (missing(fname) || !is.character(fname) || length(fname) != 1) {
    stop("Argument 'fname' doit etre une chaine de caracteres de longueur 1.")
  }

  if (!is.numeric(width_cm) || width_cm <= 0) {
    stop("Argument 'width_cm' doit etre strictement positif.")
  }

  if (!is.numeric(height_cm) || height_cm <= 0) {
    stop("Argument 'height_cm' doit etre strictement positif.")
  }

  if (!is.numeric(dpi) || dpi <= 0) {
    stop("Argument 'dpi' doit etre strictement positif.")
  }

  # Creation du dossier de sortie
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  # Nettoyage du nom de fichier
  # Les deux-points sont evites pour compatibilite Windows.
  clean_base <- gsub("[^a-zA-Z0-9_.-]", "_", basename(fname))
  clean_name <- file.path(folder, clean_base)

  # Extension
  ext <- tolower(tools::file_ext(clean_name))

  if (ext == "") {
    stop("Le nom de fichier doit contenir une extension : pdf, png, svg, jpg, jpeg ou tiff.")
  }

  # Dimensions en pouces
  w_in <- width_cm / 2.54
  h_in <- height_cm / 2.54

  # Detection du type de graphique
  is_gg <- inherits(plot_object, c("gg", "ggplot", "patchwork", "ggExtraPlot"))
  is_recorded <- inherits(plot_object, "recordedplot")
  is_function <- is.function(plot_object)

  # Theme ggplot optionnel
  if (is_gg && isTRUE(apply_theme)) {
    plot_object <- plot_object +
      ggplot2::theme_bw(base_size = 10, base_family = font_family)
  } else if (is_gg && inherits(apply_theme, "theme")) {
    plot_object <- plot_object + apply_theme
  }

  # Ouverture du device graphique
  if (ext == "pdf") {
    grDevices::cairo_pdf(clean_name, width = w_in, height = h_in,
                         family = font_family)
  } else if (ext == "png") {
    grDevices::png(clean_name, width = w_in, height = h_in,
                   units = "in", res = dpi)
  } else if (ext == "svg") {
    grDevices::svg(clean_name, width = w_in, height = h_in,
                   family = font_family)
  } else if (ext %in% c("jpg", "jpeg")) {
    grDevices::jpeg(clean_name, width = w_in, height = h_in,
                    units = "in", res = dpi)
  } else if (ext == "tiff") {
    grDevices::tiff(clean_name, width = w_in, height = h_in,
                    units = "in", res = dpi)
  } else {
    stop("Extension non supportee : ", ext)
  }

  # Fermeture garantie uniquement apres ouverture du device
  on.exit(grDevices::dev.off(), add = TRUE)

  # Rendu du graphique
  if (is_gg) {
    print(plot_object)
  } else if (is_recorded) {
    grDevices::replayPlot(plot_object)
  } else if (is_function) {
    plot_object()
  } else {
    stop(
      "Type de graphique non supporte. ",
      "Utiliser un ggplot, un recordedplot, ou une fonction qui trace le graphique."
    )
  }

  message("[SaveFigure] Sauvegarde de : ", clean_name)
  invisible(clean_name)
}