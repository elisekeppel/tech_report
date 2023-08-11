# Distance function summarizing ds models fails when one model has average.p of 1 and therefore summ$ds$average.p.se is NULL
# Must change to NA to continue with function

########################

summarize_ds_models <- function (..., sort = "AIC", output = "latex", delta_only = TRUE)
{
  models <- list(...)
  model_names <- setdiff(as.character(match.call(expand.dots = TRUE)),
                         as.character(match.call(expand.dots = FALSE)))
  r_truncs <- unlist(lapply(models, function(x) x$ddf$meta.data$width))
  l_truncs <- unlist(lapply(models, function(x) x$ddf$meta.data$left))
  if (!all(abs(c(r_truncs - mean(r_truncs), l_truncs - mean(l_truncs))) <
           1e-08)) {
    stop("All truncations must be the same for AIC comparison.")
  }
  binned <- unlist(lapply(models, function(x) x$ddf$meta.data$binned))
  if ((any(binned) & !all(binned)) | (any(!binned) & !all(!binned))) {
    stop("Can't compare binned and unbinned distances")
  }
  if (all(binned)) {
    breaks <- lapply(models, function(x) x$ddf$meta.data$breaks)
    len_breaks <- unlist(lapply(breaks, length))
    if (!all(abs(len_breaks - mean(len_breaks)) < 1e-08)) {
      stop("Distance binning must be the same for all models.")
    }
    for (i in seq_along(breaks[[1]])) {
      this_set <- unlist(lapply(breaks, "[[", i))
      if (!all(abs(this_set - mean(this_set)) < 1e-08)) {
        stop("Distance binning must be the same for all models.")
      }
    }
  }
  extract_model_data <- function(model) {
    summ <- summary(model)
    formula <- model$ddf$ds$aux$ddfobj$scale$formula
    if (is.null(formula))
      formula <- NA
    desc <- gsub(" key function", "", Distance:::model.description(model$ddf))
    if (model$ddf$meta.data$binned) {
      gof <- suppressMessages(gof_ds(model, chisq = TRUE)$chisquare$chi1$p)
    }
    else {
      gof <- suppressMessages(ddf.gof(model$ddf, qq = FALSE)$dsgof$CvM$p)
    }
    ######################
    if(is.null(summ$ds$average.p.se))       summ$ds$average.p.se <- NA
    ######################
    ret <- c(desc, formula, gof, summ$ds$average.p, summ$ds$average.p.se,
             model$ddf$criterion)
    return(ret)
  }
  res <- as.data.frame(t(as.data.frame(lapply(models, extract_model_data))),
                       stringsAsFactors = FALSE)
  if (output == "latex") {
    model_names <- gsub("_", "\\\\char`_", model_names)
    model_names <- paste0("\\texttt{", model_names,
                          "}")
    res <- cbind.data.frame(model_names, res)
  }
  else if (output == "plain") {
    res <- cbind.data.frame(model_names, res)
  }
  else {
    stop("Invalid output format")
  }
  res[, 4:7] <- apply(res[, 4:7], 2, as.numeric)
  if (all(binned)) {
    gof_name <- "Chi^2 p-value"
    gof_latexname <- "$\\chi^2$ $p$-value"
  }
  else {
    gof_name <- "C-vM $p$-value"
    gof_latexname <- "C-vM p-value"
  }
  if (output == "latex") {
    colnames(res) <- c("Model", "Key function",
                       "Formula", gof_latexname, "$\\hat{P_a}$",
                       "se($\\hat{P_a}$)", "AIC")
  }
  else if (output == "plain") {
    colnames(res) <- c("Model", "Key function",
                       "Formula", gof_name, "Average detectability",
                       "se(Average detectability)", "AIC")
  }
  else {
    stop("Invalid output format")
  }
  rownames(res) <- NULL
  if (output == "latex") {
    res[["$\\Delta$AIC"]] <- res$AIC - min(res$AIC,
                                           na.rm = TRUE)
  }
  else if (output == "plain") {
    res[["Delta AIC"]] <- res$AIC - min(res$AIC, na.rm = TRUE)
  }
  res <- res[order(res[[sort]]), ]
  if (delta_only) {
    res$AIC <- NULL
  }
  return(res)
}
