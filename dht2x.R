# dht2x <- function(ddf,
#                   observations=NULL
#                   transects=NULL
#                   geo_strat=NULL
#                  flatfile=NULL
#                  strat_formula
#                  convert_units=1
#                  er_est=c("R2", "P2")
#                  multipliers=NULL
#                  sample_fraction=1
#                  ci_width=0.95
#                  innes=FALSE
#                  stratification="geographical"
#                  total_area=NULL
#                  binomial_var=FALSE){




  ddf = hw1.75.hn.bfc.szc2 # ds outpu
  observations = NULL
  transects = NULL
  geo_strat = NULL
  flatfile = hw_sp
  # strat_formula = ~month
  strat_formula = ~year
  convert_units = 1
  er_est = "O2"
  multipliers = NULL
  sample_fraction = 1
  ci_width = 0.95
  innes = T
  stratification = "replicate"
  total_area = NULL
  binomial_var = FALSE

  # ddf = hw1.5.hn.v # ds outpu
  # observations = NULL
  # transects = NULL
  # geo_strat = NULL
  # flatfile = f
  # strat_formula = ~SurveyID
  # convert_units = 1
  # er_est = "O2"
  # multipliers = NULL
  # sample_fraction = 1
  # ci_width = 0.95
  # innes = FALSE
  # stratification = "replicate"
  # total_area = NULL
  # binomial_var = FALSE



    # get default variance estimation
  if(missing(er_est)){
    attr(er_est, "missing") <- TRUE
  }else{
    attr(er_est, "missing") <- FALSE
  }

  # check we have a valid stratification option
  if(!(stratification %in% c("geographical", "object",
                             "effort_sum", "replicate"))){
    stop("'stratification' must be one of: \"geographical\", \"object\", \"effort_sum\" or \"replicate\"")
  }

  # check ci width
  if(ci_width > 1 | ci_width < 0){
    stop("ci_width must be between 0 and 1!")
  }else{
    ci_width <- (1-ci_width)/2
  }

  # can't do both innes and binomial var
  if(innes & binomial_var){
    stop("Only 'innes' or 'binomial_var' may be TRUE")
  }


  # what are the stratum labels specicied in strat_formula?
  stratum_labels <- attr(terms(strat_formula), "term.labels")

  # TODO: currently break if >1 stratum is defined
  # https://github.com/DistanceDevelopment/Distance/issues/46
  if(length(stratum_labels) > 1){
    stop("Only one level of stratification is currently supported")
  }

  # process multiple detection functions
  ddf_proc <- Distance:::dht2_process_ddf(ddf, convert_units, er_est, strat_formula)
  bigdat <- ddf_proc$bigdat
  ddf <- ddf_proc$ddf
  transect_data <- ddf_proc$transect_data

  # we don't support effort sum or replicate for multiddf
  if((length(ddf) > 1) &
     (stratification %in% c("effort_sum", "replicate"))){
    stop("Effort-sum and replicate stratification not supported with multiple detection functions")
  }

  # grouped estimation
  # time to recurse
  if(!ddf_proc$groupsizeone){
    mc <- match.call(expand.dots = FALSE)
    dddf <- ddf
    dddf <- lapply(dddf, \(x){x$data$size <- 1;x})
    mc$ddf <- dddf
    if(!is.null(flatfile)){
      ff <- flatfile
      ff$size <- 1
      mc$flatfile <- ff
    }
    grouped <- eval.parent(mc)
    rm(dddf, mc)
  }else{
    grouped <- NULL
  }
  # ^^ we'll save this output later on

  if(!is.null(observations) & !is.null(transects)){
    if(!is.null(geo_strat)){
      # what if there were as.factor()s in the formula?
      geo_strat <- Distance:::safe_factorize(strat_formula, geo_strat)
      # which occur at the geo level?
      geo_stratum_labels <- stratum_labels[stratum_labels %in%
                                             colnames(geo_strat)]
    }else{
      geo_stratum_labels <- NULL
    }

    # what if there were as.factor()s in the formula?
    transects <- Distance:::safe_factorize(strat_formula, transects)
    observations <- Distance:::safe_factorize(strat_formula, observations)

    # TODO: do some data checking at this point
    # - check duplicate column names (e.g., obs$sex and df$data$sex)
    # - check column names don't conflict with columns created below
    #    - list of protected column names that can't be in the data
    #         protected <- c("p", ".Label", )

    # check the data
    dht2_checkdata(colnames(bigdat), observations, transects, geo_strat,
                   strat_formula, stratum_labels, geo_stratum_labels)

    # drop unused levels of factors
    observations <- droplevels(observations)
    transects <- droplevels(transects)
    geo_strat <- droplevels(geo_strat)

    # only keep observations within the truncation
    observations <- observations[observations$object %in% ddf_proc$obj_keep, ]

    # merge onto the observation frame
    bigdat <- merge(bigdat, observations, all.x=TRUE, by="object",
                    suffixes=c("DUPLICATE", ""))

    # remove column duplicates
    if(any(grepl("DUPLICATE", names(bigdat)))){
      bigdat[, grepl("DUPLICATE", names(bigdat))] <- NULL
    }

    # handle when there is only one detection function
    if(length(ddf) == 1){
      transects$ddf_id <- 1
    }
    # merge transect type onto the sample table
    transects <- merge(transects, transect_data, all.x=TRUE, by="ddf_id")

    # merge onto transects
    join_labs <- intersect(names(bigdat), names(transects))
    #join_labs <- join_labs[c("Sample.Label", geo_stratum_labels) %in% join_labs]
    bigdat <- merge(bigdat, transects, all.x=TRUE, all.y=TRUE,
                    by=join_labs,
                    suffixes=c("DUPLICATE", ""))
    # remove Sample.Label dupes
    if(any(grepl("DUPLICATE", names(bigdat)))){
      bigdat[, grepl("DUPLICATE", names(bigdat))] <- NULL
    }
    if(stratification=="object"){
      # what are all the possible combinations of obs level stratum
      # levels and sample labels?
      ex <- expand.grid(lapply(bigdat[, c("Sample.Label", stratum_labels)],
                               function(x) unique(na.omit(x))),
                        stringsAsFactors=FALSE)
      # which are not represented in the data?
      aj <- anti_join(ex, bigdat, by=c("Sample.Label", stratum_labels))
      # join the unrepresented sample combinations to the extra cols
      # (i.e., add Area, Effort data to aj)
      aj <- left_join(aj, bigdat,
                      by=c(stratum_labels))

      # remove the transects with no stratum data
      bigdat2 <- filter_at(bigdat, stratum_labels, function(x) !is.na(x))
      aj <- filter_at(aj, stratum_labels, function(x) !is.na(x))

      # rbind that onto the original data
      bigdat <- bind_rows(bigdat2, aj)
    }

    # merge on the geographical strata
    if(!is.null(geo_strat)){
      # if we have ~1 we ignore stratification
      if(strat_formula==~1){
        geo_strat$Area <- sum(geo_strat$Area)
        geo_strat$.Label <- "Total"
        geo_strat <- unique(geo_strat[, c(".Label", "Area")])

        bigdat$.Label <- "Total"
        stratum_labels <- ".Label"
        geo_stratum_labels <- ".Label"
      }
      # TODO: do something here with strat_formula
      bigdat <- merge(bigdat, geo_strat, all.x=TRUE)#, by=c(geo_stratum_labels, "Area"))
    }else{
      bigdat$Area <- NA
      bigdat$Label <- "Total"
      stratum_labels <- c("Label", stratum_labels)
    }
  }else if(!is.null(flatfile)){
    # if we have a flatfile
    # TODO: check flatfile format here
    # should this just run Distance:::checkdata(flatfile)?

    # make a dummy distance column for use later on
    # this overwrites the column that's there BUT that's okay
    # since we need to make sure it's consistent with the bins
    if(is.null(flatfile$distance)){
      if(!all((c("distend", "distbegin") %in% names(flatfile)))){
        stop("flatfile must include columns named either 'distance' or 'distbegin' and 'distend'")
      }
      flatfile$distance <- (flatfile$distend+flatfile$distbegin)/2
    }

    # labels needed for flatfile
    flatfile_labels <- c("distance", "Sample.Label", "Effort", "Area", "object")

    # check we have a ddf_id column
    if(length(ddf) == 1){
      flatfile$ddf_id <- 1
    }else{
      if(is.null(flatfile$ddf_id) ||
         !all(1:length(ddf) %in% unique(flatfile$ddf_id))){
        stop("flatfile must include ddf_id column to identify samples to detection functions")
      }
    }

    # check regular columns exist
    if(!all(flatfile_labels %in% names(flatfile))){
      stop(paste("Missing column(s) in `flatfile`:",
                 paste(flatfile_labels[!(flatfile_labels %in% names(flatfile))],
                       collapse=", "),
                 ". See ?dht2 for more information."), call. = FALSE)
    }

    # join the extra ddf data onto the flatfile
    flatfiles_per_ddf <- list()
    for(i in seq_along(ddf)){
      # get this ddfs rows
      this_flatfile <- flatfile[flatfile$ddf_id == i, ]

      # get the detected objects that we want to keep
      # first get the detections
      flatfiles_per_ddf[[i]] <- this_flatfile[!is.na(this_flatfile$object), , drop=FALSE]

      # need to check if the truncation will drop any samples
      dropped <- flatfiles_per_ddf[[i]][!(flatfiles_per_ddf[[i]]$object %in%
                                            ddf_proc$obj_keep), ,drop=FALSE]

      # keep observations inside truncation
      flatfiles_per_ddf[[i]] <- flatfiles_per_ddf[[i]][
        (flatfiles_per_ddf[[i]]$object %in%
           ddf_proc$obj_keep), ,drop=FALSE]

      # get the samples that didn't have observations
      if(nrow(dropped)>0){
        dropped$distance <- NA
        dropped$object <- NA
        dropped$size <- NA # EK edit: suggest adding this line to remove size which could be propogated through analysis
        dropped <- dropped[!duplicated(dropped$Sample.Label), , drop=FALSE]
        dropped <- dropped[!(dropped$Sample.Label %in% flatfiles_per_ddf[[i]]$Sample.Label),]
      }

      # bind together the ...            VV observations inside the truncation
      flatfiles_per_ddf[[i]] <- rbind(flatfiles_per_ddf[[i]],
                                      # any transects dropped by the trunc
                                      dropped,
                                      # samples without observations
                                      this_flatfile[is.na(this_flatfile$object),
                                                    , drop=FALSE])

      # join p
      flatfiles_per_ddf[[i]] <- left_join(flatfiles_per_ddf[[i]],
                                          ddf_proc$bigdat[!is.na(ddf_proc$bigdat$object),
                                                          c("object", "p")],
                                          by="object")
      # join ddf info
      flatfiles_per_ddf[[i]] <- left_join(flatfiles_per_ddf[[i]],transect_data,
                                          by="ddf_id")
    }
    # stick it together
    #bigdat <- unique(do.call(rbind, flatfiles_per_ddf))
    bigdat <- do.call(rbind, flatfiles_per_ddf)

    # ensure as.factor in formula are propagated to the data
    bigdat <- Distance:::safe_factorize(strat_formula, bigdat)

    # check strat columns are in the data
    if(!all(stratum_labels %in% names(bigdat))){
      stop(paste("Column(s):",
                 paste(stratum_labels[!(stratum_labels %in% names(bigdat))],
                       collapse=", "),
                 "not in `flatfile`"))
    }

    # make object column if not present
    if(is.null(bigdat$object)){
      # ensure that there isn't a size in the data if this is a
      # placeholder row for a sample unit
      bigdat$object <- NA
      bigdat$object[!is.na(bigdat$distance)] <- 1:sum(!is.na(bigdat$distance))
    }else if(!all(is.na(bigdat$distance) == is.na(bigdat$object))){
      stop("NAs in distance column do not match those in the object column, check data")
    }
    # sort by object ID
    bigdat <- bigdat[order(bigdat$object), ]

    if(strat_formula==~1){
      bigdat$Area <- sum(unique(bigdat[, c("Area", "Region.Label")])$Area)
      bigdat$Region.Label <- NULL
      bigdat$.Label <- "Total"

      stratum_labels <- ".Label"
      geo_stratum_labels <- ".Label"
    }else{
      geo_stratum_labels <- c()
    }

    # TODO: check when implementing multiple stratification
    # https://github.com/DistanceDevelopment/Distance/issues/46
    if(stratification=="object"){
      # what are all the possible combinations of obs level stratum
      # levels and sample labels?
      ex <- expand.grid(lapply(bigdat[, c("Sample.Label", stratum_labels)],
                               function(x) unique(na.omit(x))),
                        stringsAsFactors=FALSE)
      # which are not represented in the data?
      aj <- anti_join(ex, bigdat, by=c("Sample.Label", stratum_labels))
      # join the unrepresented sample combinations to the extra cols
      # (i.e., add Area, Effort data to aj)
      aj <- left_join(aj, unique(bigdat[, c("Sample.Label", "Effort", "Area",
                                            "ddf_id", "transect_type", "er_est",
                                            "df_width", "df_left", "n_ddf",
                                            "n_par")]),
                      by="Sample.Label")

      # remove the transects with no stratum data
      bigdat2 <- filter_at(bigdat, stratum_labels, function(x) !is.na(x))

      # rbind that onto the original data
      bigdat <- bind_rows(bigdat2, aj)
    }

    # TODO: this needs to be checked for the multi-strata case
    # https://github.com/DistanceDevelopment/Distance/issues/46
    # check that Area-stratum combinations are coherent
    ucomb <- unique(bigdat[, c("Area", stratum_labels)])
    if(length(na.omit(ucomb[,stratum_labels])) >
       length(na.omit(unique(bigdat[,stratum_labels])))){
      stop(">1 Area defined for a single stratum label, check data")
    }
  }else{ # end flatfile processing
    stop("Need to supply either observations, transects and geo_strat OR flatfile, see the \"Data\" section of ?dht2")
  }

  # stop if any of the transects has zero or negative effort
  if(any(is.na(bigdat[["Effort"]])) || any(bigdat[["Effort"]] <= 0)){
    stop("Some transects have Effort <=0 or NA")
  }

  # handle multipliers
  bigdat <- Distance:::dht2_multipliers(multipliers, bigdat)
  mult <- TRUE
  if(attr(bigdat, "multipliers")) mult <- TRUE else mult <- FALSE

  # make group size 1 if not in the data
  if(is.null(bigdat$size)){
    # ensure that there isn't a size in the data if this is a
    # placeholder row for a sample unit
    bigdat$size <- NA
    bigdat$size[!is.na(bigdat$distance)] <- 1
  }
  # make object column if not present
  if(is.null(bigdat$object)){
    # ensure that there isn't a size in the data if this is a
    # placeholder row for a sample unit
    bigdat$object <- NA
    bigdat$object[!is.na(bigdat$distance)] <- 1:sum(!is.na(bigdat$distance))
  }else if(!all(is.na(bigdat$distance) == is.na(bigdat$object))){
    stop("NAs in distance column do not match those in the object column, check data")
  }

  # Horvitz-Thompson-corrected per-sighting counts
  bigdat$Nhat <- bigdat$size/bigdat$p

  # handle sample fractions
  bigdat <- Distance:::dht2_sample_fraction(sample_fraction, bigdat)

  # TODO: clean-up the data, removing stratum labels with zero observations
  #       or NA labels (and warning)

  # dplyr cheatsheet:
  # - group_by : subsequent commands operate per group
  # - mutate   : adds a new column
  # - distinct : select only the unique row combinations
  # - select   : retain only these columns
  # - .data$   : get the column from the current data, don't go to global env
  # note that this all turns out to be non-standard dplyr code because
  # you can't have unquoted variable names (NSE) in CRAN-submitted packages
  # see https://dplyr.tidyverse.org/articles/programming.html for "why"s
  # so we use rlang::.data to get around this

  # first do transect level calculations
  res <- bigdat %>%
    group_by_at(vars("Sample.Label"))

  # make sure summaries are made at the sample-stratum level here
  # (since sample labels may be duplicated between strata)
  res <- res %>%
    group_by_at(.vars=stratum_labels, .add=TRUE)

  res <- res %>%
    # *observations* per transect
    mutate(transect_n = sum(.data$size, na.rm=TRUE),
           transect_n_observations = length(na.omit(unique(.data$object))),
           # abundance estimate per transect in covered area
           transect_Nc = sum(.data$Nhat, na.rm=TRUE)) %>%    # EK: how is this related to covered area? it looks like just transect abundance est, accounting for detec func?
    ungroup()
  # EK : so here res is now a bit wrong since it has size = 1 and transect_n = 1for transects that had sightings outside of truncation distance and the line data was brought back into the dataframe
  # undo second grouping
  if(stratification=="object"){
    res <- res %>%
      ungroup()
  }
  # save the sample-level stats
  res_sample <- res
  res_sample$distance <- res_sample$object <- res_sample$size <- res_sample$Nhat <- NULL
  ## EK edit function suggestion : add res_sample$size <- NULL & res_sample$Nhat <- NULL
  # because size/Nhat can still differ within transect (sighting-specific - from before grouping),
  # so have 4 extra lines for transects 506, 527, 529, 545
  # res_sample$distance <- res_sample$object <- res_sample$size <- res_sample$Nhat <- NULL #
  res_sample <- unique(res_sample)
# EK : res_sample also wrong, since has 4 transects that have an extra entry - when rolling up, there were still diff's in size and somehow Nhat which carried forward

  ## now at at the stratum level, calculate the abundance in the covered area,
  ## number of observations
  res <- res %>%
    group_by_at(.vars=stratum_labels) %>%
    # calculate various summary stats
    mutate(
      # individuals and observations per stratum
      n_individuals  = sum(.data$size, na.rm=TRUE),
      n_observations = length(na.omit(unique(.data$object))),
      # abundance estimate per stratum in covered area
      Nc             = sum(.data$Nhat, na.rm=TRUE),
      # covered area per transect
      Covered_area   = Distance:::area_calc(.data$df_width, .data$df_left,
                                 .data$Effort, .data$transect_type,
                                 .data$sample_fraction),
      # get group size stats
      group_var      = if_else(.data$n_observations>1,
                               var(.data$size, na.rm=TRUE)/
                                 sum(!is.na(.data$size)),
                               0),
      group_mean     = mean(.data$size, na.rm=TRUE)) %>%
    # report n as n_observations
    mutate(n = .data$n_observations)
  # if we didn't have any areas, then set to 1 and estimate density
  est_density <- FALSE
  if(all(res$Area == 0 | is.na(res$Area))){
    res$Area <- 1
    est_density <- TRUE
  }

  # TODO: make this more elegant
  # TODO: include cue count summary info?
  if(mult){
    res <- res %>%
      mutate(Nc_cuecorrected = .data$Nc/.data$rate)
  }else{
    res <- res %>%
      mutate(Nc_cuecorrected = NA)
  }

  # detection function uncertainty
  # do this sans-pipe, as we need cross-terms and to get the matrix
  if(length(ddf) >1) {
    df_unc <- lapply(ddf, varNhat, data=res)

    df_unc <- lapply(ddf, Distance:::varNhat, data=res)
    deltamethod <- lapply(df_unc, attr, "dm")
    vardat <- lapply(df_unc, function(x){ attr(x, "dm") <- NULL; x})
    vardat <- do.call(rbind.data.frame, vardat)

  }else{
    df_unc <- varNhatx(model = ddf[[1]], data=res) ######### BREAKS HERE for datasets with full surveys with no sightings (ie. winter hw data)

  # df_unc <- ret # EK: used this when need to step through varnhat
  # now need to post-process
  # first pull the deltamethod results
  # deltamethod <- lapply(df_unc, attr, "dm") ######### BREAKS HERE for either full data or with empty transects/surveys filtered out
 # EK edit: perhaps using lapply above here is meant for length(ddf) >1???
  # EK edit: So try:
   deltamethod <- attr(df_unc, "dm")
  # stick all the data.frame bits together
  # first removing all of the deltamethod bits we extracted above
   # vardat <- lapply(df_unc, function(x){ attr(x, "dm") <- NULL; x})
   # vardat <- do.call(rbind.data.frame, vardat)
   # EK edit: and again without lapply:
   vardat <- df_unc
   attr(vardat, "dm") <- NULL
   vardat <- vardat
}
  # we interrupt your regularly-scheduled grouping to bring you...
  # detection function uncertainty
  res <- res %>% ungroup()

  # merge variances back into res
  res <- merge(res, vardat, all.x=TRUE)

  # normal programming resumed
  res <- res %>%
    group_by_at(.vars=stratum_labels) %>%
    # now we're done with observation level stuff, so remove the columns
    # that we don't need
    dplyr::select(!!stratum_labels, "Sample.Label", "Area", "n", "Nc", "transect_n",
           "Effort", "Covered_area", "df_var", "transect_Nc", "group_var",
           "group_mean", "Nc_cuecorrected", "rate_var",  "rate", "rate_df",
           "rate_CV", #"p_var", "p_average",
           "transect_n_observations",
           "n_ddf", "n_par", "er_est") %>%
    # keep only unique rows
    distinct()

  # TODO: fix
  if(mult){
    res <- res %>%
      mutate(Nc = .data$Nc_cuecorrected) %>%
      mutate(transect_Nc = .data$transect_Nc/.data$rate)
  }
  # save ungrouped version for later calculations
  resT <- ungroup(res)

  # calculate ER variance
  res <- ER_var_fy(res, innes=innes, binomial_var=binomial_var)
  # EK edit: (ER_var_f() breaks when trying to assign .originalorder to res by 1:nrow(res) since it's grouped) ## for O2, not R2
  # seems unneeded, so stepped through skipping that one line.
  # res <- erdat
  # calculate final summaries
  res <- res %>%
    # number of transects, total effort and covered area per stratum
    mutate(k            = length(.data$Sample.Label),
           Effort       = sum(.data$Effort),
           Covered_area = sum(.data$Covered_area)) %>%
    mutate(group_var_Nhat = (.data$Area/.data$Covered_area *
                               .data$Nc)^2*
             .data$group_var/.data$group_mean^2) %>%
    mutate(rate_var_Nhat = (.data$Area/.data$Covered_area *
                              .data$Nc)^2*
             .data$rate_var/.data$rate^2) %>%
    ## keep only these columns
    dplyr::select(!!stratum_labels, "Area", "Nc", "n", "ER_var", "Effort", "k",
           "Covered_area", "df_var", "group_var", "group_mean",
           "group_var_Nhat", "ER_var_Nhat", "rate_var", "rate_var_Nhat", "rate",
           "rate_df", "rate_CV", #"p_var", "p_average",
           "n_ddf", "n_par", "er_est") %>%
    ## now just get the distinct cases
    distinct()

  # we calculated n differently above, so reconcile this in the
  # encounter rate calculation
  if(stratification=="object"){
    res <- res %>%
      mutate(ER = sum(.data$n)/.data$Effort)
  }else{
    res <- res %>%
      mutate(ER = .data$n/.data$Effort)
  }
  res <- res %>%
    mutate(ER_CV = sqrt(.data$ER_var)/.data$ER,
           ER_df = Distance:::compute_df(.data$k, type=.data$er_est)) %>%
    # calculate stratum abundance estimate
    mutate(Abundance = (.data$Area/.data$Covered_area) * .data$Nc) %>%
    mutate(df_CV = sqrt(.data$df_var)/.data$Abundance) %>%
    mutate(group_CV = if_else(.data$group_var==0, 0,
                              sqrt(.data$group_var)/.data$group_mean))

  # se and CV
  res <- res %>%
    # first add the ER+group size and detfct variance components
    mutate(Abundance_CV = sqrt(sum(c(res$ER_var_Nhat,
                                     res$df_var),
                                   na.rm=TRUE))/
             .data$Abundance) %>%
    # now add in the multiplier rate CV
    mutate(Abundance_CV = sqrt(sum(c(.data$Abundance_CV^2,
                                     .data$group_CV^2,
                                     .data$rate_CV^2),
                                   na.rm=TRUE))) %>%
    mutate(Abundance_se = .data$Abundance_CV * .data$Abundance) %>%
    distinct()

  # total degrees of freedom and CI calculation
  if(binomial_var){
    # normal approximation for binomial_var
    res <- res %>%
      mutate(bigC = exp((abs(qnorm(ci_width)) *
                           sqrt(log(1 + .data$Abundance_CV^2))))) %>%
      mutate(df = 0)
  }else{
    res <- res %>%
      mutate(df = .data$Abundance_CV^4/
               sum(c(if_else(.data$k==1, 0, .data$ER_CV^4/.data$ER_df),
                     .data$df_CV^4/(.data$n_ddf - .data$n_par),
                     .data$group_CV^4/(.data$n-1),
                     .data$rate_CV^4/.data$rate_df),
                   na.rm=TRUE)) %>%
      # adjust if df is too small
      mutate(df = if_else(.data$Abundance_CV==0, 1, .data$df)) %>%
      mutate(df = if_else(.data$df<1, 1, .data$df)) %>%
      # big C for Satterthwaite
      mutate(bigC = exp((abs(qt(ci_width, .data$df)) *
                           sqrt(log(1 + .data$Abundance_CV^2)))))
  }

  # actually calculate the CIs
  res <- res %>%
    mutate(LCI = if_else(.data$Abundance_CV==0,
                         .data$Abundance, .data$Abundance / .data$bigC),
           UCI = if_else(.data$Abundance_CV==0,
                         .data$Abundance, .data$Abundance * .data$bigC)) %>%
    # done!
    ungroup() %>%
    distinct()


  # make a summary of each stratum group
  res <- as.data.frame(res)

  if(length(ddf)>1){
    res <- res %>%
      group_by_at(.vars=stratum_labels) %>%
      mutate(df_CV = sqrt(sum(.data$df_CV^2)),
             p_average = .data$n/.data$Nc) %>%
      mutate(df_var = (.data$df_CV * .data$p_average)^2) %>%
      ungroup() %>%
      distinct()
    res$p_average <- NULL
  }

  # TODO: this is untested for multiple strata
  # https://github.com/DistanceDevelopment/Distance/issues/46
  # here we loop over the different stratification variables in the below
  # terminology, "row" indicates a summary row (so for a given stratification
  # variable, we have mutiple values ("strata": North, South etc) and then
  # summarize to one row total at the end. This is for generalizability for to
  # multiple stratification variables later)
  for(this_stratum in stratum_labels){ # BREAKS HERE - seems ok when step through instead of run all at once...
    dat_row <- res

    this_stratum <- "year" # EK edit - for stepping through

    # remove NA labels
    iind <- which(is.na(dat_row[, this_stratum]))
    if(length(iind)>0) dat_row <- dat_row[-iind, ]

    # save row names
    stra_row <- dat_row[, stratum_labels, drop=FALSE]
    # remove labels
    dat_row[, stratum_labels] <- NULL

    # don't do anything unless this stratum has rows attached to it
    if(length(stra_row[[this_stratum]]) > 1){
      this_stra_row <- stra_row[1, , drop=FALSE]
      this_stra_row[] <- NA
      this_stra_row[[this_stratum]] <- "Total"

      #### stratification options
      ## in Distance for Windows these are in terms of density
      if(stratification=="geographical"){
        # "weighting by area"
        #  which is adding up the abundances
        dat_row <- dat_row %>%
          # ER ignoring strata
          mutate(ER_var = varn(resT$Effort, resT$transect_n,
                               unique(.data$er_est))) %>%
          # for the density case weight by covered area
          mutate(weight       = if_else(rep(est_density, nrow(dat_row)),
                                        .data$Covered_area/
                                          sum(.data$Covered_area), 1),
                 Area         = sum(.data$Area),
                 Covered_area = sum(.data$Covered_area),
                 Effort       = sum(.data$Effort),
                 k            = sum(.data$k)) %>%
          # now summarize ER variance (Nhat version) and degrees of freedom
          mutate(ER_var_Nhat  = sum(.data$weight^2 * .data$ER_var_Nhat,
                                    na.rm=TRUE)) %>%
          mutate(ER_df = .data$ER_var_Nhat^2/
                   sum((res$ER_var_Nhat^2/.data$ER_df)))
      }else if(stratification %in% c("effort_sum", "replicate")){
        # check that all areas are the same value
        if(length(unique(dat_row$Area))>1 &
           is.null(total_area)){
          stop(paste0("More than 1 Area value in data, need a single Area for stratification=\"",
                      stratification, "\", fix or supply \"total_area\""))
        }
        # if the user didn't supply total_area, but the areas are the same
        # use that as the area
        if(is.null(total_area)){
          total_area <- dat_row$Area[1]
          xt <- 1
        }else{
          xt <- total_area/dat_row$Area
        }
        if(stratification == "replicate"){
          xt <- 1
        }

        dat_row <- dat_row %>%
          mutate(weight       = xt * .data$Effort/sum(.data$Effort)) %>% # EK: weight=annual proportion of total effort
          mutate(ER_var       = sum((.data$Effort/sum(.data$Effort))^2*
                                      .data$ER_var,
                                    na.rm=TRUE)) %>%
          mutate(ER_var_Nhat  = sum(.data$weight^2*.data$ER_var_Nhat, na.rm=TRUE)) %>%
          mutate(ER_df        = .data$ER_var_Nhat^2/
                   sum((
                     (.data$weight^2 *
                           res$ER_var_Nhat)^2/
                          .data$ER_df))
                 ) %>%
          mutate(Area         = total_area,
                 Covered_area = sum(.data$Covered_area),
                 Effort       = sum(.data$Effort),
                 k            = sum(.data$k))
      }else if(stratification=="object"){
        # things you want to add up like object type
        dat_row <- dat_row %>%
          mutate(ER_var       = sum(.data$ER_var, na.rm=TRUE)) %>%
          mutate(ER_var_Nhat  = sum(.data$ER_var_Nhat, na.rm=TRUE)) %>%
          mutate(weight       = 1,
                 Covered_area = .data$Covered_area[1],
                 Area         = .data$Area[1],
                 Effort       = .data$Effort[1],
                 k            = .data$k[1]) %>%
          mutate(ER_df        = .data$ER_var_Nhat^2/sum((res$ER_var_Nhat^2/
                                                           .data$ER_df)))
      }

      # calculate other summaries
      dat_row <- dat_row %>%
        mutate(n = sum(.data$n)) %>%
        mutate(ER = .data$n/.data$Effort) %>%
        mutate(ER_CV = if_else(.data$ER==0,
                               0,
                               sqrt(.data$ER_var)/.data$ER)) %>%
        mutate(group_mean     = mean(.data$group_mean),
               group_var      = sum(.data$group_var),
               group_var_Nhat = sum(.data$group_var_Nhat)) %>%
        mutate(group_CV = if_else(.data$group_var==0, 0,
                                  sqrt(.data$group_var)/.data$group_mean))

      # calculate mean abundance, unless we are doing replicate, where we need
      # the per-stratum abundances for variance later
      if(stratification != "replicate"){
        dat_row <- dat_row %>%
          mutate(Abundance  = sum(.data$weight*.data$Abundance))
      }

      # calculate total variance for detection function(s)
      df_tvar <- 0
      for(i in seq_along(deltamethod)){
        these_weights <- matrix(dat_row$weight, nrow=1)
        df_tvar <- df_tvar + (these_weights %*%
                                # deltamethod[[i]]$variance %*% t(these_weights))
                                deltamethod$variance %*% t(these_weights)) # EK edit
      }
      # detection function CV
      dat_row <- dat_row %>%
        mutate(df_CV  = sqrt(df_tvar)/dat_row$Abundance[1])

      # calculate total variance
      if(stratification=="replicate"){
        # Buckland 2001, 3.84-3.87
        nrep <- nrow(res)
        # get "between" variance (empirical variance of strata)
        rvar <- sum(dat_row$weight*(dat_row$Abundance -
                                      sum(dat_row$weight*dat_row$Abundance))^2)/
          (sum(dat_row$weight) * (nrep-1))
        # now calculate abundance
        dat_row <- dat_row %>%
          mutate(Abundance  = sum(.data$weight*.data$Abundance),
                 ER_df      = nrep-1)
        # add the detection function variance
        tvar <- rvar + df_tvar
      }else if(stratification=="effort_sum"){
        # add the pre-weighted CVs
        tvar <- dat_row$Abundance[1]^2 *
          sum(c(dat_row$ER_CV[1]^2,
                dat_row$df_CV[1]^2,
                dat_row$rate_CV[1]^2,
                dat_row$group_CV[1]^2),
              na.rm=TRUE)
      }else{
        # add sources of variance
        tvar <- dat_row$ER_var_Nhat[1] +
          df_tvar +
          dat_row$Abundance[1]^2 * dat_row$rate_CV[1]^2 # for multiplier
        # add-in group size component if not doing Innes et al
        if(!innes){
          tvar <- dat_row$group_var_Nhat[1] +
            tvar
        }
      }

      dat_row <- dat_row %>%
        mutate(Nc     = sum(.data$weight*.data$Nc),
               df_var = df_tvar[1,1]) %>%
        mutate(Abundance_se = sqrt(tvar)) %>%
        mutate(Abundance_CV = .data$Abundance_se/.data$Abundance) %>%
        mutate(df   = NA,
               bigC = NA,
               LCI  = NA,
               UCI  = NA)
      # drop weights as we don't need them any more
      dat_row$weight <- NULL
      # drop unwanted rows
      dat_row <- dat_row %>%
        distinct()

      ## compute degrees of freedom

      # replicate
      if(stratification == "replicate"){
        dat_row <- dat_row %>%
          mutate(wtcv = sum(c((sqrt(rvar)/.data$Abundance[1])^4/
                                .data$ER_df[1],
                              (df_tvar/.data$Abundance[1]^2)^2/
                                (.data$n_ddf[1] - .data$n_par[1]),
                              if_else(.data$df==0, 0 ,
                                      (.data$rate_var_Nhat[1]/
                                         .data$Abundance[1]^2)^2/
                                        .data$rate_df[1]),
                              (.data$group_var_Nhat[1]/
                                 .data$Abundance[1]^2)^2/
                                (.data$n_ddf[1]-1)
          ),
          na.rm=TRUE)) %>%
          # calculate Satterthwaite df
          mutate(df = sum(c((sqrt(rvar)/.data$Abundance[1])^2,
                            (df_tvar/.data$Abundance[1]^2),
                            if_else(.data$df==0, 0 ,
                                    (.data$rate_var_Nhat[1]/
                                       .data$Abundance[1]^2)),
                            (.data$group_var_Nhat[1]/.data$Abundance[1]^2)
          ),
          na.rm=TRUE)^2) %>%
          mutate(df = .data$df/.data$wtcv) %>%
          #mutate(df = .data$ER_df + (nrep - .data$n_par)) %>%
          mutate(bigC = exp((abs(qt(ci_width, .data$df)) *
                               sqrt(log(1 + .data$Abundance_CV^2)))))
        # drop weight column
        dat_row$wtcv <- NULL
      }else{
        # non-replicate case
        if(binomial_var){
          # normal approximation for binomial_var
          dat_row <- dat_row %>%
            mutate(bigC = exp((abs(qnorm(ci_width)) *
                                 sqrt(log(1 + .data$Abundance_CV^2))))) %>%
            mutate(df = 0)
        }else{
          df_tvar <- df_tvar[1, 1]
          dat_row <- dat_row %>%
            # average multiplier
            mutate(rate          = mean(.data$rate),
                   rate_df       = sum(.data$rate_df),
                   rate_var      = sum(.data$rate_var),
                   rate_var_Nhat = .data$Abundance^2 * .data$rate_CV^2,
                   rate_CV       = sqrt(sum(.data$rate_var))/mean(.data$rate))
          dat_row <- dat_row %>%
            # CV weights for Satterthwaite df
            # denominator of Buckland et al 2001, eqn 3.75
            mutate(wtcv = sum(c((.data$ER_var_Nhat[1]/
                                   .data$Abundance[1]^2)^2/
                                  .data$ER_df[1],
                                (df_tvar/.data$Abundance[1]^2)^2/
                                  (.data$n_ddf[1] - .data$n_par[1]),
                                if_else(.data$df==0, 0,
                                        (.data$rate_var_Nhat[1]/
                                           .data$Abundance[1]^2)^2/
                                          .data$rate_df[1]),
                                (.data$group_var_Nhat[1]/
                                   .data$Abundance[1]^2)^2/
                                  (.data$n_ddf[1]-1)
            ),
            na.rm=TRUE)) %>%
            # calculate Satterthwaite df
            # each element is CV^2, then sum and square again to get CV^4
            # numerator of Buckland et al 2001, eqn 3.75
            mutate(df = sum(c(.data$ER_var_Nhat[1]/.data$Abundance[1]^2,
                              df_tvar/.data$Abundance[1]^2,
                              if_else(.data$df==0, 0 ,
                                      (.data$rate_var_Nhat[1]/
                                         .data$Abundance[1]^2)),
                              (.data$group_var_Nhat[1]/.data$Abundance[1]^2)
            ),
            na.rm=TRUE)^2) %>%
            mutate(df = .data$df/.data$wtcv) %>%
            mutate(bigC = exp((abs(qt(ci_width, .data$df)) *
                                 sqrt(log(1 + .data$Abundance_CV^2)))))
          # drop weight column
          dat_row$wtcv <- NULL
        }
      }
      # actually calculate the CIs
      dat_row <- dat_row %>%
        mutate(LCI = .data$Abundance / .data$bigC,
               UCI = .data$Abundance * .data$bigC)

      this_stra_row <- cbind.data.frame(this_stra_row, dat_row, row.names=NULL)

      res <- rbind(res, this_stra_row)
    }
  }


  ## last bit of formatting
  res <- as.data.frame(res)
   res  <- unique(res)

  # warn if we only had one transect in one or more strata
  if(any(res$k == 1)){
    warning("One or more strata have only one transect, cannot calculate empirical encounter rate variance")
  }

  # fix area == covered area for compatibility with mrds::dht
  if(est_density){
    #res$Area <- res$Covered_area
    res <- res %>%
      mutate(Area = .data$Covered_area)
  }else{
    # generate density results too!
    dens_res <- res %>%
      mutate(Density = .data$Abundance/.data$Area,
             df_var = .data$df_var/.data$Area^2) %>%
      mutate(Density_se = sqrt(.data$Abundance_se^2/.data$Area^2)) %>%
      mutate(Density_CV = .data$Density_se/.data$Density) %>%
      mutate(bigC = exp((abs(qt(ci_width, .data$df)) *
                           sqrt(log(1 + .data$Density_CV^2))))) %>%
      mutate(LCI   = .data$Density / .data$bigC,
             UCI   = .data$Density * .data$bigC,
             Area  = .data$Covered_area) %>%
      dplyr::select(!!stratum_labels, "Area", "n", "ER_var", "Effort", "k",
             "Density", "Density_CV", "Density_se", "UCI", "LCI", "df",
             "Covered_area", "df_var", "group_var", "group_mean",
             "rate_var", "rate", "rate_df", "rate_CV", #"p_var", "p_average",
             "er_est")

    # store this
    attr(res, "density") <- dens_res
  }

  # save stratum labels
  attr(res, "stratum_labels") <- stratum_labels
  # were we really estimating density?
  attr(res, "density_only") <- est_density
  # save the sample level estimates
  attr(res, "sample_res") <- res_sample
  # detection function variance data
  attr(res, "df_var") <- deltamethod
  # save the variance proportions
  attr(res, "prop_var") <- Distance:::variance_contributions(res)
  # save grouped analysis (might be NULL)
  attr(res, "grouped") <- grouped
  attr(res, "grouped") <- NULL
  # save enounter rate variance information
  attr(res, "ER_var") <- list(unique(res$er_est), innes, binomial_var)
  # save stratification type
  attr(res, "stratification") <- stratification
  # save multiplier info
  attr(res, "multipliers") <- names(multipliers)
  # save sample_fraction
  attr(res, "sample_fraction") <- sample_fraction

  class(res) <- c("dht_result", "data.frame")
  # return(res)
  hw_O2_sp_EK <- res
# }
