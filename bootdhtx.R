bootdht <- function(
  # model,
  #                   flatfile,
  #                   resample_strata=FALSE,
  #                   resample_obs=FALSE,
  #                   resample_transects=TRUE,
  #                   nboot=100,
  #                   summary_fun=bootdht_Nhat_summarize,
  #                   convert_units=1,
  #                   select_adjustments=FALSE,
  #                   sample_fraction=1,
  #                   multipliers=NULL,
  #                   progress_bar="base",
  #                   cores=1, convert.units=NULL){

  model = hw1.5.hn.v
  flatfile = hw_w
  resample_strata=F
  resample_obs=FALSE
  resample_transects=TRUE
  nboot=10
  summary_fun=bootdht_Dhat_summarize
  convert_units=conversion.factor
  select_adjustments=FALSE
  sample_fraction=1
  multipliers=NULL
  progress_bar="base"
  cores=1
  # convert.units=conversion.factor


  .deprecated_args(c("convert.units"), as.list(match.call()))

  if(!any(c(resample_strata, resample_obs, resample_transects))){
    stop("At least one of resample_strata, resample_obs, resample_transects must be TRUE")
  }

  # make everything a list...
  if(!inherits(model, "list")){
    models <- list(model)
    # yes, I am a monster
  }else{
    models <- model
  }

  if(missing(convert_units)){
    convert_units <-  NULL
  }

  # only use valid ds models
  for(i in seq_along(models)){
    if(!is(models[[i]], "dsmodel")){
      stop("Only models fitted by Distance::ds() may be used")
    }
  }
  dat <- flatfile
  if(!("object" %in% names(dat))){
    dat$object <- 1:nrow(dat)
  }

  # if we're using the default summary function and have Area 0 then
  # we're not going to have a good time
  if(missing(summary_fun) &
     (is.null(flatfile$Area) || all(flatfile$Area==0))){
    stop("No Area in flatfile, densities will be returned and the default summary function records only abundances. You need to write your own summary_fun.")
  }

  # apply the sample fraction
  dat <- Distance:::dht2_sample_fraction(sample_fraction, dat)
  dat$Effort <- dat$Effort * dat$sample_fraction
  dat$sample_fraction <- NULL

  # process non-function multipliers
  multipliers_nofun <- Filter(Negate(is.function), multipliers)
  if(length(multipliers_nofun) > 0){
    dat <- dht2_multipliers(multipliers_nofun, dat)
  }else{
    dat$rate <- 1
  }

  # get any multiplier functions
  multipliers_fun <- Filter(is.function, multipliers)

  # this can be generalized later on
  stratum_label <- "Region.Label"
  obs_label <- "object"
  sample_label <- "Sample.Label"

  # which resamples are we going to make?
  possible_resamples <- c(stratum_label, sample_label, obs_label)
  our_resamples <- possible_resamples[c(resample_strata, resample_transects,
                                        resample_obs)]

  # make sure these are characters for resampling
  dat[, our_resamples] <- lapply(dat[, our_resamples, drop=FALSE], as.character)

  # process models
  # this resolves all symbols in the call so arguments can be accessed when
  # running in parallel
  models <- lapply(models, function(model){
    lpars <- as.list(model$call)
    for(i in seq(2, length(lpars))){
      if(is.symbol(model$call[[names(lpars)[i]]])){
        model$call[[names(lpars)[i]]] <- eval(lpars[[i]],
                                              envir=parent.frame(n=3))
      }
    }
    model
  })

  cat(paste0("Performing ", nboot, " bootstraps\n"))

  if(cores > 1 & progress_bar != "none"){
    progress_bar <- "none"
    message("Progress bars cannot be shown when using cores>1")
  }

  # decide how to report progress
  if(progress_bar == "base"){
    pb <- list(pb        = txtProgressBar(0, nboot, style=3),
               increment = function(pb){
                 setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
               },
               set = function(pb, n, max) setTxtProgressBar(pb, n),
               done      = function(pb){
                 setTxtProgressBar(pb, environment(pb$up)$max)
               })
  }else if(progress_bar == "none"){
    pb <- list(pb        = NA,
               set = function(pb, n, max) invisible(),
               increment = function(pb) invisible(),
               done = function(pb) invisible())
  }else if(progress_bar == "progress"){
    if (!requireNamespace("progress", quietly = TRUE)){
      stop("Package 'progress' not installed!")
    }else{
      pb <- list(pb = progress::progress_bar$new(
        format="   [:bar] :percent eta: :eta",
        total=nboot, clear=FALSE, width=80),
        set = function(pb, n, max) pb$update(n/max),
        increment = function(pb) pb$tick(),
        done = function(pb) pb$update(1))
      pb$pb$tick(0)
    }
  }else{
    stop("progress_bar must be one of \"none\", \"base\" or \"progress\"")
  }

  # run the bootstrap
  if(cores > 1){
    if (!requireNamespace("foreach", quietly = TRUE) &
        !requireNamespace("doParallel", quietly = TRUE) &
        !requireNamespace("doRNG", quietly = TRUE) &
        !requireNamespace("parallel", quietly = TRUE)){
      stop("Packages 'parallel', 'foreach' and 'doParallel' need to be installed to use multiple cores.")
    }

    # build the cluster
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    # shutdown cluster when the function exits
    # (this works even if the function crashes)
    on.exit(parallel::stopCluster(cl))

    # load the activity package in the other sessions
    if(length(multipliers_fun) > 0){
      packages <- c("activity")
    }else{
      packages <- NULL
    }

    # needed to avoid a syntax error/check fail
    `%dopar%` <- foreach::`%dopar%`
    `%dorng2%` <- doRNG::`%dorng%`
    # fit the model nboot times over cores cores
    # note there is a bit of fiddling here with the progress bar to get it to
    # work (updates happen in this loop rather than in bootit)
    boot_ests <- foreach::foreach(i=1:nboot, .packages=packages) %dorng2% {
      bootit(dat, models=models, our_resamples=our_resamples,
             summary_fun=summary_fun, convert_units=convert_units,
             pb=list(increment=function(pb){invisible()}),
             multipliers_fun=multipliers_fun, sample_label=sample_label,
             select_adjustments=select_adjustments)
    }

  }else{
    boot_ests <- replicate(nboot,
                           Distance:::bootit(dat, models=models, our_resamples,
                                  summary_fun, convert_units=convert_units,
                                  pb=pb, multipliers_fun=multipliers_fun,
                                  sample_label=sample_label,
                                  select_adjustments=select_adjustments),
                           simplify=FALSE)
  }

  # do some post-processing
  fail_fun <- function(x) inherits(x, "bootstrap_failure")
  # add replicate IDs
  bootids <- seq_len(length(boot_ests))
  # how many failures
  failures <- length(Filter(fail_fun, boot_ests))
  # remove failures from IDs
  bootids <- as.list(bootids[unlist(lapply(boot_ests, Negate(fail_fun)))])
  # get just the "good" results
  boot_ests <- Filter(Negate(fail_fun), boot_ests)
  # add IDs to estimates list
  boot_ests <- mapply(cbind.data.frame,
                      boot_ests, bootstrap_ID=bootids,
                      SIMPLIFY=FALSE)

  # the above is then a list of thingos, do the "right" thing and assume
  # they are data.frames and then rbind them all together
  boot_ests <- do.call(rbind.data.frame, boot_ests)

  cat("\n")

  attr(boot_ests, "nboot") <- nboot
  attr(boot_ests, "failures") <- failures
  class(boot_ests) <- c("dht_bootstrap", "data.frame")
  return(boot_ests)
}

