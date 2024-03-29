#' Fit nonlinear mixed-effects models built from one or more kinetic
#' degradation models and one or more error models
#'
#' The name of the methods expresses that (**m**ultiple) **h**ierarchichal
#' (also known as multilevel) **m**ulticompartment **kin**etic models are
#' fitted. Our kinetic models are nonlinear, so we can use various nonlinear
#' mixed-effects model fitting functions.
#'
#' @param objects A list of [mmkin] objects containing fits of the same
#' degradation models to the same data, but using different error models.
#' Alternatively, a single [mmkin] object containing fits of several
#' degradation models to the same data
#' @param backend The backend to be used for fitting. Currently, only saemix is
#' supported
#' @param no_random_effect Default is NULL and will be passed to [saem]. If a
#' character vector is supplied, it will be passed to all calls to [saem],
#' which will exclude random effects for all matching parameters. Alternatively,
#' a list of character vectors or an object of class [illparms.mhmkin] can be
#' specified. They have to have the same dimensions that the return object of
#' the current call will have, i.e. the number of rows must match the number
#' of degradation models in the mmkin object(s), and the number of columns must
#' match the number of error models used in the mmkin object(s).
#' @param algorithm The algorithm to be used for fitting (currently not used)
#' @param \dots Further arguments that will be passed to the nonlinear mixed-effects
#' model fitting function.
#' @param cores The number of cores to be used for multicore processing. This
#' is only used when the \code{cluster} argument is \code{NULL}. On Windows
#' machines, cores > 1 is not supported, you need to use the \code{cluster}
#' argument to use multiple logical processors. Per default, all cores detected
#' by [parallel::detectCores()] are used, except on Windows where the default
#' is 1.
#' @param cluster A cluster as returned by [makeCluster] to be used for
#' parallel execution.
#' @importFrom parallel mclapply parLapply detectCores
#' @return A two-dimensional [array] of fit objects and/or try-errors that can
#' be indexed using the degradation model names for the first index (row index)
#' and the error model names for the second index (column index), with class
#' attribute 'mhmkin'.
#' @author Johannes Ranke
#' @seealso \code{\link{[.mhmkin}} for subsetting [mhmkin] objects
#' @export
mhmkin <- function(objects, ...) {
  UseMethod("mhmkin")
}

#' @export
#' @rdname mhmkin
mhmkin.mmkin <- function(objects, ...) {
  mhmkin(list(objects), ...)
}

#' @export
#' @rdname mhmkin
#' @examples
#' \dontrun{
#' # We start with separate evaluations of all the first six datasets with two
#' # degradation models and two error models
#' f_sep_const <- mmkin(c("SFO", "FOMC"), ds_fomc[1:6], cores = 2, quiet = TRUE)
#' f_sep_tc <- update(f_sep_const, error_model = "tc")
#' # The mhmkin function sets up hierarchical degradation models aka
#' # nonlinear mixed-effects models for all four combinations, specifying
#' # uncorrelated random effects for all degradation parameters
#' f_saem_1 <- mhmkin(list(f_sep_const, f_sep_tc), cores = 2)
#' status(f_saem_1)
#' # The 'illparms' function shows that in all hierarchical fits, at least
#' # one random effect is ill-defined (the confidence interval for the
#' # random effect expressed as standard deviation includes zero)
#' illparms(f_saem_1)
#' # Therefore we repeat the fits, excluding the ill-defined random effects
#' f_saem_2 <- update(f_saem_1, no_random_effect = illparms(f_saem_1))
#' status(f_saem_2)
#' illparms(f_saem_2)
#' # Model comparisons show that FOMC with two-component error is preferable,
#' # and confirms our reduction of the default parameter model
#' anova(f_saem_1)
#' anova(f_saem_2)
#' # The convergence plot for the selected model looks fine
#' saemix::plot(f_saem_2[["FOMC", "tc"]]$so, plot.type = "convergence")
#' # The plot of predictions versus data shows that we have a pretty data-rich
#' # situation with homogeneous distribution of residuals, because we used the
#' # same degradation model, error model and parameter distribution model that
#' # was used in the data generation.
#' plot(f_saem_2[["FOMC", "tc"]])
#' # We can specify the same parameter model reductions manually
#' no_ranef <- list("parent_0", "log_beta", "parent_0", c("parent_0", "log_beta"))
#' dim(no_ranef) <- c(2, 2)
#' f_saem_2m <- update(f_saem_1, no_random_effect = no_ranef)
#' anova(f_saem_2m)
#' }
mhmkin.list <- function(objects, backend = "saemix", algorithm = "saem",
  no_random_effect = NULL,
  ...,
  cores = if (Sys.info()["sysname"] == "Windows") 1 else parallel::detectCores(), cluster = NULL)
{
  call <- match.call()
  dot_args <- list(...)
  backend_function <- switch(backend,
    saemix = "saem"
  )

  deg_models <- lapply(objects[[1]][, 1], function(x) x$mkinmod)
  names(deg_models) <- dimnames(objects[[1]])$model
  n.deg <- length(deg_models)

  ds <- lapply(objects[[1]][1, ], function(x) x$data)

  for (other in objects[-1]) {
    # Check if the degradation models in all objects are the same
    for (deg_model_name in names(deg_models)) {
      if (!all.equal(other[[deg_model_name, 1]]$mkinmod$spec,
            deg_models[[deg_model_name]]$spec))
      {
        stop("The mmkin objects have to be based on the same degradation models")
      }
    }
    # Check if they have been fitted to the same dataset
    other_object_ds <- lapply(other[1, ], function(x) x$data)
    for (i in 1:length(ds)) {
      if (!all.equal(ds[[i]][c("time", "variable", "observed")],
          other_object_ds[[i]][c("time", "variable", "observed")]))
      {
        stop("The mmkin objects have to be fitted to the same datasets")
      }
    }
  }

  n.o <- length(objects)

  error_models = sapply(objects, function(x) x[[1]]$err_mod)
  n.e <- length(error_models)

  n.fits <- n.deg * n.e
  fit_indices <- matrix(1:n.fits, ncol = n.e)
  dimnames(fit_indices) <- list(degradation = names(deg_models),
                                error = error_models)

  if (is.null(no_random_effect) || is.null(dim(no_random_effect))) {
    no_ranef <- rep(list(no_random_effect), n.fits)
    dim(no_ranef) <- dim(fit_indices)
  } else {
    if (!identical(dim(no_random_effect), dim(fit_indices))) {
      stop("Dimensions of argument 'no_random_effect' are not suitable")
    }
    if (is(no_random_effect, "illparms.mhmkin")) {
      no_ranef_dim <- dim(no_random_effect)
      no_ranef <- lapply(no_random_effect, function(x) {
        no_ranef_split <- strsplit(x, ", ")
        ret <- sapply(no_ranef_split, function(y) {
          gsub("sd\\((.*)\\)", "\\1", y)
        })
        return(ret)
      })
      dim(no_ranef) <- no_ranef_dim
    } else {
      no_ranef <- no_random_effect
    }
  }

  fit_function <- function(fit_index) {
    w <- which(fit_indices == fit_index, arr.ind = TRUE)
    deg_index <- w[1]
    error_index <- w[2]
    mmkin_row <- objects[[error_index]][deg_index, ]
    res <- try(do.call(backend_function,
        args = c(
          list(mmkin_row),
          dot_args,
          list(no_random_effect = no_ranef[[deg_index, error_index]]))))
    return(res)
  }


  fit_time <- system.time({
    if (is.null(cluster)) {
      results <- parallel::mclapply(as.list(1:n.fits), fit_function,
        mc.cores = cores, mc.preschedule = FALSE)
    } else {
      results <- parallel::parLapply(cluster, as.list(1:n.fits), fit_function)
    }
  })

  attributes(results) <- attributes(fit_indices)
  attr(results, "call") <- call
  attr(results, "time") <- fit_time
  class(results) <- switch(backend,
    saemix = c("mhmkin.saem.mmkin", "mhmkin")
  )
  return(results)
}

#' Subsetting method for mhmkin objects
#'
#' @param x An [mhmkin] object.
#' @param i Row index selecting the fits for specific models
#' @param j Column index selecting the fits to specific datasets
#' @param drop If FALSE, the method always returns an mhmkin object, otherwise
#'   either a list of fit objects or a single fit object.
#' @return An object inheriting from \code{\link{mhmkin}}.
#' @rdname mhmkin
#' @export
`[.mhmkin` <- function(x, i, j, ..., drop = FALSE) {
  original_class <- class(x)
  class(x) <- NULL
  x_sub <- x[i, j, drop = drop]

  if (!drop) {
    class(x_sub) <- original_class
  }
  return(x_sub)
}

#' Print method for mhmkin objects
#'
#' @rdname mhmkin
#' @export
print.mhmkin <- function(x, ...) {
  cat("<mhmkin> object\n")
  cat("Status of individual fits:\n\n")
  print(status(x))
}

#' @export
AIC.mhmkin <- function(object, ..., k = 2) {
  if (inherits(object[[1]], "saem.mmkin")) {
    check_failed <- function(x) if (inherits(x$so, "try-error")) TRUE else FALSE
  }
  res <- sapply(object, function(x) {
    if (check_failed(x)) return(NA)
    else return(AIC(x$so, k = k))
  })
  dim(res) <- dim(object)
  dimnames(res) <- dimnames(object)
  return(res)
}

#' @export
BIC.mhmkin <- function(object, ...) {
  if (inherits(object[[1]], "saem.mmkin")) {
    check_failed <- function(x) if (inherits(x$so, "try-error")) TRUE else FALSE
  }
  res <- sapply(object, function(x) {
    if (check_failed(x)) return(NA)
    else return(BIC(x$so))
  })
  dim(res) <- dim(object)
  dimnames(res) <- dimnames(object)
  return(res)
}

#' @export
update.mhmkin <- function(object, ..., evaluate = TRUE) {
  call <- attr(object, "call")
  # For some reason we get mhkin.list in call[[1]] when using mhmkin from the
  # loaded package so we need to fix this so we do not have to export
  # mhmkin.list in addition to the S3 method mhmkin
  call[[1]] <- mhmkin

  update_arguments <- match.call(expand.dots = FALSE)$...

  if (length(update_arguments) > 0) {
    update_arguments_in_call <- !is.na(match(names(update_arguments), names(call)))
  }

  for (a in names(update_arguments)[update_arguments_in_call]) {
    call[[a]] <- update_arguments[[a]]
  }

  update_arguments_not_in_call <- !update_arguments_in_call
  if(any(update_arguments_not_in_call)) {
    call <- c(as.list(call), update_arguments[update_arguments_not_in_call])
    call <- as.call(call)
  }
  if(evaluate) eval(call, parent.frame())
  else call
}

#' @export
anova.mhmkin <- function(object, ...,
  method = c("is", "lin", "gq"), test = FALSE, model.names = "auto") {
  if (identical(model.names, "auto")) {
    model.names <- outer(rownames(object), colnames(object), paste)
  }
  rlang::inject(anova(!!!(object), method = method, test = test,
      model.names = model.names))
}

