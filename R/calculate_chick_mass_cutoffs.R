#' Determine chick growth curves
#'
#' Fit logistic growth curve to chick mass data to calculate age-specific reference values.
#'
#' @param Capture_data Data frame with capture data on chicks of 1 species in 1 population
#' @param plot Create plot of mass data and logistic growth curve. Default: FALSE.
#'
#' @return A data frame with reference values for mass at each age
#' @export
#'
#' @examples
#' \dontrun{
#' # Run pipeline for Hoge Veluwe great tits
#' HOG_PARMAJ_data <- run_pipelines(PopID = "HOG", Species = "PARMAJ", output_type = "R")
#'
#' # Calculate reference values using logistic growth model
#' calculate_chick_mass_cutoffs(HOG_PARMAJ_data, plot = TRUE)
#' }

calculate_chick_mass_cutoffs <- function(Capture_data, plot = FALSE) {

  # Capture_data should be of one species in one population
  if(length(unique(Capture_data$Species)) > 1) {

    stop("Capture_data contains data on more than 1 species. Select data on 1 species only.")

  }

  if(length(unique(Capture_data$CapturePopID)) > 1) {

    stop("Capture_data contains data on more than 1 population Select data on 1 population only.")

  }

  # Filter data
  data <- Capture_data %>%
    dplyr::filter(!is.na(.data$ChickAge), .data$ChickAge > 0, !is.na(.data$Mass))

  # Check that there are non-NA records left to fit a logistic model on
  if(nrow(data) == 0) {

    stop("All ChickAge and/or Mass records are NA.")

  }

  # Set initial values for logistic model
  max_mass <- data %>%
    dplyr::filter(.data$ChickAge == max(.data$ChickAge)) %>%
    dplyr::summarise(M = max(.data$Mass)) %>%
    dplyr::pull("M")

  initial_values <- list(a = max_mass,
                         b = 5,
                         c = 0.1)

  # Fit logistic model
  logistic_model <- stats::nls(Mass ~ a / (1 + b * (exp(-c * ChickAge))), data = data,
                               start = initial_values, trace = TRUE)

  # Predict and calculate 1st and 99th quantiles
  newdata <- data.frame(ChickAge = seq(1, max(data$ChickAge), by = 1))

  logistic_pred <- tibble::tibble(fit = stats::predict(logistic_model, newdata = newdata),
                                  x = newdata$ChickAge) %>%
    dplyr::mutate(upper = .data$fit + summary(logistic_model)$sigma * stats::qnorm(0.99, 0, 1),
                  lower = .data$fit + summary(logistic_model)$sigma * stats::qnorm(0.01, 0, 1))


  if(plot == TRUE) {

    if (requireNamespace(package = "ggplot2", quietly = TRUE)) {

      # Plot logistic growth curve and data
      p <- ggplot2::ggplot() +
        ggplot2::geom_jitter(data = data, ggplot2::aes(x = .data$ChickAge, y = .data$Mass), shape = 21, alpha = 0.4, width = 0.2) +
        ggplot2::geom_line(data = logistic_pred, ggplot2::aes(x = .data$x, y = .data$lower), colour = "darkred", lty = 2) +
        ggplot2::geom_line(data = logistic_pred, ggplot2::aes(x = .data$x, y = .data$upper), colour = "darkred", lty = 2) +
        ggplot2::geom_line(data = logistic_pred, ggplot2::aes(x = .data$x, y = .data$fit), size = 1, colour = "darkred") +
        ggplot2::labs(title = paste0(unique(data$CapturePopID), ": ", unique(data$Species))) +
        ggplot2::theme_classic()

    } else {

      warning("ggplot2 is required for plotting. Plotting skipped.")

    }

  }

  # Create day-specific reference values
  ref_values <- tibble::tibble(Species = rep(unique(data$Species), length(logistic_pred$x)),
                               PopID = rep(unique(data$CapturePopID), length(logistic_pred$x)),
                               Stage = as.character(logistic_pred$x),
                               Error_min = 0,
                               Error_max = 2 * logistic_pred$upper,
                               n =   {data %>%
                                   dplyr::filter(.data$ChickAge > 1) %>%
                                   dplyr::group_by(.data$ChickAge) %>%
                                   dplyr::summarise(N = dplyr::n()) %>%
                                   tidyr::complete(ChickAge = logistic_pred$x) %>%
                                   dplyr::pull("N")},
                               Logis = TRUE
  )


  if(plot == TRUE) {

    return(p)

  } else {

    return(ref_values)

  }

}

## OLD FUNCTION

# Determine chick growth curves

# Fit logistic and gompertz growth curves
# to chick mass data for great and blue tits.

# We fit this using data from Hoge Veluwe. See example for data extraction.

# @param HOG_chick_data Data frame with chick age and mass of Hoge Veluwe great and blue tits

# @return A data frame with cutoffs values at each age
# @export
# @importFrom stats na.omit nls

# @examples
# \dontrun{
# #run pipeline for Hoge Veluwe great and blue tits
# HOG <- run_pipelines(PopID = "HOG", Species = c("PARMAJ", "CYACAE"), output_type = "R")

# Subset relevant columns
# HOG_chick_data <- HOG$Capture_data %>%
# dplyr::filter(!is.na(ChickAge) & !is.na(Mass), BreedingSeason <= 2019) %>%
# dplyr::select(ChickAge, Mass, Species)

# calculate_chick_mass_cutoffs(HOG_chick_data)
# }


# calculate_chick_mass_cutoffs <- function(HOG_chick_data){
#
#   #For great tit
#   data <- dplyr::filter(HOG_chick_data, !is.na(Mass) & Species == "PARMAJ")
#
#   #Remove the obvious outlier
#   data <- dplyr::filter(data, Mass < 25)
#
#   #Fit a logistic regression
#
#   #Starting values based on fitted lm
#   logistic_model <- nls(Mass ~ a*(1+b*(exp(-c*ChickAge)))^-1,
#                         start = list(a = 17, b = 0.1, c = 0.25), data = data, trace = TRUE)
#
#   #Fit Gompertz curve
#   gompertz_model <- nls(Mass ~ a*exp(-b*exp(-c*ChickAge)),
#                         start=list(a = 25, b = 2, c = 0.1), data = data, trace = TRUE)
#
#   #Gompertz curve fits fine
#   # ggplot() +
#   #   geom_point(data = data, aes(x = ChickAge, y = Mass), shape = 21, size = 3, alpha = 0.75) +
#   #   geom_line(aes(x = seq(0, 30, 1), y = predict(gompertz_model, newdata = data.frame(ChickAge = seq(0, 30, 1)))), size = 1) +
#   #   theme_classic()
#
#   #Use predictNLS function from RBlogger to get 95% CIs for both models
#   #Made redundant. Code is stored below out of interest.
#   # newdata <- data.frame(ChickAge = seq(0, 30, 1))
#   # logistic_pred <- as.data.frame(predictNLS(logistic_model, newdata = newdata, level = 0.999))
#   # gompertz_pred <- as.data.frame(predictNLS(gompertz_model, newdata = newdata, level = 0.999))
#
#   #Look at the same approach assuming error around the predicted line is normally distributed (i.e. 1.96*SE)
#   logistic_pred$upper <- logistic_pred$fit + (summary(logistic_model)$sigma*1.96)
#   logistic_pred$lower <- logistic_pred$fit - (summary(logistic_model)$sigma*1.96)
#
#   gompertz_pred$upper <- gompertz_pred$fit + (summary(gompertz_model)$sigma*1.96)
#   gompertz_pred$lower <- gompertz_pred$fit - (summary(gompertz_model)$sigma*1.96)
#
#   #Plot both
#   # logistic_plot <- ggplot()+
#   #   geom_point(data = data, aes(x = ChickAge, y = Mass), shape = 21, size = 3) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = lower), colour = "black", lty = 2) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = upper), colour = "black", lty = 2) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = `0.05%`), colour = "black", lty = 1) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = `99.95%`), colour = "black", lty = 1) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = fit), size = 1)+
#   #   theme_classic()
#
#   # gompertz_plot <- ggplot()+
#   #   geom_point(data = data, aes(x = ChickAge, y = Mass), shape = 21, size = 3) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = lower), colour = "black", lty = 2) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = upper), colour = "black", lty = 2) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = `0.05%`), colour = "black", lty = 1) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = `99.95%`), colour = "black", lty = 1) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = fit), size = 1)+
#   #   theme_classic()
#
#   # print(cowplot::plot_grid(logistic_plot, gompertz_plot, nrow = 1))
#
#   #Observing the fit, it seems that logistic model fits better (it better models
#   #the asymptote at ~17g) The boostrap CIs for the marginal model predications are
#   #very tight because there's simply so much data We will instead used the 1.96*SE
#   #as these are wider. This is a crude method and we will need to limit our
#   #cut-offs to 0 at the youngest ages It will work for now.
#   PARMAJ_cutoffs <- tibble(Species = "PARMAJ", ChickAge = seq(0, 30, 1),
#                          Warning_min = ifelse(logistic_pred$lower < 0, NA, logistic_pred$lower),
#                          Warning_max = logistic_pred$upper)
#
#   #For blue tit
#   data <- dplyr::filter(HOG_chick_data, !is.na(Mass) & Species == "CYACAE")
#
#   data <- dplyr::filter(data, ChickAge < 20)
#
#   #Plot info if needed
#   # data %>%
#   #   ggplot() +
#   #   geom_point(aes(x = ChickAge, y = Mass), shape = 21, size = 3, alpha = 0.75) +
#   #   geom_smooth(aes(x = ChickAge, y = Mass)) +
#   #   theme_classic()
#
#   #Fit a logistic regression
#
#   #Starting values based on fitted lm
#   logistic_model <- nls(Mass ~ a*(1+b*(exp(-c*ChickAge)))^-1,
#                         start = list(a = 17, b = 5, c = 0.15), data = data, trace = TRUE)
#
#   #Fit Gompertz curve
#   gompertz_model <- nls(Mass ~ a*exp(-b*exp(-c*ChickAge)),
#                         start=list(a = 25, b = 5, c = 0.1), data = data, trace = TRUE)
#
#   #Gompertz curve fits fine
#   # ggplot() +
#   #   geom_point(data = data, aes(x = ChickAge, y = Mass), shape = 21, size = 3, alpha = 0.75) +
#   #   geom_line(aes(x = seq(0, 30, 1), y = predict(gompertz_model, newdata = data.frame(ChickAge = seq(0, 30, 1)))), size = 1) +
#   #   theme_classic()
#
#   #Use predictNLS function from RBlogger to get 95% CIs for both models
#   #As above, no longer used
#   # newdata <- data.frame(ChickAge = seq(0, 30, 1))
#   # logistic_pred <- as.data.frame(predictNLS(logistic_model, newdata = newdata, level = 0.999))
#   # gompertz_pred <- as.data.frame(predictNLS(gompertz_model, newdata = newdata, level = 0.999))
#
#   #Look at the same approach assuming error around the predicted line is normally distributed (i.e. 1.96*SE)
#   logistic_pred$upper <- logistic_pred$fit + (summary(logistic_model)$sigma*1.96)
#   logistic_pred$lower <- logistic_pred$fit - (summary(logistic_model)$sigma*1.96)
#
#   gompertz_pred$upper <- gompertz_pred$fit + (summary(gompertz_model)$sigma*1.96)
#   gompertz_pred$lower <- gompertz_pred$fit - (summary(gompertz_model)$sigma*1.96)
#
#   #Plot both
#   # logistic_plot <- ggplot()+
#   #   geom_point(data = data, aes(x = ChickAge, y = Mass), shape = 21, size = 3) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = lower), colour = "black", lty = 2) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = upper), colour = "black", lty = 2) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = `0.05%`), colour = "black", lty = 1) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = `99.95%`), colour = "black", lty = 1) +
#   #   geom_line(data = logistic_pred, aes(x = newdata$ChickAge, y = fit), size = 1)+
#   #   theme_classic()
#   #
#   # gompertz_plot <- ggplot()+
#   #   geom_point(data = data, aes(x = ChickAge, y = Mass), shape = 21, size = 3) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = lower), colour = "black", lty = 2) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = upper), colour = "black", lty = 2) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = `0.05%`), colour = "black", lty = 1) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = `99.95%`), colour = "black", lty = 1) +
#   #   geom_line(data = gompertz_pred, aes(x = newdata$ChickAge, y = fit), size = 1)+
#   #   theme_classic()
#   #
#   # print(cowplot::plot_grid(logistic_plot, gompertz_plot, nrow = 1))
#
#   #Observing the fit, it seems that logistic model fits better (it better models
#   #the asymptote at ~17g) The boostrap CIs for the marginal model predications are
#   #very tight because there's simply so much data We will instead used the 1.96*SE
#   #as these are wider. This is a crude method and we will need to limit our
#   #cut-offs to 0 at the youngest ages It will work for now.
#   CYACAE_cutoffs <- tibble(Species = "CYACAE", ChickAge = seq(0, 30, 1),
#                            Warning_min = ifelse(logistic_pred$lower < 0, NA, logistic_pred$lower),
#                            Warning_max = logistic_pred$upper)
#
#   chick_mass_cutoffs <- dplyr::bind_rows(PARMAJ_cutoffs, CYACAE_cutoffs) %>%
#     dplyr::mutate(Error_min = 0) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(Error_max = ifelse(Species == "PARMAJ", 44, 27)) %>%
#     tidyr::pivot_longer(cols = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
#                         names_to = "Reference", values_to = "Value")
#
#   return(chick_mass_cutoffs)
#
# }
#
# predictNLS <- function(
#   object,
#   newdata,
#   level = 0.95,
#   nsim = 10000
# )
# {
#   require(MASS, quietly = TRUE)
#
#   ## get right-hand side of formula
#   RHS <- as.list(object$call$formula)[[3]]
#   EXPR <- as.expression(RHS)
#
#   ## all variables in model
#   VARS <- all.vars(EXPR)
#
#   ## coefficients
#   COEF <- coef(object)
#
#   ## extract predictor variable
#   predNAME <- setdiff(VARS, names(COEF))
#
#   ## take fitted values, if 'newdata' is missing
#   if (missing(newdata)) {
#     newdata <- eval(object$data)[predNAME]
#     colnames(newdata) <- predNAME
#   }
#
#   ## check that 'newdata' has same name as predVAR
#   if (names(newdata)[1] != predNAME) stop("newdata should have name '", predNAME, "'!")
#
#   ## get parameter coefficients
#   COEF <- coef(object)
#
#   ## get variance-covariance matrix
#   VCOV <- vcov(object)
#
#   ## augment variance-covariance matrix for 'mvrnorm'
#   ## by adding a column/row for 'error in x'
#   NCOL <- ncol(VCOV)
#   ADD1 <- c(rep(0, NCOL))
#   ADD1 <- matrix(ADD1, ncol = 1)
#   colnames(ADD1) <- predNAME
#   VCOV <- cbind(VCOV, ADD1)
#   ADD2 <- c(rep(0, NCOL + 1))
#   ADD2 <- matrix(ADD2, nrow = 1)
#   rownames(ADD2) <- predNAME
#   VCOV <- rbind(VCOV, ADD2)
#
#   ## iterate over all entries in 'newdata' as in usual 'predict.' functions
#   NR <- nrow(newdata)
#   respVEC <- numeric(NR)
#   seVEC <- numeric(NR)
#   varPLACE <- ncol(VCOV)
#
#   ## define counter function
#   counter <- function (i)
#   {
#     if (i%%10 == 0)
#       cat(i)
#     else cat(".")
#     if (i%%50 == 0)
#       cat("\n")
#     flush.console()
#   }
#
#   outMAT <- NULL
#
#   for (i in 1:NR) {
#     counter(i)
#
#     ## get predictor values and optional errors
#     predVAL <- newdata[i, 1]
#     if (ncol(newdata) == 2) predERROR <- newdata[i, 2] else predERROR <- 0
#     names(predVAL) <- predNAME
#     names(predERROR) <- predNAME
#
#     ## create mean vector for 'mvrnorm'
#     MU <- c(COEF, predVAL)
#
#     ## create variance-covariance matrix for 'mvrnorm'
#     ## by putting error^2 in lower-right position of VCOV
#     newVCOV <- VCOV
#     newVCOV[varPLACE, varPLACE] <- predERROR^2
#
#     ## create MC simulation matrix
#     simMAT <- mvrnorm(n = nsim, mu = MU, Sigma = newVCOV, empirical = TRUE)
#
#     ## evaluate expression on rows of simMAT
#     EVAL <- try(eval(EXPR, envir = as.data.frame(simMAT)), silent = TRUE)
#     if (inherits(EVAL, "try-error")) stop("There was an error evaluating the simulations!")
#
#     ## collect statistics
#     PRED <- data.frame(predVAL)
#     colnames(PRED) <- predNAME
#     FITTED <- predict(object, newdata = data.frame(PRED))
#     MEAN.sim <- mean(EVAL, na.rm = TRUE)
#     SD.sim <- sd(EVAL, na.rm = TRUE)
#     MEDIAN.sim <- median(EVAL, na.rm = TRUE)
#     MAD.sim <- mad(EVAL, na.rm = TRUE)
#     QUANT <- quantile(EVAL, c((1 - level)/2, level + (1 - level)/2))
#     RES <- c(FITTED, MEAN.sim, SD.sim, MEDIAN.sim, MAD.sim, QUANT[1], QUANT[2])
#     outMAT <- rbind(outMAT, RES)
#   }
#
#   colnames(outMAT) <- c("fit", "mean", "sd", "median", "mad", names(QUANT[1]), names(QUANT[2]))
#   rownames(outMAT) <- NULL
#
#   cat("\n")
#
#   return(outMAT)
# }
