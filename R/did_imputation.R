#' Borusyak, Jaravel, and Spiess (2021) Estimator
#'
#' Treatment effect estimation and pre-trend testing in staggered adoption
#' diff-in-diff designs with an imputation approach of Borusyak, Jaravel, and
#' Spiess (2021)
#'
#' @import fixest
#'
#' @param data A data frame
#' @param yname String. Variable name for outcome.
#' @param idname String. Variable name for unique unit id
#' @param gname String. Variable name for unit-specific date of treatment
#'   (never-treated should be zero or `NA`)
#' @param tname String. Variable name for calendar period
#' @param first_stage Formula for Y(0).
#'   Formula following \code{\link[fixest:feols]{fixest::feols}}.
#'   Fixed effects specified after "`|`".
#'   If not specified, then just unit and time fixed effects will be used.
#' @param weights Estimation weights for observations. This is used in estimating
#'   Y(0) and also augments treatment effect weights.
#' @param wtr Character vector of treatment weight names
#'   (see horizon for standard static and event-study weights)
#' @param horizon Integer vector of event_time or `TRUE`. This only applies if `wtr` is left
#'   as `NULL`. if specified, weighted averages/sums of treatment effects will be
#'   reported for each of these horizons separately (i.e. tau0 for the treatment
#'   period, tau1 for one period after treatment, etc.).
#'   If `TRUE`, all horizons are used.
#'   If `wtr` and `horizon` are null, then the static treatment effect is calculated.
#' @param pretrends Integer vector or `TRUE`. Which pretrends to estimate.
#'   If `TRUE`, all `pretrends` are used.
#'
#' @export
#'
#' @section Examples:
#'
#'
#' Load example dataset which has two treatment groups and homogeneous treatment effects
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Load Example Dataset
#' data("df_hom", package="did2s")
#' ```
#'
#' You can run a static TWFE fixed effect model for a simple treatment indicator
#' ```{r, comment = "#>", collapse = TRUE}
#' did_imputation(data = df_hom, yname = "dep_var", gname = "g",
#'                tname = "year", idname = "unit")
#' ```
#'
#' Or you can use relative-treatment indicators to estimate an event study estimate
#' ```{r, comment = "#>", collapse = TRUE}
#' did_imputation(data = df_hom, yname = "dep_var", gname = "g",
#'                tname = "year", idname = "unit", horizon=TRUE)
#' ```
#'
#' Here's an example using data from Castle (2013)
#' ```{r, comment = "#>", collapse = TRUE}
#' # Castle Data
#' castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did_imputation(data = castle, yname = "l_homicide", gname = "effyear",
#'                first_stage = ~ 0 | sid + year,
#'                tname = "year", idname = "sid")
#' ```
#'
did_imputation = function(data, yname, gname, tname, idname, first_stage = NULL,
						  weights = NULL, wtr = NULL, horizon = NULL,
						  pretrends = NULL, return_df = FALSE){


	# Set-up Parameters ------------------------------------------------------------
	orig.df = data
	data = as.data.frame(data)

	# Extract vars from formula
	if(is.null(first_stage)) {
		first_stage = glue::glue("0 | {idname} + {tname}")
	} else if(inherits(first_stage, "formula")) {
		first_stage = as.character(first_stage)[[2]]
	}

	# Treat
	data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)
	data[is.na(data$zz000treat), "zz000treat"] = 0

	# Create event time
	data$zz000event_time = ifelse(
		is.na(data[[gname]]) | data[[gname]] == 0 | data[[gname]] == Inf,
		-Inf,
		as.numeric(data[[tname]] - data[[gname]])
	)

	# Get list of event_time
	event_time = unique(data$zz000event_time)
	event_time = event_time[is.finite(event_time)]

	# horizon/allhorizon options
	if(is.null(wtr)) {

		# event-study
		if(!is.null(horizon)) {
			# create event time weights
			wtr = c()

			# allhorizons
			if(all(horizon == TRUE)) horizon = event_time

			# Create wtr of horizons
			for(e in event_time) {
				if(e %in% horizon) {
					if(e >= 0) {
						var = paste0("zz000wtr", e)

						wtr = c(wtr, var)

						data[,var] = 1*(data$zz000event_time == e & !is.na(data$zz000event_time))
					}
				}
			}
			# static
		} else {
			wtr = "zz000wtrtreat"

			data[, wtr] = 1*(data$zz000treat == 1)
		}
	}

	# Weights specified or not
	if(is.null(weights)) {
		weights_vector = rep(1, nrow(data))
	} else {
		weights_vector = data[[weights]]
	}

	for(w in wtr) {
		# Treatment weights * weights_vector
		data[[w]] = data[[w]] * weights_vector
		# Normalize weights
		data[[w]] = data[[w]]/sum(data[[w]])
	}

	# First Stage estimate ---------------------------------------------------------

	# First stage among untreated
	formula = stats::as.formula(glue::glue("{yname} ~ {first_stage}"))

	# Estimate Y(0) using untreated observations
	first_stage_est = fixest::feols(formula, se = "standard",
									data[data$zz000treat == 0,],
									weights = weights_vector[data$zz000treat == 0],
									warn=FALSE, notes=FALSE)

	# Residualize outcome variable
	data$zz000adj = data[[yname]] - stats::predict(first_stage_est, newdata = data)

	# Point estimate for wtr
	est = c()
	for(w in wtr) {
		# \sum w_{it} * \tau_{it}
		est = c(est,
				sum(
					data[data$zz000treat == 1,][[w]] * data[data$zz000treat == 1,][["zz000adj"]]
				))
	}


	# Standard Errors --------------------------------------------------------------

	# Create Zs
	Z = sparse_model_matrix(data, first_stage_est, gname=gname, tname=tname)

	# Equation (6) of Borusyak et. al. 2021
	# - Z (Z_0' Z_0)^{-1} Z_1' wtr_1
	v_star = make_V_star(
		(Z * weights_vector),
		(Z * weights_vector)[data$zz000treat == 0, ],
		(Z * weights_vector)[data$zz000treat == 1, ],
		Matrix::Matrix(as.matrix(data[data$zz000treat == 1, wtr]), sparse = TRUE)
	)

	se = c()
	for(i in 1:length(wtr)) {

		# Calculate v_it^* = - Z (Z_0' Z_0)^{-1} Z_1' * w_1
		data$zz000v = v_star[, i]

		# fix v_it^* = w for treated observations
		data[data$zz000treat == 1, "zz000v"] = data[data$zz000treat == 1,][[wtr[i]]]

		# Equation (10) of Borusyak et. al. 2021
		# Calculate tau_it - \bar{\tau}_{et}

		# \bar{\tau}_{et}
		split_et <- split(data, list(data[[gname]], data$zz000event_time), drop = T)
		results <- lapply(split_et, function(x) {
			temp = ifelse(
				x$zz000treat == 1,
				sum(x$zz000v^2 * x$zz000adj)/sum(x$zz000v^2) * x$zz000treat,
				0
			)

			temp = ifelse(is.nan(temp), 0, temp)

			x$zz000tau_et = temp

			return(x)
		})
		data = do.call("rbind", results)

		# Recenter tau by \bar{\tau}_{et}
		data$zz000tau_centered = data$zz000adj - data$zz000tau_et


		# Equation (8)
		# Calculate variance of estimate
		split_id <- split(data, data[[idname]], drop = T)
		results <- lapply(split_id, function(x) {
			temp = sum(x$zz000v * x$zz000tau_centered)^2
			return(temp)
		})

		variance = sum(unlist(results))

		se = c(se, sqrt(variance))
	}


	# Pre-event Estimates ----------------------------------------------------------

	if(!is.null(pretrends)) {
		if(all(pretrends == TRUE)) {
			pre_formula <- stats::as.formula(glue::glue("{yname} ~ i(zz000event_time) + {first_stage}"))
		} else {
			if(all(pretrends %in% event_time)) {
				pre_formula <- stats::as.formula(
					glue::glue("{yname} ~ i(zz000event_time, keep = c({paste(pretrends, collapse = ', ')}))  + {first_stage}")
				)
			} else {
				stop(glue::glue("Pretrends not found in event_time. Event_time has values {event_time}"))
			}
		}

		pre_est <- fixest::feols(pre_formula, data[data$zz000treat == 0, ], weights = weights_vector[data$zz000treat == 0], warn=FALSE, notes=FALSE)
	}


	# Create dataframe of results in tidy format -----------------------------------

	# Fix term for horizon option
	wtr = stringr::str_replace(wtr, "zz000wtr", "")

	out <- dplyr::tibble(
		term      = wtr,
		estimate  = est,
		std.error = se,
		conf.low  = est - 1.96 * se,
		conf.high = est + 1.96 * se
	)
	out$term = as.character(out$term)

	if(!is.null(pretrends)) {
		pre_out <- broom::tidy(pre_est)

		pre_out$term = stringr::str_remove(pre_out$term, "zz000event_time::")
		pre_out$term = as.character(pre_out$term)

		pre_out$conf.low = pre_out$estimate - 1.96 * pre_out$std.error
		pre_out$conf.high = pre_out$estimate + 1.96 * pre_out$std.error

		pre_out = pre_out[,c("term", "estimate", "std.error", "conf.low", "conf.high")]

		out = dplyr::bind_rows(pre_out, out)
	}
	if(return_df){
		orig.df$centered_tau <- data$zz000tau_centered
		orig.df$tau <- data$zz000adj
		orig.df$est.v <- data$zz000v
		return(list("out" = out, "data" = orig.df))

		}
		

	return(out)
}
