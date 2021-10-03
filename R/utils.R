# Make a sparse_model_matrix for fixest estimate. This only keeps the variables that are not removed from `fixest::feols`
sparse_model_matrix = function(data, fixest, gname, tname) {
	Z = NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) Z = methods::as(stats::model.matrix(fixest, data = data), "sparseMatrix")

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
		frmla <- stats::as.formula(
			paste("~ 0 + ", paste(glue::glue("factor({all.vars(fixest$fml_all$fixef)})"), collapse = " + "))
		)

		Z_fixef = Matrix::sparse.model.matrix(frmla, data = data)
		print(head(Z_fixef))

		temp = fixest::fixef(fixest)
		select =	lapply(names(temp), function(var){
			names = names(temp[[var]])
			names = names[temp[[var]] != 0]

			glue::glue("factor({var}){names}")
		})

		alt_vars = c(glue::glue("factor({gname})"),glue::glue("factor({tname})"))
		Z = cbind(Z, Z_fixef[, alt_vars])
	}

	return(Z)
}
