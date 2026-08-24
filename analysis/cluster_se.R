# ==============================================================================
# Respondent-clustered covariance for conditional logit models fitted with logitr
#
# Each respondent completes eight choice tasks, so the tasks are not independent
# observations. logitr's default standard errors treat them as if they were,
# which understates uncertainty. The mixed logit is unaffected because it is
# estimated with panelID set, so its likelihood already accounts for the panel
# structure; only the conditional logit and the subgroup CLs need this.
#
# Implements the standard cluster-robust sandwich: bread %*% meat %*% bread,
# where the meat sums the per-observation score contributions within respondent
# before taking the cross-product.
#
#   score_ij = (y_ij - P_ij) * x_ij      for alternative j in choice task i
#
# Source this file and call cluster_se(model, long_data, cluster_id).
# ==============================================================================

cluster_vcov <- function(model, data, cluster) {
  cf <- coef(model)
  X  <- as.matrix(data[, names(cf), drop = FALSE])

  # Multinomial choice probabilities within each choice task
  ex  <- exp(as.numeric(X %*% cf))
  den <- ave(ex, data$obsID, FUN = sum)
  P   <- ex / den

  # Per-alternative scores, summed within cluster (respondent)
  S  <- (data$chosen - P) * X
  Sr <- rowsum(S, group = cluster)

  bread <- solve(-model$hessian)
  bread %*% crossprod(Sr) %*% bread
}

cluster_se <- function(model, data, cluster) {
  sqrt(diag(cluster_vcov(model, data, cluster)))
}

# Sanity check: sqrt(diag(bread)) must reproduce logitr's own standard errors.
# If this fails, the Hessian convention has changed and the sandwich is wrong.
check_bread <- function(model, tol = 1e-6) {
  implied <- sqrt(diag(solve(-model$hessian)))
  reported <- logitr::se(model)[names(implied)]
  max(abs(implied - reported)) < tol
}
