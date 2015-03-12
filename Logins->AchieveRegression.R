# Uses the dumbest.variable from NoZLogins->Archive.R

  fit <- lm(avg.progress ~ logins, data = dumbest.variable)
  summary(fit)

# predicted values
  summary(fitted(fit))

# residuals
  summary(residuals(fit))

# anova
  anova(fit)

# covariance matrix
  cor.test(dumbest.variable$avg.progress, dumbest.variable$logins)
