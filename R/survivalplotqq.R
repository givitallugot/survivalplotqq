#' Plot Q-Q plot for survival data
#'
#' Draw 8 different Q-Q plots to inference which distribution is the best to fit the survival data.
#' List of Candidate distributions: Weibull, Log-Normal, Gompertz, Gumbel, Normal, Pareto, Exponential, Laplace.

#' @param data Data Frame with 3 variables such as x(variable of interest), tag(binary 0 or 1, survival or not) and lnx(log x)
#'
#' @return
#' @export

plotqq.survival <- function(data){

  data$pr <- rank(data$x)/(nrow(data) + 1)
  data$norm_x <- qnorm(data$pr) # ln은 log-normal
  data$gompertz_x <- log(-log(1-data$pr)) # weibull
  data$gumbel_x <- -log(-log(data$pr)) # Frechet
  data$exp_x <- -log(1-data$pr) # ln은 pareto
  data$laplace_x <- rmutil::qlaplace(data$pr)
  data$logis_x <- log(data$pr/(1-data$pr)) # log-logistic

  weibull_x <- ggplot2::ggplot(data, ggplot2::aes(x=gompertz_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q Weibull", subtitle = paste("R.sq = ", round(summary(lm(lnx ~ gompertz_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  lognorm_x <- ggplot2::ggplot(data, ggplot2::aes(x=norm_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q Log-Normal", subtitle = paste("R.sq = ", round(summary(lm(lnx ~ norm_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  gompertz_x <- ggplot2::ggplot(data, ggplot2::aes(x=gompertz_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q Gompertz", subtitle = paste("R.sq = ", round(summary(lm(x ~ gompertz_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  gumbel_x <- ggplot2::ggplot(data, ggplot2::aes(x=gumbel_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q Gumbel", subtitle = paste("R.sq = ", round(summary(lm(x ~ gumbel_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  norm_x <- ggplot2::ggplot(data, ggplot2::aes(x=norm_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q Normal", subtitle = paste("R.sq = ", round(summary(lm(x ~ norm_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  pareto_x <- ggplot2::ggplot(data, ggplot2::aes(x=exp_x, y=lnx)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q plot: Pareto Distribution", subtitle = paste("R.sq = ", round(summary(lm(lnx ~ exp_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  exp_x <- ggplot2::ggplot(data, ggplot2::aes(x=exp_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q plot: Exponential Distribution", subtitle = paste("R.sq = ", round(summary(lm(x ~ exp_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()
  laplace_x <- ggplot2::ggplot(data, ggplot2::aes(x=laplace_x, y=x)) + ggplot2::geom_point(size=2) + ggplot2::geom_smooth(method = "lm", se=FALSE) + ggplot2::labs(title = "Q-Q plot: Laplace Distribution", subtitle = paste("R.sq = ", round(summary(lm(x ~ logis_x, data))$r.squared,4)*100, "%", sep = "")) + ggplot2::theme_light()

  gridExtra::grid.arrange(weibull_x, lognorm_x, gompertz_x, gumbel_x, norm_x, laplace_x, pareto_x, exp_x, ncol=3)

}
