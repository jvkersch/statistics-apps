newcombe_2 <- function(theta_hat, m, n, alpha = 0.05) {
  newcombe_explicit(theta_hat, m - 1, n - 1, m, n, alpha)
}

newcombe_3 <- function(theta_hat, m, n, alpha = 0.05) {
  newcombe_implicit(theta_hat, m - 1, n - 1, m, n, alpha)
}

newcombe_4 <- function(theta_hat, m, n, alpha = 0.05) {
  m_star <- n_star <- (m + n)/2 - 1
  newcombe_explicit(theta_hat, m_star, n_star, m, n, alpha)
}

newcombe_5 <- function(theta_hat, m, n, alpha = 0.05) {
  m_star <- n_star <- (m + n)/2 - 1
  newcombe_implicit(theta_hat, m_star, n_star, m, n, alpha)
}

newcombe_explicit <- function(theta_hat, m_star, n_star, m, n, alpha) {
  z <- qnorm(1 - alpha/2)
  lower <- theta_hat - z*sqrt(V(theta_hat, m_star, n_star, m, n))
  upper <- theta_hat + z*sqrt(V(theta_hat, m_star, n_star, m, n))
  c(lower=lower, upper=upper)
}

newcombe_implicit <- function(theta_hat, m_star, n_star, m, n, alpha) {

  find_root <- function(upper) {
    if (upper) {
      z <- qnorm(1 - alpha/2)
      interval <- c(theta_hat, 1.0)
    } else {
      z <- -qnorm(1 - alpha/2)
      interval <- c(0.0, theta_hat)
    }

    res <- uniroot(function(theta) {
      theta_hat - theta + z*sqrt(V(theta, m_star, n_star, m, n))
    }, interval = interval, check.conv = TRUE)
    res$root
  }

  c(lower=find_root(upper = FALSE), upper=find_root(upper = TRUE))
}

V <- function(theta, m_star, n_star, m, n) {
  theta*(1 - theta)*(1 + n_star*(1-theta)/(2-theta)
                       + m_star*theta/(1+theta))/m/n
}

