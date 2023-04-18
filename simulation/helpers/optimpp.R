optimpp <- function(f, par, lower, upper, n_eval = 5, max_iter = 5,
                    tol = 1e-5) {
  par <- seq(lower, upper, length.out = n_eval)
  step <- head(diff(par), 1)
  val <- sapply(par, f)
  best <- which.min(val)
  best_par <- par[best]
  best_val <- val[best]
  cur_best_par <- best_par
  cur_best_val <- best_val
  message("Current best estimate: ", round(cur_best_val, 4), 
          " @ ", round(cur_best_par, 4))
  iter <- 0
  while ((iter < max_iter) && (step > tol)) {
    iter <- iter + 1
    par <- seq(max(cur_best_par - step, lower), 
               min(cur_best_par + step, upper), 
               length.out = n_eval)
    step <- head(diff(par), 1)
    val <- sapply(par, f)
    best <- which.min(val)
    best_par <- par[best]
    best_val <- val[best]
    if (best_val < cur_best_val) {
      cur_best_val <- best_val
      cur_best_par <- best_par
      message("Improved estimate: ", round(cur_best_val, 4), 
              " @ ", round(cur_best_par, 4))
    }
  }
  
  list(
    par = best_par,
    value = best_val,
    step = step,
    iter = iter,
    lower = lower, upper = upper,
    n_eval = n_eval, max_iter = max_iter, tol = tol,
    converge_before_max_iter = iter < max_iter
  )
}
