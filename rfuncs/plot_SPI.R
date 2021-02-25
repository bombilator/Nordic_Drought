plot_SPI = function (x, lag_spi,...) 
{
  label <- ifelse(as.character(x$call)[1] == "spei", "SPEI", 
    "SPI")
  ser <- ts(as.matrix(x$fitted[-c(1:x$scale), ]), end = end(x$fitted), 
    frequency = frequency(x$fitted))
  ser[is.nan(ser - ser)] <- 0
  se <- ifelse(ser == 0, ser, NA)
  tit <- dimnames(x$coefficients)[2][[1]]
  if (start(ser)[2] == 1) {
    ns <- c(start(ser)[1] - 1, 12)
  }
  else {
    ns <- c(start(ser)[1], start(ser)[2] - 1)
  }
  if (end(ser)[2] == 12) {
    ne <- c(end(ser)[1] + 1, 1)
  }
  else {
    ne <- c(end(ser)[1], end(ser)[2] + 1)
  }
  n <- ncol(ser)
  if (is.null(n)) 
    n <- 1
  par(mar = c(4, 4, 2, 1) + 0.1)
  if (n > 1 & n < 5) 
    par(mfrow = c(n, 1))
  if (n > 1 & n >= 5) 
    par(mfrow = c({
      n + 1
    }%/%2, 2))
  for (i in 1:n) {
    datt <- ts(c(0, ser[, i], 0), frequency = frequency(ser), 
      start = ns, end = ne)
    datt.pos <- ifelse(datt > 0, datt, 0)
    datt.neg <- ifelse(datt <= 0, datt, 0)
    plot(datt, type = "n", xlab = "", ylab = label, main = lag_spi) # main = tit[i]
    if (!is.null(x$ref.period)) {
      k <- ts(5, start = x$ref.period[1, ], end = x$ref.period[2, 
        ], frequency = 12)
      k[1] <- k[length(k)] <- -5
      polygon(k, col = "light grey", border = NA, density = 20)
      abline(v = x$ref.period[1, 1] + (x$ref.period[1, 
        2] - 1)/12, col = "grey")
      abline(v = x$ref.period[2, 1] + (x$ref.period[2, 
        2] - 1)/12, col = "grey")
    }
    grid(col = "black")
    polygon(datt.pos, col = "black", border = NA)
    polygon(datt.neg, col = "black", border = NA)
    lines(datt, col = "dark grey")
    abline(h = 0)
    points(se, pch = 21, col = "white", bg = "black")
    # title = lag_spi
  }
}
