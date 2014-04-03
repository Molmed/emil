n <- 25
x <- rbind(matrix(rnorm(n*2), n, 2) + 1,
           matrix(rnorm(n*2), n, 2) \%*\% matrix(c(1, 0, 0, 2), 2, 2))
y <- gl(2, n, 2*n)
fit <- design("qda", x, y)
contour(fit, x, y)

