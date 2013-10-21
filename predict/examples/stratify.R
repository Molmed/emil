set.seed(123)

x <- data.frame(type=gl(3, 10, labels=LETTERS[1:3]),
                X1 = floor(3*runif(30)),
                X2 = rnorm(30))

# Single group
group <- stratify(x, list(type=c("A", "C"), X1=2))
print(x[group,])

# Multiple groups
groups <- stratify(x, list(A.high = list(type="A", X1=2),
                           A.low = list(type="A", X1=0),
                           B = list(type="B"),
                           all = list()))
sapply(groups, sum)

