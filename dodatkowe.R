# Zadanie 1  - asymptotycznie

x <- sum(SuicideChina$Hospitalizowano)
n <- length(SuicideChina$Hospitalizowano)
alpha <- 0.05

p0 <- 0.5
p_hat <- x / n

T_stat <- 2 * (x * log(p_hat / p0) + (n - x) * log((1 - p_hat) / (1 - p0)))

p_value <- pchisq(T_stat, df = 1, lower.tail = FALSE)
p_value

wartosc_krytyczna <- qchisq(1 - alpha, df = 1)
wartosc_krytyczna

test <- binom.test(x, n, 0.5, alternative = "two.sided")
test$p.value

# Zadanie 1 - dokładnie
k <- 0:n

lambda <- dbinom(k, n, p0) / dbinom(k, n, k / n)
prob <- dbinom(k, n, p0)

lambda_sort <- lambda[order(lambda)]
prob_sort <- prob[order(lambda)]

cum_prob <- cumsum(prob_sort)
j <- which(cum_prob >= alpha)[1]

lambda0 <- lambda_sort[j]
gamma <- (alpha - sum(prob[lambda < lambda0])) / sum(prob[lambda == lambda0])

K0 <- k[lambda < lambda0]
K_gamma <- k[lambda == lambda0]

T_stat <- dbinom(x, n, p0) / dbinom(x, n, x / n)

p_value <- sum(prob[lambda <= T_stat])
p_value

# Zadanie 2
xK <- sum(SuicideChina$Zgon[SuicideChina$Plec == "kobieta"] == 1)
xM <- sum(SuicideChina$Zgon[SuicideChina$Plec == "mężczyzna"] == 1)

nK <- sum(SuicideChina$Plec == "kobieta")
nM <- sum(SuicideChina$Plec == "mężczyzna")

test <- prop.test(
  x = c(xK, xM),
  n = c(nK, nM),
  alternative = "two.sided",
  correct = FALSE
)

test

pK <- xK / nK
pM <- xM / nM
p <- (xK + xM) / (nK + nM)

log_lambda <- (xK + xM) * log(p) +
  (nK + nM - xK - xM) * log(1 - p) -
  xK * log(pK) -
  xM * log(pM) -
  (nK - xK) * log(1 - pK) -
  (nM - xM) * log(1 - pM)

T_stat <- -2 * log_lambda

p_value <- pchisq(T, df = 1, lower.tail = FALSE)
p_value

