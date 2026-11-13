alpha <- 0.05
### H0: p = p0
p0 <- 0.5

# Z naszego zbioru danych
x <- 579
n <- 1184

# wbudowany
test <- binom.test(x, n, 0.5, alternative = "two.sided")
test

# IW - astymptiotycznie
p_hat <- x / n

T_stat <- 2 * (x * log(p_hat / p0) + (n - x) * log((1 - p_hat) / (1 - p0)))

wartosc_krytyczna <- exp(-0.5 * qchisq(1 - alpha, df = 1)) # powrót do lambda(x)
wartosc_krytyczna

p_value <- pchisq(T_stat, df = 1, lower.tail = FALSE)
p_value

# IW - dokładnie
k <- 0:n

# Logarytm ilorazu wiarygodności
log_lambda <- k * log(p0) + (n - k) * log(1 - p0) - k * log(k / n) - (n -
                                                                        k) * log(1 - k / n)

log_lambda[is.nan(log_lambda)] <- 0 # bo log(0) jest nieokreślone

# Prawdopodobieństwa pod H0 - logarytm
log_prob <- lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) + k * log(p0) + (n -
                                                                                 k) * log(1 - p0)
prob <- exp(log_prob)

# Sortowanie według ilorazu wiarygodności
ord <- order(log_lambda)
log_lambda <- log_lambda[ord]
prob <- prob[ord]

# Wyznaczenie log(lambda0) i lambda0
suma <- cumsum(prob)
log_lambda0 <- log_lambda[which(suma >= alpha)[1]]
lambda0 <- exp(log_lambda0)

# Wyznaczenie gamma
gamma <- (alpha - sum(prob[log_lambda < log_lambda0])) / sum(prob[log_lambda == log_lambda0])

# Iloraz dla zaobserwowanego x
log_lambda_x <- x * log(p0) + (n - x) * log(1 - p0) - x * log(x / n) - (n -
                                                                          x) * log(1 - x / n)
lambda_x <- exp(log_lambda_x)

lambda0
gamma

### H0: p1 = p2

# Z naszego zbioru danych
#x <- sum(SuicideChina$Zgon[SuicideChina$Plec == "kobieta"] == 1)
#y <- sum(SuicideChina$Zgon[SuicideChina$Plec == "mężczyzna"] == 1)

#n1 <- sum(SuicideChina$Plec == "kobieta")
#n2 <- sum(SuicideChina$Plec == "mężczyzna")

x <- 579
y <- 650
n1 <- 1184
n2 <- 1102

# IW - asymptotpycznie

p1 <- x / n1
p2 <- y / n2
p0 <- (x + y) / (n1 + n2)

TStat <- 2 * (
  x*log(p1/p0) +
    (n1-x)*log((1-p1)/(1-p0)) +
    y*log(p2/p0) +
    (n2-y)*log((1-p2)/(1-p0))
)

wart_kryt <- qchisq(1-alpha, df=1)
p_value <- pchisq(TStat, df=1, lower.tail=FALSE)

wart_kryt
TStat
p_value

# JNMN
s <- x + y
xx <- max(0, s - n2):min(n1, s)
p <- dhyper(xx, m = n1, n = n2, k = s)

pary <- combn(xx, 2)

test <- data.frame()

for (j in 1:ncol(pary)) {
  c1 <- pary[1, j]
  c2 <- pary[2, j]
  
  A <- sum(p[xx < c1 | xx > c2])
  B <- sum(xx[xx < c1 | xx > c2] * p[xx < c1 | xx > c2])
  C <- p[xx == c1]
  D <- p[xx == c2]
  
  g1 <- (c2 * (alpha - A) - alpha * n1 * s / (n1 + n2) + B) /
    ((c2 - c1) * C)
  
  g2 <- (alpha * n1 * s / (n1 + n2) - B - c1 * (alpha - A)) /
    ((c2 - c1) * D)
  
  if (g1 >= 0 && g1 <= 1 && g2 >= 0 && g2 <= 1) {
    test <- rbind(test, data.frame(
      c1 = c1,
      c2 = c2,
      gamma1 = g1,
      gamma2 = g2
    ))
  }
}

print(test)
# x  nie należy między c1 i c2 zatem na zadanym poziomie istotności 0.05
# odrzucamy hipotezę zerową

# Zad 1 dokładny

N <- n1 + n2
s <- x + y

i <- max(0, s - n2):min(n1, s - n2 + n2)
j <- s - i

p <- s / N

lambda <- exp(
  dbinom(i, n1, i / n1, log = TRUE) +
    dbinom(j, n2, j / n2, log = TRUE) -
    dbinom(i, n1, p, log = TRUE) -
    dbinom(j, n2, p, log = TRUE)
)

prob <- dhyper(i, s, N - s, n1)

ord <- order(lambda)
lambda <- lambda[ord]
prob <- prob[ord]

skumulowane <- cumsum(prob)
k <- which(skumulowane >= alpha)[1]

lambda0 <- lambda[k]
gamma <- (alpha - sum(prob[lambda < lambda0])) /
  sum(prob[lambda == lambda0])

lambda0
gamma
