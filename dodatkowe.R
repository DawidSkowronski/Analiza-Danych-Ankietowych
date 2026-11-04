# Zadanie 1 dodatkowe

x <- sum(SuicideChina$Hospitalizowano)
n <- length(SuicideChina$Hospitalizowano)

p0 <- 0.5
p_hat <- x / n

T_stat <- 2 * (x * log(p_hat / p0) + (n - x) * log((1 - p_hat) / (1 - p0)))

p_value <- pchisq(T_stat, df = 1, lower.tail = FALSE)
p_value

wartosc_krytyczna <- qchisq(1 - 0.05, df = 1)
wartosc_krytyczna

test <- binom.test(x, n, 0.5, alternative = "two.sided")
test$p.value
