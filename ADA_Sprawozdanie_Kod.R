

###### Zadanie 3 ########

# Przeprowadzić symulacje, których celem jest porównanie estymatorów πˆ, π˜
# prawdopodobieństwa pozytywnej odpowiedzi na pytanie wrażliwe postaci


# Dla różnych pi, q, n
# Wyznaczyć: obciążenia i błędy MSE

pi_hat_funct <- function(p_hat, q) {
  wynik <- p_hat / (2 * q - 1) + (q - 1) / (2 * q - 1)
  return(wynik)
}

pi_wave_funct <- function(p_hat, q) {
  # pmax dla wielu wektorów
  wynik <- pmax(pmin(pi_hat_funct(p_hat = p_hat, q = q), 1), 0)
  return(wynik)
}


n <- c(10, 100)
q <- c(.6, .9)
pi <- c(.1, .3, .5)
M <- 1000

symulacja_rrt <- function(n, q, pi, M = 1000) {
  # Prawdziwe prawd odpowiedzi "tak"
  p_prawdziwe <- (2 * q - 1) * pi + 1 - q
  
  # Wygenerowanie M prób z dwumianowego
  proby_odp_tak <- rbinom(M, size = n, prob = p_prawdziwe)
  p_hat <- proby_odp_tak / n
  
  # Estymatory
  pi_hat <- pi_hat_funct(p_hat, q)
  pi_wave <- pi_wave_funct(p_hat, q)
  
  # Obciążenie
  pi_hat_bias <- mean(pi_hat) - pi
  pi_wave_bias <- mean(pi_wave) - pi
  
  # MSE
  pi_hat_mse <- mean((pi_hat - pi)^2)
  pi_wave_mse <- mean((pi_wave - pi)^2)
  
  # pi_hat jest nieobciążony, MSE = Var + bias^2
  teoretyczne_pi_hat_bias <- 0
  teoretyczne_pi_hat_mse <-  ((2 * q - 1) * pi + 1 - q) * (q - (2 * q -
                                                                  1) * pi) / (n * (2 * q - 1)^2)
  
  data.frame(
    n = n,
    q = q,
    pi = pi,
    
    Bias_hat = pi_hat_bias,
    Bias_wave = pi_wave_bias,
    Bias_hat_teor = teoretyczne_pi_hat_bias,
    
    MSE_hat = pi_hat_mse,
    MSE_wave = pi_wave_mse,
    MSE_hat_teor = teoretyczne_pi_hat_mse
  )
}

symulacja_rrt_wektory <- function(wek_n, wek_q, wek_pi, M = 1000) {
  wyniki <- data.frame()
  
  for (i in seq_along(wek_n)) {
    n <- wek_n[i]
    
    for (j in seq_along(wek_q)) {
      q <- wek_q[j]
      
      for (k in seq_along(wek_pi)) {
        pi <- wek_pi[k]
        
        jedna_symulacja_wynik <- symulacja_rrt(
          n = n,
          q = q,
          pi = pi,
          M = M
        )
        
        wyniki <- rbind(wyniki, jedna_symulacja_wynik)
      }
    }
  }
  return(wyniki)
}

df_wyniki_symulacji_dane_wrazliwe <- symulacja_rrt_wektory(
  wek_n = n,
  wek_q = q,
  wek_pi = pi,
  M = 1000
)

df_wyniki_symulacji_dane_wrazliwe <- round(df_wyniki_symulacji_dane_wrazliwe, 4)

View(df_wyniki_symulacji_dane_wrazliwe)



#### Zadania do policzenia (Lista 3) ####

#Zadanie 1
# Wyprowadzić postać testu ilorazu wiarogodności do weryfikacji hipotezy 
# H0 : p = p0 ,przy hipotezie alternatywnej H1 : p ̸= p0

test_iw_jeden <- function(x,n, p0){
  # x - liczba sukcesów, n - rozmiar próby
  
  wartosci_k <- 0:n
  
  # Estymator NW parametru p dla zadanego k
  p_hat_k <- wartosci_k/n
  
  # Wyznaczenie ilorazu wiarygodności
  stat_lambda <- dbinom(wartosci_k, n, p0) / dbinom(wartosci_k, n, p_hat_k)
  
  lambda_obserwowane <- stat_lambda[x+1] # bo liczymy sukcesy od 0

  # dokładna wartość poziomu krytycznego
  # Sumujemy te wartości k, których IW jest nie większy niż zaobserwowane IW
  p_val <- sum(dbinom(wartosci_k[round(stat_lambda, 10) <= round(lambda_obserwowane, 10)], n, p0))
  # Zaokrąglenia aby zniwelować błędy numeryczne
  
  return(p_val)
}

# p wartość z dokładnego testu IW
test_iw_jeden(2,5,0.2)

# p wartość z funkcji binom.test
binom.test(2,5,0.2, alternative = 'two.sided')


sukcesy <- 10
liczba_obserwacji <- 100
prawd_p0 <- 0.2

test_iw_jeden(sukcesy, liczba_obserwacji, prawd_p0)
binom.test(sukcesy, liczba_obserwacji, prawd_p0, alternative = 'two.sided')$p.value

## Zadanie 2 (do policzenia) ##

test_iw_dwie_proby <- function(x, y, n1, n2){
  p1_hat <- x/n1
  p2_hat <- y/n2
  p_hat <- (x+y)/(n1+n2)
  
  # Iloraz lambda
  licznik <- dbinom(x,n1, p_hat) * dbinom(y,n2,p_hat)
  mianownik <- dbinom(x,n1, p1_hat) * dbinom(y,n2,p2_hat)
  
  lambda_obs <- licznik/mianownik
  
  statystyka_wilks <- -2*log(lambda_obs)
  
  p_val <- 1 - pchisq(statystyka_wilks, df = 1)
  return(p_val)
}

test_iw_dwie_proby(50,30,
                   100,100)

prop.test(x = c(50,30), n = c(100,100), alternative = 'two.sided')

