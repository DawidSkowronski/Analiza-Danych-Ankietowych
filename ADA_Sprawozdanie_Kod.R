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
