
library(Stat2Data)
library(dplyr)


distance_correlation_test <- function(x, y, M=1000){
  # Wektory X i Y to nie liczności, tylko pojedyncze obserwacje dla każdego pacjenta
  df <- data.frame(x= x, y= y)
  x_wektor <- df$x
  y_wektor <- df$y
  
  # Liczność populacji
  n <- nrow(df)
  
  # Tworzymy tabelę kontyngencji
  tabela <- table(x_wektor,y_wektor)
  
  # Estymatory prawdopodobieństw brzegowych
  pi_ <- rowSums(tabela)/n
  p_j <- colSums(tabela)/n
  
  # Zliczanie licznika
  policz_licznik <- function(X,Y){
    tabela_X_Y <- table(X,Y)
    # Estymator
    p_ij <- tabela_X_Y/n
    
    # Oczekiwane prawdopodobieństwa przy prawdziwości niezależności (iloczyn prawdopodobieństw brzegowych)
    oczekiwane_prawd <- outer(rowSums(tabela_X_Y)/n, colSums(tabela_X_Y)/n)
    return( sqrt(sum((p_ij - oczekiwane_prawd)^2)) )
  }
  
  # Mianownik (stała wartość)
  pi_2 <- sum(pi_^2)
  pi_3 <- sum(pi_^3)
  p_j_2 <- sum(p_j^2)
  p_j_3 <- sum(p_j^3)
  
  mianownik <- (pi_2*(pi_2+1)-2*pi_3)^(1/4) * (p_j_2*(p_j_2+1)-2*p_j_3)^(1/4)
  
  # Wartosć statystyki T
  licznik <- policz_licznik(x_wektor, y_wektor)
  statystyka_T <- licznik / mianownik
  
  # wektor na wyniki permutacji
  permutacje_wektor <- numeric(M)
  
  for (i in 1:M){
    # Wykonujemy wiele permutacji jednego wektora (np. y)
    # Permutacje nie zmieniają rozkładów brzegowych w tabeli
    # Możemy analizować tylko licznik, mianownik podczas permutacji pozostaje taki sam
    
    y_permutacje <- sample(y_wektor)
    
    permutacje_wektor[i] <- policz_licznik(x_wektor, y_permutacje)
  }
  
  # p-value
  p_val <- sum(permutacje_wektor>licznik)/M
  
  return(list(
    Statystyka_T = statystyka_T,
    p_value = p_val
  ))
}

x <- rbinom(100,1,1/2)
y <- rbinom(100,1,1/2)
z <- x*y

distance_correlation_test(x,y, M=2000)
distance_correlation_test(x,z, M=2000)

#### Dane #####

#SuicideChina <- read.csv('SuicideChina.csv')
data(SuicideChina)

SuicideChina <- SuicideChina |>
  select(-Person_ID, -Year, -Month) |>
  mutate(
    Education = recode(Education, "iliterate" = "illiterate")
  ) |>
  filter(
    Urban != "unknown",
    Education != "unknown",
    Occupation != "others/unknown",
    Occupation != "others",
    method != "Others",
    method != "unspecified"
  ) |>
  mutate(
    Hospitalised = recode(Hospitalised, "yes" = 1L, "no" = 0L),
    Died = recode(Died, "yes" = 1L, "no" = 0L),
    method = recode(method, "Poison unspec" = "Other poison"),
    Education = factor(
      Education,
      levels = c("illiterate", "primary", "Secondary", "Tertiary"),
      ordered = TRUE
    )
  ) |>
  mutate(
    across(where(is.factor), droplevels)
  ) |>
  rename(
    Hospitalizowano = Hospitalised,
    Zgon = Died,
    Miasto = Urban,
    Plec = Sex,
    Wiek = Age,
    Wyksztalcenie = Education,
    Zawod = Occupation,
    Metoda = method
  ) |>
  mutate(
    Miasto = recode(Miasto, "no" = 0L, "yes" = 1L),
    Plec = recode(Plec, "female" = "kobieta", "male" = "mężczyzna"),
    Wyksztalcenie = factor(
      recode(
        as.character(Wyksztalcenie),
        "illiterate" = "niepiśmienny",
        "primary" = "podstawowe",
        "Secondary" = "średnie",
        "Tertiary" = "wyższe"
      ),
      levels = c("niepiśmienny", "podstawowe", "średnie", "wyższe"),
      ordered = TRUE
    ),
    Zawod = recode(
      Zawod,
      "business/service" = "handel/usługi",
      "farming" = "rolnictwo",
      "household" = "gospodarstwo domowe",
      "professional" = "wolny zawód",
      "retiree" = "emeryt",
      "student" = "uczeń/student",
      "unemployed" = "bezrobotny",
      "worker" = "robotnik"
    ),
    Metoda = recode(
      Metoda,
      "Cutting" = "podcięcie",
      "Drowning" = "utonięcie",
      "Hanging" = "powieszenie",
      "Jumping" = "skok",
      "Other poison" = "inna trucizna",
      "Pesticide" = "pestycyd"
    )
  )

tabela_zgon_wyksztalcenie <- table(SuicideChina$Zgon, SuicideChina$Wyksztalcenie)
tabela_zgon_wyksztalcenie


tabela_zgon_wyksztalcenie
distance_correlation_test(SuicideChina$Zgon,
                          SuicideChina$Wyksztalcenie,
                          M=2000)

distance_correlation_test(SuicideChina$Zgon,
                          SuicideChina$Plec,
                          M=2000)

chisq.test(SuicideChina$Miasto, SuicideChina$Zgon)
distance_correlation_test(SuicideChina$Miasto,
                          SuicideChina$Zgon,
                          M=2000)

chisq.test(SuicideChina$Miasto, SuicideChina$Plec)
table(SuicideChina$Miasto, SuicideChina$Plec)
distance_correlation_test(SuicideChina$Miasto,
                          SuicideChina$Plec,
                          M=2000)



