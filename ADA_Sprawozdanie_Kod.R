library(Stat2Data)
library(dplyr)

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


# =================== #
  #### Lista 4 #####
# =================== #

#### Lista 4 Zadanie 1 ####

test_creesie_reed <- function(tabela, lambda = 2/3){
  
  R <- nrow(tabela)
  C <- ncol(tabela)
  n <- sum(tabela)
  # O = n_ij - observed (czyli komórka w tabeli kontyngencji)
  # E = n_i+ * n_+j / n = n*hat_p_ij^(H0)- expected
    # hat_p_ij^(H0) to estymator NW prawdopodobieństwa w i-tej komórce,
    # przy prawdziwości H0
  E <- outer(rowSums(tabela), colSums(tabela)) / n
  
  if (lambda == 0){
  # Gdy lambda równa 0  
    statystyka_I <-  2*sum(tabela * log(tabela / E))
    
  }else if (lambda == -1){
  # Gdy lambda równa -1 
    statystyka_I <-  2*sum(E * log(E / tabela))
    
  } else {
  # Gdy lambda różna od {0, -1}
    statystyka_I <- 2/(lambda*(lambda+1)) * sum(tabela * ((tabela/E)^lambda-1))
}
  
  k <- R*C # liczba możliwych zestawów odpowiedzi
  q <- (R-1) + (C-1) # liczba estymowanych parametrów
  
  # Liczba stopni swobody
  df <- k-q-1
  p_wartosc <- 1- pchisq(statystyka_I, df = df)
  
  # Funkcja zwraca wartość statystyki testowej I_lambda oraz p-wartość
  return(data.frame(
    Lambda = lambda,
    Statystyka_I = round(statystyka_I, 4),
    Stopnie_swobody = df,
    p_value = p_wartosc
  ))
}


#### Lista 4 Zadanie 2 ####

# Załóżmy lambda = 2/3 - sugestia autorów

# Tabela zgony od wykształcenia
tabela_zgon_wyksztalcenie <- table(SuicideChina$Zgon, SuicideChina$Wyksztalcenie)
tabela_zgon_wyksztalcenie

wynik_testu_CR <- test_creesie_reed(tabela_zgon_wyksztalcenie, lambda = 2/3)
wynik_testu_CR


# Tabela hospitalizowano od wykształcenie

tabela_hospitalizacja_wyksztalcenie <- table(SuicideChina$Hospitalizowano, SuicideChina$Wyksztalcenie)
tabela_hospitalizacja_wyksztalcenie
test_creesie_reed(tabela_hospitalizacja_wyksztalcenie, lambda = 2/3)

# Tabela Zgon vs Metoda

tabela_zgon_metoda <- table(SuicideChina$Zgon, SuicideChina$Metoda)
tabela_zgon_metoda # Występują zera dla utonięcia
test_creesie_reed(tabela_zgon_metoda, lambda = 2/3)

# Tabela Zgon vs Miasto

tabela_zgon_miasto <- table(SuicideChina$Zgon, SuicideChina$Miasto)
tabela_zgon_miasto

test_creesie_reed(tabela_zgon_miasto, lambda = 2/3)
test_creesie_reed(tabela_zgon_miasto, lambda = -1/2)


# Tabela Zgon vs Płeć

tabela_zgon_plec <- table(SuicideChina$Zgon, SuicideChina$Plec)
tabela_zgon_plec

test_creesie_reed(tabela_zgon_plec, lambda = 2/3)

# Tabela Zgon vs Zawod
tabela_zgon_zawod <- table(SuicideChina$Zgon, SuicideChina$Zawod)
tabela_zgon_zawod # Trzeba połączyć jakoś emeryt, robotnik z innymi kategoriami

test_creesie_reed(tabela_zgon_zawod, lambda = 2/3)


# ===================== #
####### LISTA 5 #########
# ===================== #

#### Lista 5 Zadanie 1 ####

dane_lista5 <- SuicideChina

table(dane_lista5$Wyksztalcenie)

# Łączymy niepiśmienny + podstawowe i średnie + wyższe
# Tworzymy nową zmienną w danych

dane_lista5$Wyksztalcenie_zgrupowane <- ifelse(
  dane_lista5$Wyksztalcenie%in% c('niepiśmienny', 'podstawowe'),
  'niskie',
  'wysokie')

# Zamiana na faktor
dane_lista5$Wyksztalcenie_zgrupowane <- factor(
  dane_lista5$Wyksztalcenie_zgrupowane, levels = c('niskie','wysokie'))

table(dane_lista5$Wyksztalcenie_zgrupowane)




# Tabela liczności dla cech Zgon i Wykształcenie_zgrupowane
zgon_edu_zgrupowane <- table(dane_lista5$Zgon,
                                  dane_lista5$Wyksztalcenie_zgrupowane)

rownames(zgon_edu_zgrupowane) <-  c('Nie', 'Tak')
zgon_edu_zgrupowane

# Proporcje zgonów w grupach wykształcenia
# Szacujemy prawdopodobieństwo zgonu w grupie o niskim poziomie wykształcenia
# oraz w grupie o wysokim poziomie wykształcenia
proporcje_zgon_edu_zgrupowane <- prop.table(zgon_edu_zgrupowane, margin =2) # Proporcje w kolumnach
round(proporcje_zgon_edu_zgrupowane,4)

# Test o równości prawdopodobieństwa 'sukcesu' (Zgonu) w dwóch grupach różniących się wykształceniem
prop.test(x = zgon_edu_zgrupowane['Tak',], # liczba 'sukcesów'
          n = colSums(zgon_edu_zgrupowane), # liczba osób w danej grupie wykształcenia
          alternative = 'two.sided', correct = FALSE)
# Odrzucamy hipotezę H0 o równości prawdopodobieństwa zgonu w grupach o różnym poziomie wykształcenia



# Tabele z uwzględnieniem zmiennej Płeć
dane_kobiety <- dane_lista5[dane_lista5$Plec=='kobieta',]
dane_mezczyzni <- dane_lista5[dane_lista5$Plec=='mężczyzna',]

# Kobiety
K_zgon_edukacja <- table(dane_kobiety$Zgon, dane_kobiety$Wyksztalcenie_zgrupowane)
rownames(K_zgon_edukacja) <-  c('Nie', 'Tak')

proporcja_K <- prop.table(K_zgon_edukacja, margin = 2)
round(proporcja_K,4)
test_K <- prop.test(x=K_zgon_edukacja['Tak',],
                    n = colSums(K_zgon_edukacja),
                    alternative = 'two.sided', correct = FALSE)
test_K

# Mężczyźni
M_zgon_edukacja <- table(dane_mezczyzni$Zgon, dane_mezczyzni$Wyksztalcenie_zgrupowane)
rownames(M_zgon_edukacja) <-  c('Nie', 'Tak')

proporcja_M <- prop.table(M_zgon_edukacja, margin = 2)
round(proporcja_M,4)
test_M <- prop.test(x = M_zgon_edukacja['Tak',],
                    n = colSums(M_zgon_edukacja),
                    alternative = 'two.sided', correct = FALSE)
test_M

# Wykształcenie wpływa na zgon
# zarówno w całej populacji, jak i w grupach z podziałem na płeć

# Zależność między wykształceniem a ryzykiem zgonu
# jest istotna statystycznie i ma taki sam kierunek niezależnie od płci


# Nie występuje paradoks Simpsona
# Niezależnie od tego czy uwzględniamy Płeć czy nie, prawdopodobieństwo Zgonu
# w grupie o niższym poziomie wykształcenia jest większe




#### Zadanie 2 Lista 5 ####

# Słowny opis modeli [1 3], [13], [1 2 3], [12 3], [12 13], [1 23]

# Zamiast modelu [13] powinien być chyba [2 13] (chyba że nie uwzględniamy zmiennej 2)



#### Zadanie 3 Lista 5 ####

# Weryfikujemy 3 hipotezy zerowe

# a) H0: M = [1 2 3]
# b) H0: M = [1 23]
# c) H0: M = [13 23]

# W każdym przypadku a) b) c) hipotezą alternatywną jest model pełny H1: M = [123]

# Tworzymy tabelę z trzema pytaniami

tab_zgon_plec_edukacja <- table(dane_lista5$Plec,
                                     dane_lista5$Zgon,
                                     dane_lista5$Wyksztalcenie_zgrupowane)


# a) H0: M = [1 2 3]
model_a_loglin <- loglin(tab_zgon_plec_edukacja, margin = list(1,2,3),
                         param = TRUE, fit = TRUE)
# param = TRUE - zwrócenie oszacowanych parametrów modelu log-liniowego (lambdy)
# fit = TRUE - zwrócenie oczekiwanych liczności w komórkach tabeli (przy prawdziwości H0)
print(model_a_loglin)

# Wyznaczenie p-wartości
a_pval <- 1 - pchisq(model_a_loglin$lrt, df = model_a_loglin$df)
a_pval


# b) H0: M = [1 23]
model_b_loglin <- loglin(tab_zgon_plec_edukacja, margin = list(1,c(2,3)),
                         param = TRUE, fit = TRUE)

b_pval <- 1 - pchisq(model_b_loglin$lrt, df = model_b_loglin$df)
b_pval


# c) H0: M = [13 23]
model_c_loglin <- loglin(tab_zgon_plec_edukacja, margin = list(c(1,3),c(2,3)),
                         param = TRUE, fit = TRUE)

c_pval <- 1 - pchisq(model_c_loglin$lrt, df = model_c_loglin$df)
c_pval

# We wszystkich 3 hipotezach odrzucamy H0
# Oznacza to, że przyjęcie założenia o całkowitej, wzajemnej niezależności Zgonu, Płci i Wykształcenia
# prowadzi do istotnego pogorszenia dopasowania do danych względem modelu pełnego (hipotezy alternatywnej).
# Przynajmniej niektóre z tych zmiennych muszą być ze sobą wzajemnie zależne



# Jeśli zamienimy miejscami cechy w tabeli
tab_zgon_plec_edukacja_ver2 <- table(dane_lista5$Zgon,
                                dane_lista5$Wyksztalcenie_zgrupowane,
                                dane_lista5$Plec)

# a) H0: M = [1 2 3]
model_a_loglin <- loglin(tab_zgon_plec_edukacja_ver2, margin = list(1,2,3),
                         param = TRUE, fit = TRUE)
print(model_a_loglin)

# Wyznaczenie p-wartości
a_pval <- 1 - pchisq(model_a_loglin$lrt, df = model_a_loglin$df)


# b) H0: M = [1 23]
model_b_loglin <- loglin(tab_zgon_plec_edukacja_ver2, margin = list(1,c(2,3)),
                         param = TRUE, fit = TRUE)

b_pval <- 1 - pchisq(model_b_loglin$lrt, df = model_b_loglin$df)
b_pval


# c) H0: M = [13 23]
# Czyli sprawdzamy czy zgon i wykształcenie warunkowo niezależne od płci
model_c_loglin <- loglin(tab_zgon_plec_edukacja_ver2, margin = list(c(1,3),c(2,3)),
                         param = TRUE, fit = TRUE)

c_pval <- 1 - pchisq(model_c_loglin$lrt, df = model_c_loglin$df)
c_pval



# Jeśli jeszcze raz zamienimy miejscami cechy w tabeli

tab_zgon_plec_edukacja_ver3 <- table(dane_lista5$Zgon,
                                     dane_lista5$Plec,
                                     dane_lista5$Wyksztalcenie_zgrupowane)

# a) H0: M = [1 2 3]
model_a_loglin <- loglin(tab_zgon_plec_edukacja_ver3, margin = list(1,2,3),
                         param = TRUE, fit = TRUE)
# Wyznaczenie p-wartości
a_pval <- 1 - pchisq(model_a_loglin$lrt, df = model_a_loglin$df)


# b) H0: M = [1 23]
# Czyli sprawdzamy czy płeć niezależna od zgonu i wykształcenia
model_b_loglin <- loglin(tab_zgon_plec_edukacja_ver3, margin = list(1,c(2,3)),
                         param = TRUE, fit = TRUE)

b_pval <- 1 - pchisq(model_b_loglin$lrt, df = model_b_loglin$df)
b_pval


# c) H0: M = [13 23]
model_c_loglin <- loglin(tab_zgon_plec_edukacja_ver3, margin = list(c(1,3),c(2,3)),
                         param = TRUE, fit = TRUE)

c_pval <- 1 - pchisq(model_c_loglin$lrt, df = model_c_loglin$df)
c_pval


# Żaden z tych prostszych modeli nie opisuje dobrze danych

# Co gdyby zamiast H1: MODEL PEŁNY dać jakiś prostszy nadmodel?


# Model H0 (np. [13 23])
model_H0 <- loglin(tab_zgon_plec_edukacja, margin = list(c(1,3), c(2,3)), fit = TRUE)

# Model H1 - nadmodel (np. [12 13 23] - dodajemy interakcję 1 i 2)
model_H1 <- loglin(tab_zgon_plec_edukacja, margin = list(c(1,2), c(1,3), c(2,3)), fit = TRUE)

# Statystyka G^2 dla różnicy modeli
G2_diff <- model_H0$lrt - model_H1$lrt

# Różnica stopni swobody
df_diff <- model_H0$df - model_H1$df

p_val_diff <- 1 - pchisq(G2_diff, df = df_diff)
p_val_diff
# P-value bliskie 0, czyli rozbudowa modelu o nowe parametry (interakcję 12)
# istotnie poprawia jego dopasowanie do danych




#### Zadanie 4 Lista 5 ####

# Dla wybranej trójki zmiennych z analizowanego zbioru danych, wybrać najlepszy
# model w oparciu o kryterium a) AIC b) BIC

# Z istnienia i jednoznaczności oszacowania NW, możemy wykorzystać zarówno loglin() jak i glm()

# Korzystamy z glm() do funkcji step()
# Później wracamy do loglin() aby mieć interpretację identyczną jak w 

# Tabela z pierszej wersji
tab_zgon_plec_edukacja <- table(dane_lista5$Plec,
                                dane_lista5$Zgon,
                                dane_lista5$Wyksztalcenie_zgrupowane)


# Ze skryptu: w praktyce modele log-liniowe stosuje się najczęściej do zmiennych
# losowych o rozkładzie Poissona lub współrzędnych wektora o rozkładzie wielomianowym

# Konieczna konwersja do data frame dla glm()
df_tab <- as.data.frame(tab_zgon_plec_edukacja)
colnames(df_tab) <- c("Plec", "Zgon", "Wyksztalcenie", 'Freq')
df_tab # Freq to liczność w każdej z komórek

# Tworzymy model pełny, z którego będziemy usuwać zależności
model_pelny <- glm(Freq ~ Plec*Zgon*Wyksztalcenie, data = df_tab, family = poisson)

# Wybór według AIC
najlepszy_aic <- step(model_pelny, direction = 'backward', k=2) # k=2 domyślna kara dla AIC
summary(najlepszy_aic)

# Wybór według BIC
najlepszy_bic <- step(model_pelny, direction = 'backward', k=log(sum(df_tab$Freq))) # k=log(n) domyślna kara dla BIC
summary(najlepszy_bic)



# Najlepszy model AIC to [12 23] 
# Najlepszy model BIC to [12 23] 
# 1 - Płeć , 2- Zgon, 3 - Wykształcenie
# W obu przypadkach model jest ten sam

# Wracamy do loglin

najlepszy_model <- loglin(tab_zgon_plec_edukacja, margin = list(c(1,2), c(2,3)),
                          param = TRUE, fit = TRUE)

najlepszy_parametry <- najlepszy_model$param
# Jak interpretować wartości tych parametrów?

oczekiwane_licznosci <- najlepszy_model$fit

liczba_badanych <- sum(tab_zgon_plec_edukacja)

# Oszacowanie prawdopodobieństwa p_ijk
p_ijk <- oczekiwane_licznosci/liczba_badanych
round(p_ijk,4)

# Prawdopodobieństwo wylosowania kobiety z niskim wykształceniem, która zmarła, wynosi około 18,8%
# Prawdopodobieństwo wylosowania mężczyzny z niskim wykształceniem, który zmarł, wynosi około 21,1%

