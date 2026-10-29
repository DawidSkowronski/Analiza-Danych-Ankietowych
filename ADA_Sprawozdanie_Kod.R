
library(Stat2Data)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggmosaic)
library(binom)

#data(SuicideChina)
SuicideChina <- read.csv('SuicideChina.csv', row.names = 1)
View(SuicideChina)

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
    Hospitalizowany = Hospitalised,
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

str(SuicideChina)

# Tablica liczności i częstości występowania
tablica_licznosc_czestosc <- function(x){
  tab_licznosc <- table(x)
  tab_czestosc <- prop.table(tab_licznosc)
  
  data.frame(
    Liczność = as.numeric(tab_licznosc),
    Częstość = round(as.numeric(tab_czestosc),3),
    row.names = names(tab_licznosc)
  )
}

tablica_licznosc_czestosc(SuicideChina$Plec)
tablica_licznosc_czestosc(SuicideChina$Metoda)
tablica_licznosc_czestosc(SuicideChina$Wyksztalcenie)


#### Barploty #####

wyk_slupkowy <- function(data, feature){
  
  bar_plt <- ggplot(data, aes(x = .data[[feature]], fill = .data[[feature]])) + 
    geom_bar() + 
    labs(x = "",
         y = "Liczność") +
    scale_fill_brewer(palette = "Set2")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          plot.title = element_text(size = 11, face = "bold"))+
    theme(legend.position = "none")+ geom_text(
      stat = "count", position = position_stack(vjust = 0.5),
      aes(label = ..count..))
  
  return(bar_plt)
}

bar_plec <- wyk_slupkowy(SuicideChina, "Plec") +
  labs(title = "Liczność z podziałem na płeć")
bar_plec

bar_edukacja <- wyk_slupkowy(SuicideChina, 'Wyksztalcenie') +
  labs(title = 'Liczność z podziałem na poziom wykształcenia')
bar_edukacja


##### Wykresy mozaikowe ####

ggplot(SuicideChina) + 
  geom_mosaic(aes(x=product(Zgon), fill = Wyksztalcenie)) +
  theme_mosaic()+
  coord_cartesian(expand = 0)+
  theme(axis.text = element_text(size = 10),
        #axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
        axis.title = element_text(size = 12, face = "bold", color = "grey40"))

ggplot(SuicideChina) + 
  geom_mosaic(aes(x=product(Zgon), fill = Metoda)) +
  theme_mosaic()


####### Lista 2 Zadanie 2 #######

# Przedział ufności dla zmiennej Zgon (1 = "sukces")
wszystkie_proby <- length(SuicideChina$Zgon)
liczba_udanych_prob <- sum(SuicideChina$Zgon)

# Realizacja przedziału ufności (wszystkie metody)
binom.confint(liczba_udanych_prob, wszystkie_proby, conf.level = 0.95)

####### Dodatkowe zadania ########

# 1

# prawdopodobieństwa pokrycia i oczekiwanej długości przedziałów ufności Cloppera-Pearsona, Walda i Wilsona.
rozmiar_proby <- c(30,50,100,500)
prawd_p <- c(.1, .5, .9)

poz_ufnosci <- .95

M <- 10000

# rbinom(M, n, p) zwróci M realizacji próby długości n o prawd sukcesu p
# wynikiem jest wektor liczby sukcesów w każdej realizacji próby

# Dwie macierze, na pokrycie i oczekiwaną długość
# Badamy 3 metody, 4 rozmiary próby i 3 prawdopodobieństwa. Daje to 36 kombinacji

metody <- c('exact','asymptotic', 'wilson')
nazwy_metod <- c("Clopper-Pearson", "Wald", "Wilson")

podsumowanie_symulacji_przedzialy <- data.frame()

for (i in seq_along(rozmiar_proby)){
    n <- rozmiar_proby[i]
    
  for (j in seq_along(prawd_p)){
    p <- prawd_p[j]

    realizacje_proby <- rbinom(M,n,p) 

    realizacje_przedzialow <- binom.confint(realizacje_proby, n, conf.level = .95, methods = metody)
    
    # Kolejno dla każdej metody
    for (indeks_m in seq_along(metody)){
      obecna_metoda <- metody[indeks_m]
      nazwa_obecnej_metody <- nazwy_metod[indeks_m]
      
      realizacje_obecnej_metody <- realizacje_przedzialow[realizacje_przedzialow$method == obecna_metoda,]
    
    # Dla każdej realizacji sprawdzamy:
    
    # Symulacyjnie - oczekiwana długość i pokrycie 
    oczekiwana_dlugosc <- mean(realizacje_obecnej_metody$upper - realizacje_obecnej_metody$lower)
    
    szacowane_pokrycie <- mean(p>=realizacje_obecnej_metody$lower & p<=realizacje_obecnej_metody$upper)
    
    # Teoretycznie - funkcją binom.coverage
    pokrycie_bi_coverage <- binom.coverage(p = p, n = n, conf.level = .95, method = obecna_metoda)$coverage
    
    wynik_dla_metody <- data.frame(
      n=n,
      p=p,
      Metoda = nazwa_obecnej_metody,
      Pokrycie_Symulacja = round(szacowane_pokrycie,4),
      Pokrycie_binom= round(pokrycie_bi_coverage,4),
      Srednia_Dlugosc = round(oczekiwana_dlugosc,4)
    )
    podsumowanie_symulacji_przedzialy <- rbind(podsumowanie_symulacji_przedzialy, wynik_dla_metody)
    }
    
  }
}

View(podsumowanie_symulacji_przedzialy)
podsumowanie_symulacji_przedzialy

ggplot(podsumowanie_symulacji_przedzialy, aes(x = factor(n), y = Pokrycie_Symulacja, fill = Metoda)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 0.8) +
  facet_wrap(~ paste("p =", p)) +
  coord_cartesian(ylim = c(0.80, 1.0)) +
  labs(
    title = "Empiryczne prawdopodobieństwo pokrycia (Monte Carlo)",
    x = "Rozmiar próby (n)",
    y = "Prawdopodobieństwo pokrycia"
  ) +
  theme_minimal()

ggplot(podsumowanie_symulacji_przedzialy, aes(x = n, y = Srednia_Dlugosc, color = Metoda, group = Metoda)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ paste("p =", p), scales = "free_y") +
  labs(
    title = "Oczekiwana długość przedziału ufności",
    x = "Rozmiar próby (n)",
    y = "Średnia szerokość przedziału"
  ) +
  theme_minimal()


# Pokrycie dla n = 30 w pełnym zakresie p:
binom.plot(n = c(30,100), method = binom.exact, main = "Clopper-Pearson (n=30)")
binom.plot(n = c(30,100), method = binom.asymp, main = "Wald (n=30)")
binom.plot(n = c(30,100), method = binom.wilson, main = "Wilson (n=30)")










