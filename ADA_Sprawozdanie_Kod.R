
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

M <- 10


macierz_realizacji_prob <- matrix(NA, nrow = M, ncol = 4 )
macierz_realizacji_prob

for (i in 1:M){
  
} 













