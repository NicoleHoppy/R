## 1. Analiza win

Zbiór danych wykorzystany do poniższej analizy pochodzi ze strony [kaggle.com](https://www.kaggle.com/datasets/shelvigarg/wine-quality-dataset/data). Jest to zbiór zawierający dane o czerwonym oraz białym wariancie wina "Vinho Verde", pochodzącego z północy Portugalii.

Celem niniejszej analizy jest zastosowanie *regresji liniowej*.

*Regresja liniowa* to metoda statystyczna, wykorzystywana do badania zależności między jedną zmienną zależną a jedną lub większą liczbą zmiennych niezależnych. Polega ona na próbie dopasowania linii do danych, aby zrozumieć charakter relacji między nimi.

Załadujemy na początek potrzebne biblioteki, żeby funkcje, które są wykorzystywane w projekcie, działały poprawnie.

```{r,message = FALSE, warning = FALSE}
library(car)
library(corrplot)
library(dplyr)
library(faraway)
library(lmtest)
library(MASS)
library(nortest)
library(RColorBrewer)
```

## 2. Dane - ich struktura oraz klasyfikacja

Załadujmy plik winequalityN.csv oraz prześledźmy jego strukturę danych.

Załadujmy plik winequalityN.csv oraz prześledźmy jego strukturę danych.

```{r}
wine <- read.csv("C:\\Users\\Nikola\\Documents\\Nikola Chmielewska\\R\\Datasets\\winequalityN.csv")
str(wine)
```
```{r}
unique(wine$type)
```

Na podstawie powyższej komendy widzimy, że zbiór danych zawiera również informacje o wariancie wina: czerwonym i białym.

Dokonajmy jeszcze objaśnienia zmiennych występujących w zbiorze danych.

| Nazwa | Opis | 
|:--------|:--------|
| type | rodzaj wina (białe, czerwone) |
| fixed.acidity | stała kwasowość |
| volatile acidity | zmienna kwasowość |
| citric acid | kwas cytrynowy |
| residual sugar | resztowy cukier |
| chlorides |	chlorki |
| free sulfur dioxide | 	wolny dwutlenek siarki |
| total sulfur dioxide | 	całkowity dwutlenek siarki |
| density |	gęstość |
| pH – potential of hydrogen | współczynnik kwasowości/zasadowości pH |
| sulphates | siarczyny |
| alcohol | alkohol |
| quality |	jakość |

Oraz, aby lepiej zrozumieć dane, zobaczmy ich podsumowanie:

```{r}
summary(wine)
```

Sprawdźmy jeszcze, czy w zbiorze wszystkie dane są podane:

```{r}
(sapply(wine, function(x) {sum(is.na(x))}))
```
Jak widać powyżej, nasza tabela zawiera wartości NA. Usuniemy zatem wiersze, które nie posiadają danych, ponieważ w kontekście całego zbioru danych jest to marginalna ilość. Robimy to, ponieważ brak danych może obniżyć jakość późniejszej analizy danych.

```{r}
wine_clean <- na.omit(wine) #usuwa wiersze zawierające wartości NA
```

Wyświetlmy jeszcze raz nasz nowy zbiór danych z usuniętymi wartościami NA.

```{r}
(sapply(wine_clean, function(x) {sum(is.na(x))}))
```

Jak widzimy, w tej chwili żadna zmienna nie jest obarczona brakiem danych, więc nasz zbiór danych jest gotowy do obróbki.

Jednocześnie w powyższym zestawie danych zmienną zależną jest atrybut quality, a więc ocena jakości wina. Pozostałe zmienne są zmiennymi niezależnymi, ponieważ wpływają one na końcową ocenę wina; są to również zmienne ilościowe. Natomiast zmienną objaśnianą ‘quality’ można potraktować zarówno jako zmienną ilościową oraz jako zmienną nominalną o uporządkowanych kategoriach (jakościową). 

W niniejszej pracy zmienną 'quality' potraktujemy jako zmienną ilościową, rozważając modele regresji liniowej. Jednak podczas tworzenia modelu proporcjonalnych szans, zmienną ‘quality’ potraktujemy jako zmienną jakościową. Warto w tym miejscu zaznaczyć, że w celu weryfikacji hipotez będziemy zakładać poziom istotności *α = 0,05*.
