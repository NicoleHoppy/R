# Analiza win

---
title: "Analiza zbioru danych zawierającego informacje o winach pochodzących z północy Portugalii"
output:
  html_document:
    df_print: paged
    toc: true #włącza spis treści
    #toc_float: true #umożliwia przewijany spis treści
    number_sections: true #numeruje sekcje
  pdf_document: default
---


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

# Dane - ich struktura oraz klasyfikacja

Załadujmy plik winequalityN.csv oraz prześledźmy jego strukturę danych.
