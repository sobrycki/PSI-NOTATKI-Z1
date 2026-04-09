

#' ---
#' title: "Zajęcia 7"
#' author: "Autor: SO"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: darkly
#'     highlight: breezedark
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide
#'     number_sections: true
#' ---


# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury slow
library(factoextra)   # Wizualizacje klastrow
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele



# Dane tekstowe ----

# Ustaw Working Directory!
# Zaladuj dokumenty z folderu
docs <- DirSource("C:/Users/so469349/Desktop/textfolder")
# W razie potrzeby dostosuj scieżke
# np.: docs <- DirSource("C:/User/Documents/textfolder")


# Utworz korpus dokumentow tekstowych
corpus <- VCorpus(docs)


### Gdy tekst znajduje sie w jednym pliku csv:
### data <- read.csv("file.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
### corpus <- VCorpus(VectorSource(data$text))


# Korpus
inspect(corpus)


# Korpus - zawartosc przykladowego elementu
corpus[[1]]
corpus[[1]][[1]][7:9]
corpus[[1]][2]




# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usuniecie zbednych znakow ----


# Zapewnienie kodowania w calym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))



# Funkcja do zamiany znakow na spacje
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usun zbedne znaki lub pozostalosci url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze slowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAlY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukosnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostalosc po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostalosci
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]][7:9]



corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]][7:9]

# usuniecie ewt. zbednych nazw wlasnych
corpus <- tm_map(corpus, removeWords, c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", "javi", "javis", "reed", "josh", "joshs", "elliot", "elliots", "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug"))
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]][7:9]




# Stemming ----

# zachowaj kopie korpusu 
# do użycia jako dictionary w uzupelnianiu rdzeni
corpus_copy <- corpus

# wykonaj stemming w korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)


# Sprawdzenie
corpus[[1]][[1]][7:9]
# Sprawdzenie
corpus_stemmed[[1]][[1]][7:9]



# Uzupelnienie rdzeni slow po stemmingu ----

# funkcja pomocnicza: wykonuje stemCompletion linia po linii
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  # podziel na slowa
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") # uzupelnij rdzenie
  paste(x, collapse = " ")                       # polacz z powrotem w tekst
})

# wykonaj stemCompletion do każdego dokumentu w korpusie
corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usun NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)


# Sprawdzenie
corpus_completed[[1]][[1]][1]

# Porownaj:
corpus[[1]][[1]][7:9]
corpus_stemmed[[1]][[1]][7:9]



# Decyzja dotyczaca korpusu ----
# Należy w tym momencie rozważyc, 
# ktory obiekt użyc do dalszej analizy:
#
# - corpus (oryginalny, bez stemmingu)
# - corpus_stemmed (po stemmingu)
# - corpus_completed (uzupelnione rdzenie)





# Tokenizacja ----


# Macierze czestosci TDM i DTM ----


# a) Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus_completed)
tdm
inspect(tdm)


tdm_m <- as.matrix(tdm)

tdm_m[1:5, 1:5]
# Można zapisac TDM w pliku .csv
# write.csv(tdm_m, file="TDM.csv")


# b) Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus_completed)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

dtm_m[1:5, 1:5]
# Można zapisac DTM w pliku .csv
# write.csv(dtm_m, file="DTM.csv")



# 2. Zliczanie czestosci slow ----
# (Word Frequency Count)

# Można zliczyc same czestosci slow w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)



# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura slow (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyswietl top 10
print(head(tdm_df, 10))




# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja slow i dokumentow w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)


# - podejscie surowych czestosci slow
# (czestosc slowa = liczba wystapien w dokumencie)
# (Raw Word Counts)



# Użyj utworzonej wczesniej macierzy DTM
dtm

inspect(dtm)

dtm_m[1:5, 1:5]




# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)



# Klastrowanie k-srednich (k-means) ----


# Dobor liczby klastrow
# Metoda sylwetki (silhouette)
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobor liczby klastrow", subtitle = "Metoda sylwetki")



# Wykonaj klastrowanie kmeans
# (sprawdź wyniki dla k = 3,4,5)
set.seed(123) # ziarno losowe dla replikacji wynikow



# a) Ustaw liczbe klastrow k = 2 ----
k <- 2 # ustaw liczbe klastrow


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrow
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrow dokumentow")



# Interaktywna tabela z przypisaniem dokumentow i top 5 slow
# Dla każdego klastra: liczba dokumentow oraz top 5 slow
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentow = length(cluster_docs_idx),
    Top_5_slow = top_words,
    stringsAsFactors = FALSE
  )
})

# Polacz wszystko w ramke danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentow z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentow do klastrow
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dolaczamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pelnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczestsze slowa i licznosc klastrow",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury slow dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentow w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plikow odpowiadajace dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmure slow dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura slow - Klaster", i))
}




# a) Przypisanie dokumentow do klastrow ----
document_names <- names(corpus)  # Nazwy dokumentow z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentow do klastrow

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podglad
print(documents_clusters)


# a) Wizualizacja przypisania dokumentow do klastrow ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentow do klastrow",
       x = "Dokument",
       y = "Liczba wystapien (powinna wynosic 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)








# b) Ustaw liczbe klastrow k = 3 ----
k <- 3 # ustaw liczbe klastrow


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrow
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrow dokumentow")



# Interaktywna tabela z przypisaniem dokumentow i top 5 slow
# Dla każdego klastra: liczba dokumentow oraz top 5 slow
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentow = length(cluster_docs_idx),
    Top_5_slow = top_words,
    stringsAsFactors = FALSE
  )
})

# Polacz wszystko w ramke danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentow z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentow do klastrow
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dolaczamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pelnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczestsze slowa i licznosc klastrow",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury slow dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentow w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plikow odpowiadajace dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmure slow dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura slow - Klaster", i))
}




# b) Przypisanie dokumentow do klastrow ----
document_names <- names(corpus)  # Nazwy dokumentow z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentow do klastrow

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podglad
print(documents_clusters)


# b) Wizualizacja przypisania dokumentow do klastrow ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentow do klastrow",
       x = "Dokument",
       y = "Liczba wystapien (powinna wynosic 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)





# c) Ustaw liczbe klastrow k = 4 ----
k <- 4 # ustaw liczbe klastrow


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrow
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrow dokumentow")



# Interaktywna tabela z przypisaniem dokumentow i top 5 slow
# Dla każdego klastra: liczba dokumentow oraz top 5 slow
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentow = length(cluster_docs_idx),
    Top_5_slow = top_words,
    stringsAsFactors = FALSE
  )
})

# Polacz wszystko w ramke danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentow z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentow do klastrow
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dolaczamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pelnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczestsze slowa i licznosc klastrow",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury slow dla każdego klastra
for (i in 1:k) {
  # znajdź indeksy dokumentow w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plikow odpowiadajace dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmure slow dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura slow - Klaster", i))
}




# c) Przypisanie dokumentow do klastrow ----
document_names <- names(corpus)  # Nazwy dokumentow z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentow do klastrow

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podglad
print(documents_clusters)


# c) Wizualizacja przypisania dokumentow do klastrow ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentow do klastrow",
       x = "Dokument",
       y = "Liczba wystapien (powinna wynosic 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)






