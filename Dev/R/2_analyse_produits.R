####################
# Analyse des produits
# analyse des descriptifs des produits 
# pour déduire leur interet
#################### 

library("tm")
library("tidytext")
library("tidyverse")
# devtools::install_github("ThinkRstat/stopwords")
library("stopwords")
library("wordcloud")
library("SnowballC")
library(Rgr)

chemin <- "/home/menyssa/Calleis"

read.


# 2- Analyse des descriptifs produits


# Toutes les descriptions sous forme de corpus
corpus <- data_products$descriptif %>%
  VectorSource()%>%
  VCorpus()

# Nettoyage
corpus <- corpus %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, stopwords("french"))

# Matrice de fréquence de mots de tout le corpus
TDM <- corpus %>% TermDocumentMatrix()
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
data_terms <- data.frame(word = names(v),freq=v)
head(data_terms) # 8248 mots

# Réduction du nombre de mots en utilisant que les racines
stem <- wordStem(data_terms$word, language = "french")
length(unique(stem)) # 5748 mots

# Plot des fréquences de mots 
barplot(height=head(data_terms,10)$freq,
        names.arg=head(data_terms,10)$word, 
        xlab="Mots", ylab="Fréquence", 
        col="#973232", 
        main="Fréquence des mots dans la description de tous les produits soin visage")

# Plot nuage des mots
set.seed(1234)
wordcloud(words = data_terms$word, freq = data_terms$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Création du dictionnaire: 
# Pour tous ces mots rechercher les associations cohérentes
# Etape 1 : récupérer les produits vides pour un item concerné
# Etape 2 : matrice pour chaque mot le vocabulaire correspondant
# Etape 3 : attribuer l'effet ou non 
terms <- c("pollution", "hydratation","age","ride", "liftant",  
           "matifiant","parfum","teint","protections","tache_brune","rougeur",
           "matin","soir","acne", "lotion", "masque_gommage", "serum",
           "cicatrice", "lavant","sublimateur","correcteur", "anticerne",
           "anti_poche","yeux_levres", "lotion_oculaire","mecanique", 
           "chimique", "moussant", "purifiant", "rincage","filmogene",
           "contour_des_yeux","nourrissant","traitant", "protection",
           "protection_froid","colore","contour","repulpant","exfoliant",
           "enfant", "rincage_eau","tous_maquillage", "nettoyant",
           "tous_type_maquillage","apaisant","rafraichissant", 
           "sans_alcool","protection_solaire")

associations <- findAssocs(TDM, terms = terms, corlimit = 0.2)
freq.terms <- findFreqTerms(TDM, lowfreq=15)
plot(TDM, term = freq.terms, corThreshold = 0.12, weighting = T)










