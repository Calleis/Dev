####################
# Analyse des produits
# analyse des descriptifs des produits pour déduire leur interet cosmetique
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

####################  Lecture 

data_products <- readRDS(file = file.path(chemin,"BDD/clean/data_products.rds"))

####################  Analyse produits 

# Toutes les descriptions sous forme de corpus
corpus <- data_products$descriptif %>%
  VectorSource()%>%
  VCorpus()

# Nettoyage
myStopWords <- c("plus","peau","peaux","jour", "tout",
                 "permet","resultats","resultat","parmi",
                 "pendant","total","egalement", "voulez")

corpus <- corpus %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords("french"),myStopWords))

# Matrice de fréquence de mots de tout le corpus
TDM <- corpus %>% TermDocumentMatrix()
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
data_terms <- data.frame(word = names(v),freq=v)
head(data_terms) 

# Réduction du nombre de mots en utilisant que les racines
stem <- wordStem(data_terms$word, language = "french")
length(unique(stem)) 

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
freq.terms <- findFreqTerms(TDM, lowfreq = 15)
plot(TDM, term = freq.terms, corThreshold = 0.12, weighting = T)

####################  Graphiques

# Plot des fréquences de mots 
data_terms$word <- factor(data_terms$word, levels = data_terms$word)
q <- ggplot(data=head(data_terms,10), aes(x=word, y=freq, fill=word)) +
  geom_bar(stat="identity")+ theme_bw() +
  scale_fill_brewer(palette="Spectral") +
  ylab(label = "Frequence") + xlab(label = "Mots")+
  geom_text(aes(label=freq), vjust=-0.3, size=3.5)+
  theme(legend.position="none") +
  ggtitle("Fréquence des mots les plus courants dans \n la description d'un produit soin visage")

# Plot nuage des mots
set.seed(1234)
wordcloud(words = data_terms$word,
          freq = data_terms$freq, 
          min.freq = 5,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))






