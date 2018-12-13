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

####################  Découpage des descriptions 

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
inspect(TDM)

# Récupère tous les mots du corpus par documents 
DF <- tidy(TDM)
DF <- DF[order(DF$term, DF$document),]

m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
data_terms <- data.frame(word = names(v),freq=v)
head(data_terms) 
length(unique(data_terms$word))

# Réduction du nombre de mots en utilisant que les racines
stem <- wordStem(data_terms$word, language = "french")
length(unique(stem)) 

####################  Analyse description produits 
# Pour tous ces mots rechercher les associations cohérentes
# Etape 1 : champs lexical de l'effet recherché
# Etape 2 : vérification si la description du mot contient un des mots
# Etape 3 : attribuer l'effet au produit

# Pollution
findAssocs(TDM, terms = "pollution", corlimit = 0.2)
mots <- c("pollution","subit","agressions","stress")
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$protection_pollution[as.numeric(docs$document)] <- "Oui"

# Hydratation 
findAssocs(TDM, terms = "hydratation", corlimit = 0.2)
mots <- c(unique(grep(pattern = "hydra",DF$term,value = T)),
          "eau", "desalterer","aqua","aquaserum")
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$hydratant[as.numeric(docs$document)] <- "Oui"

# Age
findAssocs(TDM, terms = "age", corlimit = 0.2)
mots <- c("age","agees","ageing","antiage","antiages","antiger",
          "signes","agefocus","ride","slow","evolution","jeune","premiers")
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$anti_age[as.numeric(docs$document)] <- "Oui"

# Ride
findAssocs(TDM, terms = "antiride", corlimit = 0.2)
unique(grep(pattern = "ride",DF$term,value = T))
mots <- c("ride","rides","antiride","antirides","antiridesnaturellement",
          "antiridesserum", "deride", "derider","hyalurides","ridee",
          "ridestachesfermete", "reduites","extraferme", "reduite","combleurs",
          "aging","extrafermete","ameliorees", "marquees",
          "ridules","fermete","lisses","lisse","hyaluronique","antiage","botulique")
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$anti_ride[as.numeric(docs$document)] <- "Oui"

# Liftant 
findAssocs(TDM, terms = "liftant", corlimit = 0.2)
mots <- c("lifting","liftant","lisse","lissee","progeriliftr",
          "actilift",unique(grep(pattern = "lift",DF$term,value = T)))
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$liftant[as.numeric(docs$document)] <- "Oui"

terms <- c("protection_pollution", "hydratant","anti_age","anti_ride", "liftant",  
           "matifiant","no_parfum","teint","protections_uv","tache_brune","rougeur",
           "matin","soir","acne", "lotion", "masque_gommage", "serum",
           "cicatrice", "lavant","sublimateur","correcteur", "anticerne",
           "anti_poche","yeux_levres", "lotion_oculaire","mecanique", 
           "chimique", "moussant", "purifiant", "rincage","filmogene",
           "contour_des_yeux","nourrissant","traitant", "protection",
           "protection_froid","colore","contour","repulpant","exfoliant",
           "enfant", "rincage_eau","tous_maquillage", "nettoyant",
           "tous_type_maquillage","apaisant","rafraichissant", 
           "sans_alcool","protection_solaire")

# Matifiant 
findAssocs(TDM, terms = "matifiant", corlimit = 0.2)
mots <- unique(grep(pattern = "matif",DF$term,value = T))
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$matifiant[as.numeric(docs$document)] <- "Oui"

# Parfum
findAssocs(TDM, terms = "teint", corlimit = 0.2)
mots <- c("teint","unifie","eclatant","lumineux","",
          "parfumeur","delicat","fleuri","subtil")
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$no_parfum[as.numeric(docs$document)] <- "Non"

# Acne
findAssocs(TDM, terms = "acne", corlimit = 0.2)
mots <- c("acne", "boutons", "bouton","comedons","cicatrisante","cicatrices")
docs <- data.frame(DF[which(DF$term %in% mots),"document"])
data_products$acne[as.numeric(docs$document)] <- "Oui"

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

# Plot les relations entre les mots les plus fréqents
freq.terms <- findFreqTerms(TDM, lowfreq = 300)
vtxcnt <- rowSums(cor(as.matrix(t(TDM[freq.terms,])))>.1)-1
mycols<-c("#f7fbfe","#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5", "#084594")
vc <- mycols[vtxcnt+1]
names(vc) <- names(vtxcnt)
plot(TDM, term = freq.terms,
     corThreshold = 0.1, 
     weighting = F,
     nodeAttrs=list(fillcolor=vc))





