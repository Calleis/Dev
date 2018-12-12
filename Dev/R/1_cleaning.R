####################
# Cleaning des fichiers commentaires et produits
#################### 

library(data.table)
library(stringr)
library(VIM)

chemin <- "/home/menyssa/Calleis"

source(file = file.path(chemin, "Dev/R/Toolbox/text_cleaning.R"))
source(file = file.path(chemin, "Dev/R/Toolbox/text_mining.R"))


####################  Lecture 

data_comments <- fread(file = file.path(chemin, "BDD/raw/df_comments_soin_visage_preprocessed.csv"),
                       na.strings = "non defini")

data_products <- fread(file = file.path(chemin,"BDD/raw/df_products_soin_visage_preprocessed.csv"),
                       header = T, sep = ",",fill = TRUE, encoding = "UTF-8", 
                       na.strings = "non defini")

####################  Données Produits : Recoding & ID
names(data_products)
# Clean des variables textuelles 
data_products$full_name <- try_to_lower(improve_text(data_products$full_name))
data_products$name <- try_to_lower(improve_text(data_products$name))
data_products$brand <- try_to_lower(improve_text(data_products$brand))
data_products$gamme <- try_to_lower(improve_text(data_products$gamme))
data_products$category <- try_to_lower(improve_text(data_products$category))
data_products$presentation <- try_to_lower(improve_text(data_products$presentation))
data_products$texture <- try_to_lower(improve_text(data_products$texture))
data_products$skin_type <- try_to_lower(improve_text(data_products$skin_type))
data_products$label <- try_to_lower(improve_text(data_products$label))
data_products$descriptif <- try_to_lower(improve_text(data_products$descriptif))
data_products$advice <- try_to_lower(improve_text(data_products$advice))
data_products$composant <- try_to_lower(improve_text(data_products$composant))

# Produits unique 
length(unique(data_products$full_name)) # 1480
length(which(duplicated(data_products$full_name))) # 37 dupliqués mais marques différentes
data_products[which(duplicated(data_products$full_name)), c("name","brand")]

# 2 - Création id_produits
data_products$id_product <- paste0(data_products$full_name, "_", data_products$brand)

# Produits unique 
length(unique(data_products$id_product))# 1515
length(which(duplicated(data_products$id_product))) # 2 produits identiques
data_products[which(duplicated(data_products$id_product)), c("full_name","brand","id_product")]

# Cas problèmatiques : 
data_products[c(285,364),]
data_products[c(53,881),]

####################  Données Commentaires : Recoding & ID
data_comments$product <- try_to_lower(improve_text(data_comments$product))
data_comments$brand <- try_to_lower(improve_text(data_comments$brand))
data_comments$pseudo_writer <- try_to_lower(improve_text(data_comments$pseudo_writer))
data_comments$point_positif <- try_to_lower(improve_text(data_comments$point_positif))
data_comments$point_negatif <- try_to_lower(improve_text(data_comments$point_negatif))
data_comments$comment <- try_to_lower(improve_text(data_comments$comment))
# définition du même id
data_comments$id_product <- paste0(data_comments$product, "_", data_comments$brand)

# produits unique
length(unique(data_comments$id_product)) # 11.948 produits différents commentés

# Commentaires uniquement sur les produits de soin visages
data_comments_soin_visage <- subset(data_comments, id_product %in% data_products$id_product)

# Produits sans commentaires ?
produits_sans_commentaires <- subset(data_products, !(id_product %in% data_comments$id_product))
produits_sans_commentaires$id_product
produits_sans_commentaires$link_product
# exemple
which(data_comments$brand == "cha ling")
data_comments[which(data_comments$brand == "cha ling"),"product"]
data_comments[which(data_comments$brand == "elyctia"),"product"]

####################  Données d'analyses finales: Uniquement tous produits avec des commentaires
# Produits
data_products <- subset(data_products, id_product %in% data_comments$id_product) # 1240 produits 
ordered_data <- data_products[order(data_products$id_product)]
data_products <- ordered_data[!duplicated(ordered_data$id_product),] # 1238 produits ( retrait des doublons)
# Commentaires
data_comments <- subset(data_comments, id_product %in% data_products$id_product) # 56.534 commentaires 

# Données manquantes
m <- aggr(data_products) # protection -> advice : necessité analyse descriptif !!!
m_bis <- aggr(data_comments) # note douceur et note facility

# Merge : chaque ligne correspond à un commentaire avec le produit
data_soin_visage <- merge(x = data_products, data_comments,
                          by = c("id_product","brand"), all.y = T)

data_soin_visage <- data_soin_visage[,-c("V1.x","V1.y","full_name","name","price_indication",
                                         "price_one_litre","date_out_on_market",
                                         "price_one_kg","product","note_douceur",
                                         "note_facility")]
# Sauvegarde fichier final
# write.csv(data_soin_visage, 
#           file = file.path(chemin,"BDD/clean/data_soin_visage.csv"),
#           row.names = F, na = "non defini", 
#           fileEncoding = "UTF-8")

saveRDS(data_products, file = file.path(chemin,"BDD/clean/data_products.rds"))
saveRDS(data_comments, file = file.path(chemin,"BDD/clean/data_comments.rds"))
saveRDS(data_soin_visage, file = file.path(chemin,"BDD/clean/data_soin_visage.rds"))

