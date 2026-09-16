#installer bibliothèques nécessaires
library(sf)
library(tmap)
library(spData)

#importer les tables

performances <- read.csv("data/performances.csv", sep=",")
judged_aspects <- read.csv("data/judged-aspects.csv", sep=",")
judge_scores <- read.csv("data/judge-scores.csv", sep=",")
programs <- read.csv("data/programs.csv", sep=",")
judges_info <- read.csv("data/judges_info.csv", sep = ";")
correspondance <- read.csv("data/correspondance.csv", sep=",")

#fusionner les données dans une grosse table

df_full <- merge(performances, judged_aspects,
                 by = "performance_id")
df_full <- merge(df_full, judge_scores,
                 by = "aspect_id")
df_full <- merge(df_full, programs,
                 by = c("competition", "program"))
df_full2 <- merge(df_full, judges_info,
                  by = c("competition", "program", "judge"))

#additioner le score par juge, par patineur, par performance, par compétition

df_summary <- aggregate(
  score ~ competition + program + judge + judge_name +
    judge_nation + performance_id + name + nation,
  data = df_full2,
  FUN = sum,
  na.rm = TRUE
)

#ajouter la colonne pour le même pays

df_summary$same_country <-
  df_summary$judge_nation == df_summary$nation

#comparaison des moyennes

mean(df_summary$score[df_summary$same_country == FALSE])
mean(df_summary$score[df_summary$same_country == TRUE])

aggregate(score ~ same_country,
          data = df_summary,
          FUN = mean)

#et par discipline

aggregate(score ~ program + same_country,
          data = df_summary,
          FUN = mean)

#faire des graphiques 

boxplot(score ~ same_country,
        data = df_summary,
        main = "Scores selon le biais national",
        xlab = "Même pays juge / patineur",
        ylab = "Score total",
        names = c("Différents pays", "Même pays"))

par(mar = c(12, 5, 4, 2))

boxplot(
  score ~ program,
  data = df_summary,
  main = "Distribution des scores par programme",
  ylab = "Score total",
  las = 2,
  cex.axis = 0.8
)

hist(df_summary$score,
     main = "Distribution de l'ensemble des scores",
     xlab = "Score total",
     ylab = "Nombre d'observations")


#tests

t.test(score ~ same_country, data = df_summary)

m1 <- lm(score ~ judge_nation + nation + same_country,
         data = df_summary)
summary(m1)

#MAYBE, différences selon le pays du juge

moyennes <- aggregate(
  score ~ judge_nation + same_country,
  data = df_summary,
  FUN = mean
)

meme_pays <- subset(moyennes, same_country == TRUE)
autre_pays <- subset(moyennes, same_country == FALSE)

resultat <- merge(
  meme_pays,
  autre_pays,
  by = "judge_nation",
  suffixes = c("_meme_pays", "_autre_pays")
)

resultat$difference <- resultat$score_meme_pays - resultat$score_autre_pays

resultat <- resultat[order(-resultat$difference), ]

resultat

barplot(resultat$difference,
        names.arg = resultat$judge_nation,
        main = "Écart de notation selon la nationalité du juge",
        xlab = "Nationalité du juge",
        ylab = "Score moyen (même pays - pays différent)",
        las = 2)

#faire la carte du monde

data(World)

participants <- unique(df_full[c("name", "nation")])

nb_participants <- aggregate(
  name ~ nation,
  data = participants,
  FUN = length
)

names(nb_participants) <- c("nation", "nb_participants")

nb_participants <- merge(
  nb_participants,
  correspondance,
  by = "nation"
)

World <- merge(
  World,
  nb_participants,
  by = "iso_a3",
  all.x = TRUE
)

World$nb_participants[is.na(World$nb_participants)] <- 0

tm_shape(World) +
  tm_polygons(
    "nb_participants",
    fill.scale = tm_scale_intervals(
      breaks = c(0, 1, 5, 10, 20, 30, 45, 59),
      values = "brewer.blues",
      as.count = TRUE
    ),
    fill.legend = tm_legend(
      title = "Nombre de participants"
    )
  ) +
  tm_title(
    "Participants en patinage artistique 2016-2017",
    position = c("center", "top"),
    size = 1.2
  ) +
  tm_layout(
    inner.margins = c(0.05, 0.05, 0.25, 0.05)
  )

#faire le tableau qui va avec la carte

noms_pays <- as.data.frame(World)[c("iso_a3", "name")]

tableau_participants <- merge(
  nb_participants,
  noms_pays,
  by = "iso_a3"
)

tableau_participants <- tableau_participants[
  c("name", "nb_participants")
]

names(tableau_participants) <- c(
  "Pays",
  "Nombre de participants"
)

tableau_participants <- tableau_participants[
  order(-tableau_participants$`Nombre de participants`),
]

rownames(tableau_participants) <- NULL

tableau_participants

#biais par pays

biais_par_pays <- aggregate(
  score ~ judge_nation + same_country,
  data = df_summary,
  FUN = mean
)

#écart de notation selon la discipline

moyennes_programme <- aggregate(
  score ~ program + same_country,
  data = df_summary,
  FUN = mean
)

moyennes_programme

meme <- subset(moyennes_programme, same_country == TRUE)
autre <- subset(moyennes_programme, same_country == FALSE)

comparaison_programme <- merge(
  meme,
  autre,
  by = "program",
  suffixes = c("_meme", "_autre")
)

comparaison_programme$ecart <-
  comparaison_programme$score_meme -
  comparaison_programme$score_autre

comparaison_programme <- comparaison_programme[
  order(-comparaison_programme$ecart),
]

comparaison_programme

par(mar = c(12, 5, 4, 2))

barplot(
  comparaison_programme$ecart,
  names.arg = comparaison_programme$program,
  main = "Écart de notation selon le programme",
  ylab = "Score moyen (même pays - pays différent)",
  las = 2,
  cex.names = 0.8
)

mtext(
  "Programme",
  side = 1,
  line = 10
)
