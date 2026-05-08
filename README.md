# Student Performance - Modélisation

Projet d’analyse statistique du jeu de données `student-mat.csv`, issu du
dataset Student Performance (UC Irvine Machine Learning Repository). L’objectif est d’expliquer et de prédire la note
finale `G3` en mathématiques, puis d’étudier la réussite scolaire sous forme
binaire avec la variable `passed`.

La pipeline principal est écrit en R. Une analyse exploratoire complémentaire
est disponible en MATLAB.

## Objectifs

- décrire les données et vérifier leur structure ;
- modéliser la note finale `G3` par régression linéaire ;
- comparer plusieurs spécifications avec et sans les notes intermédiaires
  `G1` et `G2` ;
- sélectionner les modèles par critère AIC ;
- diagnostiquer les résidus du modèle linéaire retenu ;
- modéliser la réussite ou l’échec par régression logistique ;
- produire les tableaux et figures utilisés dans le rapport.

## Données

Le fichier principal est `student-mat.csv`.

- Nombre d’observations : 395 élèves.
- Nombre de variables initiales : 33.
- Séparateur CSV : point-virgule (`;`).
- Dictionnaire des variables : `student.txt`.

Variables cibles :

- `G3` : note finale en mathématiques, comprise entre 0 et 20.
- `passed` : variable créée par le pipeline R, égale à 1 si `G3 >= 10`,
  et 0 sinon.

Les prédicteurs couvrent les notes intermédiaires (`G1`, `G2`), les variables
démographiques, familiales, scolaires et comportementales.

## Structure du dépôt

```text
.
|-- Modelisation.R
|-- Figures_supplementaires.R
|-- SaveFigure.r
|-- student-mat.csv
|-- student.txt
|-- outputs_modelisation/
|   |-- tables/
|   `-- figures/
`-- MATLAB/
    |-- analyse_exploratoire.m
    `-- SaveFigure.m
```

Description des fichiers principaux :

- `Modelisation.R` : pipeline principal de modélisation et d’export.
- `Figures_supplementaires.R` : figures additionnelles construites à partir
  des sorties du pipeline principal.
- `SaveFigure.r` : fonction R de sauvegarde standardisée des figures.
- `MATLAB/analyse_exploratoire.m` : analyse exploratoire descriptive.
- `MATLAB/SaveFigure.m` : fonction MATLAB de sauvegarde des figures.

## Prérequis

### R

Packages nécessaires :

```r
install.packages(c("MASS", "ggplot2"))
```

La pipeline a été conçu pour être lancé depuis la racine du projet.

### MATLAB

MATLAB est uniquement requis pour l’analyse exploratoire située dans le dossier
`MATLAB/`. La pipeline final de modélisation ne dépend pas de MATLAB.

## Exécution

### 1. Pipeline principal R

Depuis la racine du projet :

```powershell
source("Modelisation.R")
```

Le script cherche automatiquement le fichier de données dans cet ordre :

1. variable d’environnement `STUDENT_DATA_PATH` ;
2. variable d’environnement `INPUT_PATH` ;
3. `student-mat.csv` ;
4. `studentmat.csv`.

### 2. Figures supplémentaires

Après avoir lancé `Modelisation.R` :

```powershell
source("Figures_supplementaires.R")
```

Ce script réutilise les termes retenus et les journaux AIC exportés par le
pipeline principal.

### 3. Analyse exploratoire MATLAB

Depuis le dossier `MATLAB/` :

```matlab
analyse_exploratoire
```

Les sorties MATLAB sont créées dans `MATLAB/AnalyseExploratoire/`.

## Sorties produites

Le dossier `outputs_modelisation/` contient la version finale des sorties R.

### Tableaux

Les tableaux CSV sont dans `outputs_modelisation/tables/`. La version actuelle
contient 28 fichiers, notamment :

- coefficients du modèle complet et des modèles sélectionnés ;
- test F global du modèle linéaire complet ;
- matrice de corrélation des variables quantitatives ;
- comparaisons des procédures AIC ;
- journaux des sélections stepAIC ;
- diagnostics des résidus ;
- coefficients du modèle structurel sans `G1` et `G2` ;
- odds ratios des modèles logistiques ;
- matrice de confusion du modèle logistique sélectionné ;
- synthèse finale des modèles retenus.

Fichiers de synthèse principaux :

- `3.6.3_comparaison_modeles_lineaires.csv` ;
- `3.9_comparaison_modeles_logistiques.csv` ;
- `3.9_matrice_confusion_modele_selectionne.csv` ;
- `3.10_synthese_modeles_retenus.csv` ;
- `3.10_termes_retenus_par_modele.csv`.

### Figures

Les figures PDF sont dans `outputs_modelisation/figures/`. La version actuelle
contient 9 figures :

- évolution de l’AIC durant la sélection ;
- résidus vs valeurs ajustées ;
- QQ-plot des résidus ;
- histogrammes des résidus ;
- valeurs observées vs valeurs ajustées ;
- forest plot du modèle structurel ;
- forest plot des odds ratios ;
- distribution des probabilités prédites.

## Modèles estimés

Le pipeline estime et compare :

- un modèle linéaire complet avec `G1` et `G2` ;
- un modèle linéaire sélectionné avec `G1` et `G2` ;
- un modèle linéaire sélectionné sans `G1` ;
- un modèle linéaire sélectionné sans `G2` ;
- un modèle structurel complet sans `G1` et `G2` ;
- un modèle structurel sélectionné sans `G1` et `G2` ;
- un modèle logistique complet pour `passed` ;
- un modèle logistique sélectionné pour `passed`.

La sélection de variables est effectuée par `MASS::stepAIC`, avec comparaison
des procédures backward, forward et stepwise.

## Résultats principaux

Synthèse des modèles retenus dans `3.10_synthese_modeles_retenus.csv` :

| Modèle | Paramètres | Métrique principale | AIC |
| --- | ---: | ---: | ---: |
| Linéaire sélectionné avec `G1`, `G2` | 10 | R² ajusté = 0.8344 | 1624.88 |
| Linéaire sélectionné sans `G1` | 8 | R² ajusté = 0.8305 | 1632.13 |
| Linéaire sélectionné sans `G2` | 14 | R² ajusté = 0.6800 | 1888.99 |
| Linéaire sélectionné sans `G1`, `G2` | 17 | R² ajusté = 0.2087 | 2249.52 |
| Logistique sélectionnée | 23 | Accuracy = 0.9747 | 105.05 |

Interprétation générale :

- les notes intermédiaires, surtout `G2`, portent une part importante du
  pouvoir prédictif sur `G3` ;
- le modèle structurel sans `G1` et `G2` reste informatif mais beaucoup moins
  performant ;
- le modèle logistique sélectionné classe correctement 385 élèves sur 395 avec
  un seuil de 0.5 ;
- les diagnostics montrent une attention particulière à porter aux observations
  avec `G3 = 0` et à la normalité des résidus.

## Reproductibilité

- La graine aléatoire R est fixée à `42`.
- Les références des facteurs sont fixées explicitement dans `Modelisation.R`.
- Les sorties sont régénérées automatiquement par les scripts.
- Les dossiers de sorties sont ignorés par Git via `.gitignore`.
- Les figures R sont exportées au format PDF avec une mise en forme homogène.

## Commandes finales recommandées

Pour régénérer l’ensemble des sorties R finales :

```powershell
Rscript Modelisation.R
Rscript Figures_supplementaires.R
```

Les fichiers finaux à vérifier en priorité sont :

- `outputs_modelisation/tables/3.10_synthese_modeles_retenus.csv` ;
- `outputs_modelisation/tables/3.10_termes_retenus_par_modele.csv` ;
- `outputs_modelisation/tables/3.9_matrice_confusion_modele_selectionne.csv` ;
- `outputs_modelisation/figures/3.7_residus_vs_ajustes.pdf` ;
- `outputs_modelisation/figures/3.9_forest_plot_odds_ratios.pdf`.
