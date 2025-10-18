# Application R Shiny – Projet RH Pay Insights

## Objectif du projet
Cette application Shiny a été développée dans le cadre du **Master Sciences des Données / Modélisation Statistique**.  
Elle vise à **explorer et visualiser les données RH** d’une entreprise fictive, en se concentrant sur les **salaires** et **types de contrats**.

L’application permet :
- d’analyser les indicateurs principaux (effectifs, types de contrats, salaires moyens et médians) ;
- de visualiser les répartitions à l’aide de graphiques interactifs ;
- d’exporter facilement les données filtrées au format CSV.

> Le projet évoluera prochainement avec l’ajout de la page **“Explorer”**, dédiée à l’analyse interactive détaillée des données.

---

## Structure du projet

### 1. Fichiers principaux
| Fichier | Rôle |
|----------|------|
| **global.R** | Chargement des bibliothèques, importation et préparation des données. |
| **ui.R** | Interface utilisateur : onglets *Accueil*, *Exporter les données*, et *À propos*. |
| **server.R** | Logique serveur : calculs, graphiques interactifs, filtres et exports CSV. |
| **www/** | Contient les images (logos, avatars, icônes). |
| **data/** | Contient les jeux de données RH fournis (`RH_Salaries.xlsx` et `RH_Contrats.xlsx`). |

---

## Données utilisées

| Fichier | Description |
|----------|--------------|
| **RH_Salaries.xlsx** | Informations principales des salariés (sexe, âge, salaire, situation familiale, etc.). |
| **RH_Contrats.xlsx** | Détails sur les contrats (type de contrat, durée hebdomadaire, identifiant salarié). |

Un **nettoyage des données** est effectué dans `global.R` :  
- renommage des colonnes pour cohérence,  
- exclusion des salariés **sans contrat** via une jointure interne (`inner_join`).

---

## 🖥️ Fonctionnalités actuelles

### Onglet **Accueil**
- Indicateurs : nombre de salariés, nombre de contrats, part de CDI, salaire médian/moyen (commutable).  
- Graphiques interactifs :
  - Histogramme de la distribution des salaires,
  - Diagramme circulaire de la répartition des contrats.
- Design sobre et réactif via `bslib` et `plotly`.

### Onglet **Exporter les données**
- Filtres sur :
  - âge, sexe, état civil, nombre d’enfants, contrat, salaire minimum, durée hebdomadaire, etc.  
- Affichage d’un aperçu du jeu de données filtré.  
- Téléchargement au format `.csv` :
  - `RH_Salaries.csv` (table des salariés),
  - `RH_Contrats.csv` (table des contrats),
  - `Donnees_filtrees.csv` (résultat des filtres actifs).

### ℹOnglet **À propos**
- Présentation du projet et des données.  
- Liens vers le dépôt GitHub et les profils des auteurs :
  - **TOURE Abdoulaye Wade** – Master Sciences des Données  
  - **DIOP Daouda** – Master en Modélisation Statistique

---

## Technologies utilisées
- **R**  
- **Shiny** pour l’application web  
- **tidyverse** pour le traitement des données (`dplyr`, `readxl`, etc.)  
- **plotly** pour les graphiques interactifs  
- **bslib** pour la mise en forme et le thème visuel  
- **shinyWidgets** pour les éléments d’interface modernes  

---

## 🚀 Lancer l’application

### Cloner le dépôt
```bash
git clone https://github.com/abd-29/RH_ShinyProject.git
cd RH_ShinyProject
