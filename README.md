# Analyse de la Filière Maïs et Sécurité Alimentaire au Burkina Faso

Ce projet analyse la filière maïs au Burkina Faso et son lien avec la sécurité alimentaire des ménages, à partir des données de l'enquête harmonisée sur les conditions de vie des ménages (EHCVM).

## 📋 Contenu du projet

- **`main.R`** — Script principal du projet
- **`scripts/`** — Scripts R utilisés pour le nettoyage, le traitement et l'analyse des données
- **`dashboard/`** — Tableau de bord interactif de visualisation des résultats
- **`filiereBFA/`** — Package R développé pour ce projet, regroupant les fonctions spécifiques à l'analyse de la filière maïs
- **`fichier_important_comprehension/`** — Documents de référence pour la compréhension des données et du contexte
- **`sorties/`** — Résultats, graphiques et tableaux générés par les analyses
- **`Rapport_Securite_alimentaire_Burkina_Faso.Rmd`** / **`.pdf`** — Rapport complet sur la sécurité alimentaire
- **`presentation_filiere.Rmd`** / **`.html`** — Présentation de synthèse de l'analyse de la filière
- **`Analyse de la Filière Maïs et Sécurité Alimentaire.pdf`** — Document de synthèse de l'analyse
- **`custom_filiere.css`** — Feuille de style personnalisée pour les documents R Markdown
- **`projet_R.Rproj`** — Fichier projet RStudio

## 📊 Données

Les données brutes issues de l'enquête EHCVM (Burkina Faso, 2021) **ne sont pas incluses dans ce dépôt** en raison de leur taille (plusieurs fichiers `.dta` dépassant 100 Mo). 

Pour reproduire les analyses, placez les fichiers de données suivants dans le dossier `donnee/base_burkina/` à la racine du projet :

- `ehcvm_prix_bfa2021.dta`
- `s07b_me_bfa2021.dta`
- `s15_me_bfa2021.dta`
- (et autres fichiers `s*_me_bfa2021.dta` requis par les scripts)

> Ces données sont disponibles auprès de l'Institut National de la Statistique et de la Démographie (INSD) du Burkina Faso ou via les plateformes de diffusion des enquêtes EHCVM.

## 🚀 Utilisation

1. Cloner le dépôt :
   ```bash
   git clone https://github.com/jo-david14/Analyse_filiere_Mais_Burkina_final.git
   ```
2. Ouvrir `projet_R.Rproj` dans RStudio
3. Placer les fichiers de données dans `donnee/base_burkina/` (voir section Données)
4. Exécuter `main.R` ou les scripts dans `scripts/` pour lancer les analyses

## 🛠️ Prérequis

- R (version récente recommandée)
- RStudio
- Packages R utilisés dans le projet (à installer selon les scripts, ex. `haven`, `tidyverse`, `dplyr`, etc.)

## 📦 Installation du package `filiereBFA`

Le dossier `filiereBFA/` est un package R développé spécifiquement pour ce projet. Pour l'installer localement :

```r
# Installer devtools si ce n'est pas déjà fait
install.packages("devtools")

# Installer le package depuis le dossier local
devtools::install("filiereBFA")
```

Ou, après avoir cloné le dépôt, directement en ligne de commande :

```r
devtools::install_github("jo-david14/Analyse_filiere_Mais_Burkina_final", subdir = "filiereBFA")
```

Une fois installé, chargez le package dans vos scripts avec :

```r
library(filiereBFA)
```

## 📄 Rapports

Les résultats de l'analyse sont disponibles sous forme de rapport et de présentation :
- [Rapport de sécurité alimentaire](Rapport_Securite_alimentaire_Burkina_Faso.pdf)
- [Présentation de la filière](presentation_filiere.html)
- [Analyse complète](<Analyse de la Filière Maïs et Sécurité Alimentaire.pdf>)

## Auteur

Projet réalisé par jo-david14.
