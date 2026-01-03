# 🎲 Rummikub - Implémentation en OCaml

![OCaml](https://img.shields.io/badge/OCaml-5.0%2B-orange?logo=ocaml)
![License](https://img.shields.io/badge/License-MIT-green)
![Status](https://img.shields.io/badge/Status-Completed-blue)

Une implémentation complète et fonctionnelle du célèbre jeu de société **Rummikub** en langage OCaml. Ce projet met en œuvre des structures de données avancées (multi-ensembles), une gestion rigoureuse des règles officielles et une interface en ligne de commande (CLI).

## 📖 Table des matières
- [Fonctionnalités](#-fonctionnalités)
- [Règles Implémentées](#-règles-implémentées)
- [Architecture Technique](#-architecture-technique)
- [Installation et Exécution](#-installation-et-exécution)
- [Tests Unitaires](#-tests-unitaires)
- [Structure du Code](#-structure-du-code)
- [Auteur](#-auteur)

## ✨ Fonctionnalités

* ✅ **Jeu à 2 joueurs** : Gestion des tours, des mains et de la pioche.
* ✅ **Moteur de jeu robuste** : Validation automatique des combinaisons (Suites et Groupes).
* ✅ **Gestion des Jokers** : Les jokers (J1, J2) sont gérés et peuvent remplacer n'importe quelle tuile.
* ✅ **Règle du premier coup** : Vérification stricte des 30 points minimum pour la première pose.
* ✅ **Interface CLI** : Affichage textuel de l'état du jeu, de la table et des mains.
* ✅ **Multi-ensembles (Multisets)** : Implémentation "from scratch" pour la gestion des tuiles.

  ![rummicub.png](rummicub.png)

## 🎮 Règles Implémentées

Le programme respecte les règles standards du Rummikub :

1.  **Matériel** : 106 tuiles (2 sets de 1 à 13 en 4 couleurs + 2 Jokers).
2.  **Combinaisons valides** :
    * **Suite** : Au moins 3 tuiles de même couleur et valeurs consécutives (ex: `Bleu 1, Bleu 2, Bleu 3`).
    * **Groupe** : 3 ou 4 tuiles de même valeur mais de couleurs différentes (ex: `Rouge 5, Bleu 5, Vert 5`).
3.  **Déroulement** :
    * Le joueur doit piocher s'il ne peut pas jouer.
    * Pour "sortir" (premier coup), le total des points posés doit être ≥ 30.
    * Le gagnant est celui qui vide sa main le premier.

## 🏗 Architecture Technique

Le projet repose sur une modélisation précise des données en OCaml.

### Types principaux

```ocaml
type couleur = Bleu | Rouge | Vert | Jaune
type valeur = V1 | ... | V13
type typeJoker = J1 | J2

type tuile = 
  | T of valeur * couleur 
  | Joker of typeJoker

(* Utilisation de listes d'association pour les multi-ensembles *)
type 'a melt = 'a * int
type 'a mset = 'a melt list 

type combinaison = 
  | Suite of tuile list 
  | Groupe of tuile list
```

## 👤 Auteur

**Votre Nom**
* GitHub : ![@Logibuilder](https://github.com/Logibuilder)
* Portfolio : [https://logibuilder.github.io/assane.kane/](https://logibuilder.github.io/assane.kane/)

*Projet réalisé dans un but éducatif pour démontrer la maîtrise d'OCaml et de la programmation fonctionnelle.*
