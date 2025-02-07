# Rapport sur le Projet d'Interpréteur pour le Langage LATSI <!-- omit in toc -->

## Sommaire <!-- omit in toc -->

1. [Introduction](#introduction)
2. [Commandes pour Compiler et Exécuter](#commandes-pour-compiler-et-exécuter)
3. [Étapes du Projet](#étapes-du-projet)
   1. [Étape 1 : Lexeur (`lexer.mll`)](#étape-1--lexeur-lexermll)
   2. [Étape 2 : Analyseur Syntaxique (`parser.mly`)](#étape-2--analyseur-syntaxique-parsermly)
   3. [Travail en binôme - Étapes 1 et 2](#travail-en-binôme---étapes-1-et-2)
   4. [Étape 3 : Interpréteur Sans `VAVERS` et `ENTREE` (`main.ml` et `ast.ml`)](#étape-3--interpréteur-sans-vavers-et-entree-mainml-et-astml)
   5. [Travail en binôme - Étape 3](#travail-en-binôme---étape-3)
   6. [Étape 4 : Extension avec `VAVERS` et `ENTREE` (`ast.ml`)](#étape-4--extension-avec-vavers-et-entree-astml)
   7. [Travail en binôme - Étape 4](#travail-en-binôme---étape-4)


## Introduction

Le projet vise à implémenter un interpréteur pour le langage LATSI (Langage Très Simple d'Instructions).

## Commandes pour Compiler et Exécuter

Pour compiler et exécuter le projet, utilisez les commandes suivantes :

- **Compilation** : `dune build`
- **Exécution** : `_build/default/main.exe exemples/test-general.latsi`

## Étapes du Projet

### Étape 1 : Lexeur (`lexer.mll`)

Le fichier `lexer.mll` utilise OCaml et ocamllex pour définir les règles lexicales du langage LATSI. Il reconnaît les différents tokens du langage LATSI.

### Étape 2 : Analyseur Syntaxique (`parser.mly`)

Le fichier `parser.mly` utilise Menhir pour définir la grammaire du langage LATSI et produit un arbre de syntaxe abstraite (AST).

### Travail en binôme - Étapes 1 et 2

Ces deux premières étapes ont été effectuées en groupe dans le but d'avoir une base de travail solide et de s'accorder sur les types et la manière de les parser. C'est Mohamed qui les a implémentées en se basant sur le travail produit en amont.

### Étape 3 : Interpréteur Sans `VAVERS` et `ENTREE` (`main.ml` et `ast.ml`)

L'interpréteur évalue les expressions et exécute les instructions définies dans l'AST. Le fichier `ast.ml` définit les types, et implémente les instructions. Le fichier `main.ml` contient la fonction d'évaluation du programme une fois le texte parsé.

### Travail en binôme - Étape 3

Cette étape a été réalisée en discussion constante. Comme le travail d'architecture avait été fait au préalable, l'implémentation des types et des fonctions qui permettent d'effectuer les instructions a été plutôt simple à quelques exceptions près.

### Étape 4 : Extension avec `VAVERS` et `ENTREE` (`ast.ml`)

L'implémentation des instructions `VAVERS` et `ENTREE` a été ajoutée à l'interpréteur.

### Travail en binôme - Étape 4

L'instruction `ENTREE` nous a posé problème mais nous avons finalement réussi à l'implémenter grâce à Mohamed qui a trouvé le problème dans `main.ml`.

- **Organisation** : Nous avons travaillé en utilisant un outils de gestion de versions (Git) pour partager le code et suivre les modifications. Les échanges réguliers ont permis de résoudre les problèmes ensemble.

