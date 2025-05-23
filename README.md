 		# readme de : bulletinsElevesGitH
 
	 # Programme COBOL : Étudiants et Cours

Ce programme lit un fichier avec des informations sur des **étudiants** et leurs **cours**, 
les stocke dans la mémoire, puis affiche les résultats à l’écran.

---

    ## Objectif du  programme ?

- Lire des données depuis un fichier texte (`input.dat`)
- Distinguer les étudiants et les cours
- Associer chaque étudiant à ses cours
- Afficher les informations de façon organisée

---

    ## Fichiers utilisés

### Fichier d'entrée : `input.dat` qui contient :

- Des lignes qui commencent par `01` pour les **étudiants**
  
- Des lignes qui commencent par `02` pour les **cours**
  

### Fichier de sortie : `output.dat`, prévu mais pas encore utilisé dans cette version.

---

    ## Fonctionnement ?

1. Le programme lit chaque ligne du fichier.
2. Il regarde les **2 premiers caractères** :
   - Si c’est `01`, il lit un **étudiant**
   - Si c’est `02`, il lit un **cours** et l’ajoute à l’étudiant précédent
3. Les informations sont stockées en mémoire dans des tableaux.
4. À la fin, le programme **affiche les étudiants et leurs cours** à l’écran.

---


