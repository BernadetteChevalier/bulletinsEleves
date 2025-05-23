      * Brief jour20-1 LECTURE DES DONNEES.
      * 2 types d'enregistrement
      * 1-infos sur etudiants
      * 2- infos sur cours

      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. report.
       AUTHOR. AlexEnCode.

      *****************************************************************
      *
      *****************************************************************

       ENVIRONMENT DIVISION.    

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      * indique que le separateur decimal utilisé est la virgule.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Déclaration du fichier d'entrée (lecture des données)
      * - Assignation au fichier 'input.dat'
      * - Accès séquentiel ligne par ligne
      * - Statut pour gérer les erreurs (variable spéciale qui contient
      *  un  code indiquant si une opération sur un fichier a réussi 
      * ou pas (lect, ecri, ouv)
            
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS. 

      * Déclaration du fichier de sortie (écriture des résultats)
           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.    

      ****************************************************************
      *
      ****************************************************************     

       DATA DIVISION.
       FILE SECTION.
      
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.
      * Le mode 'V' signifie que les longueurs des enregistrements 
      * peuvent varier de 2 à 1000 caractères.

      * Déclaration de plusieurs formats de lecture 
       01  REC-F-INPUT-2       PIC 9(02).
       01  REC-F-INPUT-10      PIC X(10).
       01  REC-F-INPUT-100     PIC X(100).
       01  REC-F-INPUT-1000    PIC X(1000).
      
      * Enregistrement de type "étudiant"
      * Contient une clé, nom, prénom, et âge
       01  REC-STUDENT.
           03 R-S-KEY          PIC 9(02).       
           03 R-LASTNAME       PIC X(07).       
           03 R-FIRSTNAME      PIC X(06).       
           03 R-AGE            PIC 9(02).       

      * Enregistrement de type "cours"
      * Contient une clé, un libellé, un coefficient, et une note
       01  REC-COURSE.
           03 R-C-KEY          PIC 9(02).       
           03 R-LABEL          PIC X(21).       
           03 R-COEF           PIC 9,9.       
           03 R-GRADE          PIC 99,99.       

      * Fichier de sortie : enregistrements longueur fixe de 250 car.
       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT            PIC X(250).

      *****************************************************************
      *
      *****************************************************************

       WORKING-STORAGE SECTION.

      * Codes de statut pour les fichiers
       01  F-INPUT-STATUS          PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS         PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

      * Décla des tableaux pour stocker les étudiants et leurs cours
      * longueurs des lignes etudiants et cours
       01  WS-DATA-STUDENT.
           05 WS-STUDENT-LGHT         PIC 9(03).
              *> Nombre d'étudiants chargés
           05 WS-COURSE-LGHT          PIC 9(03).
              *> Nombre de cours pour le dernier étudiant lu

     
      * Tableau dynamique des étudiants (max 999)
           05 WS-STUDENT OCCURS 1 TO 999 TIMES 
                                       DEPENDING ON WS-STUDENT-LGHT.
             10 WS-S-LASTNAME       PIC X(07).      
             10 WS-S-FIRSTNAME      PIC X(07).
             10 WS-S-AGE            PIC 9(02). 
             10 WS-S-AVERAGE        PIC 99V99. *>Moy Pond
      * Chaque étudiant peut avoir jusqu’à 10 cours associés
             10 WS-COURSE OCCURS 10.
                15 WS-C-LABEL          PIC X(21).       
                15 WS-C-COEF           PIC 9V9.       
                15 WS-C-GRADE          PIC 99V99. 
       
      * Index utilisé pour parcourir les étudiants
       77  WS-IDX-S                   PIC 9(02).
      * Index utilisé pour parcourir les cours
       77  WS-IDX-C                   PIC 9(02).
      
      * somme des coef.
       01 WS-SUM-COEF                PIC 9(05)V9    VALUE 0.
      * somme ponderee
       01 WS-SUM-PONDEREE            PIC 9(05)V99    VALUE 0.

      *****************************************************************
      *
      *****************************************************************

       PROCEDURE DIVISION.

      * Lecture des données depuis le fichier d’entrée
       PERFORM 0100-LIRE-FICHIER-DEB
           THRU 0100-LIRE-FICHIER-FIN.

      * Affichage en console des données lues (étudiants + cours)
       PERFORM 0200-AFFICHAGE-FICHIER-DEB
           THRU 0200-AFFICHAGE-FICHIER-FIN.

      * Calcul de la moyenne
       PERFORM 0300-CALCUL-MOY-ETUDIANT-DEB
           THRU 0300-CALCUL-MOY-ETUDIANT-FIN.

       STOP RUN.


      *****************************************************************
      *
      *****************************************************************
       0100-LIRE-FICHIER-DEB.
      *****************************************************************
      * - Ouvre le fichier F-INPUT
      * - Boucle sur chaque enregistrement
      * - Remplit les tableaux avec les données des étudiants et cours
      *****************************************************************
           MOVE 0 TO WS-IDX-S.
           MOVE 0 TO WS-STUDENT-LGHT.
           OPEN INPUT F-INPUT.

      * Boucle jusqu’à détection de la fin du fichier
       PERFORM UNTIL F-INPUT-STATUS-EOF
         READ F-INPUT
           NOT AT END

      * Détermination du type d’enregistrement lu :
      * si REC-F-INPUT-2 = 1 : étudiant, = 2 : cours
             EVALUATE REC-F-INPUT-2
               WHEN 1 
                 ADD 1 TO WS-STUDENT-LGHT 
      * Stockage infos étudiant dans la prochaine ligne du tableau
                 MOVE R-LASTNAME 
                     TO WS-S-LASTNAME(WS-STUDENT-LGHT)
                 MOVE R-FIRSTNAME 
                     TO WS-S-FIRSTNAME(WS-STUDENT-LGHT)
                 MOVE R-AGE 
                     TO WS-S-AGE(WS-STUDENT-LGHT)
      * Initialisation du nombre de cours pour ce nouvel étudiant
                 MOVE 0 TO WS-COURSE-LGHT

               WHEN 2
                 ADD 1 TO WS-COURSE-LGHT
      * Stockage cours ds position correspondante de l’étudiant courant
                 MOVE R-LABEL 
                     TO WS-C-LABEL(WS-STUDENT-LGHT WS-COURSE-LGHT)
                 MOVE R-COEF 
                     TO WS-C-COEF(WS-STUDENT-LGHT WS-COURSE-LGHT)
                 MOVE R-GRADE 
                     TO WS-C-GRADE(WS-STUDENT-LGHT WS-COURSE-LGHT)

             END-EVALUATE
         END-READ
       END-PERFORM.
       CLOSE F-INPUT.
       0100-LIRE-FICHIER-FIN.


      *****************************************************************
      *
      *****************************************************************
       0200-AFFICHAGE-FICHIER-DEB.
      *****************************************************************
      * Affichage des données stockées : pour chaque étudiant,
      * on affiche ses informations personnelles et ses cours associés.
      *****************************************************************
       PERFORM VARYING WS-IDX-S FROM 1 BY 1
               UNTIL WS-IDX-S > WS-STUDENT-LGHT

      * Affichage des informations de l’étudiant courant
                DISPLAY "Student " WS-IDX-S ":"
                DISPLAY "Last Name: " WS-S-LASTNAME(WS-IDX-S)
                DISPLAY "First Name: " WS-S-FIRSTNAME(WS-IDX-S)
                DISPLAY "Age: " WS-S-AGE(WS-IDX-S)

      * Parcours des cours pour cet étudiant
       PERFORM VARYING WS-IDX-C FROM 1 BY 1
               UNTIL WS-IDX-C > WS-COURSE-LGHT

                DISPLAY "Course " WS-IDX-C " for Student " WS-IDX-S ":"
                DISPLAY "Course Label: " WS-C-LABEL(WS-IDX-S WS-IDX-C)
                DISPLAY "Coefficient: " WS-C-COEF(WS-IDX-S WS-IDX-C)
                DISPLAY "Grade: " WS-C-GRADE(WS-IDX-S WS-IDX-C)

       END-PERFORM
       END-PERFORM.

       0200-AFFICHAGE-FICHIER-FIN.


      *****************************************************************
      *
      *****************************************************************
       0300-CALCUL-MOY-ETUDIANT-DEB.
      *****************************************************************
      * Pr chaque etudiant, calcul de la moyenne ponderee(coef)
      * Procedure parcourt tous les cours associés à chaque étudiant,
      * calcule la somme des (note × coefficient), puis divise
      * par la somme des coefficients.
      * Affichage eleve et moyenne.
      *****************************************************************

       PERFORM VARYING WS-IDX-S FROM 1 BY 1
               UNTIL WS-IDX-S > WS-STUDENT-LGHT

      * Initialise les deux sommes à zéro pour un nouvel étudiant
           MOVE 0 TO WS-SUM-COEF
           MOVE 0 TO WS-SUM-PONDEREE

      * Boucle sur les cours de l'étudiant concerné 
           PERFORM VARYING WS-IDX-C FROM 1 BY 1
                   UNTIL WS-IDX-C > WS-COURSE-LGHT

      * Calcul somme pondérée : grade * coef et ajout ce produit 
      * à la somme pondérée
               COMPUTE WS-SUM-PONDEREE = WS-SUM-PONDEREE +
                   (WS-C-GRADE(WS-IDX-S WS-IDX-C) * 
                    WS-C-COEF(WS-IDX-S WS-IDX-C))
                    DISPLAY "SUM PONDEREE" WS-SUM-PONDEREE

      * Calcul la somme des coefficients pour l’étudiant
               COMPUTE WS-SUM-COEF = WS-SUM-COEF +
                   WS-C-COEF(WS-IDX-S WS-IDX-C)
                   DISPLAY "SUM COEF" WS-SUM-COEF

           END-PERFORM

      * Si l’étudiant a au moins un coefficient (évite division par 0)
           IF WS-SUM-COEF NOT = 0

      * Calcul de la moyenne pondérée : somme pondérée / somme des coefs
      * Utilise ROUNDED pour arrondir le résultat final
               COMPUTE WS-S-AVERAGE(WS-IDX-S) ROUNDED =
                   WS-SUM-PONDEREE / WS-SUM-COEF 

      * Si aucun cours → on met la moyenne à 0 pour éviter erreur
           ELSE
               MOVE 0 TO WS-S-AVERAGE(WS-IDX-S)
           END-IF

           DISPLAY "ELEVE " WS-S-LASTNAME(WS-IDX-S)
           DISPLAY "MOYENNE " WS-S-AVERAGE(WS-IDX-S)
       END-PERFORM.

       0300-CALCUL-MOY-ETUDIANT-FIN.



