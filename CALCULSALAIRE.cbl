      ******************************************************************
      * Author: Nadir Pelletier
      * Date:   2019/04/10
      * Purpose:

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULSALAIRE.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FICHIER-ENTREE   ASSIGN TO "Donnee.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT RAPPORT-IMPRIME  ASSIGN TO "Sortie.doc"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FICHIER-TRI ASSIGN TO "Tri.txt".

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ENTREE.
       01  FICHE-PERSONNELLE           PIC X(59).

       FD  RAPPORT-IMPRIME.
       01  LIGNE-IMPRIME               PIC X(120).

       SD  FICHIER-TRI.
       01  TRI-FICHE-PERSONNELLE.
           05  TRI-CODE                  PIC X(6).
           05  TRI-REGION                PIC 99.
           05  TRI-SEXE                  PIC X.
           05  TRI-NOM                   PIC X(20).
           05  TRI-PRENOM                PIC X(15).
           05  TRI-DATE-ENGAGEMENT       PIC 9(8).
           05  TRI-TAUX                  PIC 99V99.
           05  TRI-NB-HEURES             PIC 9(3).

       WORKING-STORAGE SECTION.
       77  W-MSG-FICHIER-VIDE          PIC X(120)   VALUE SPACES.
       77  W-MSG-ECRAN                 PIC X(30)    VALUE SPACES.
      *********************************************************
      * Descriptif du fichier des employés de la
      * compagnie CRACK-INFO.
      *********************************************************
       01  W-FICHE-PERSONNELLE.
           05  W-CODE                  PIC X(6).
           05  W-REGION                PIC 99.
           05  W-SEXE                  PIC X.
           05  W-NOM                   PIC X(20).
           05  W-PRENOM                PIC X(15).
           05  W-DATE-ENGAGEMENT       PIC 9(8).
           05  W-TAUX                  PIC 99V99.
           05  W-NB-HEURES             PIC 9(3).

      *********************************************************
      * Zone des indicateurs
      *********************************************************
       01  W-INDICATEUR.
           05  W-IND-FIN-FICHIER    PIC 9 VALUE 0.
               88  W-FIN-FICHIER          VALUE 1.

      *********************************************************
      * Zone variables
      *********************************************************
       01  W-CHOIX-RAPPORT       PIC X       VALUE SPACE.
           88  QUITTER-R           VALUE "Q" "q".
           88  W-CHOIX-R-VALIDE    VALUE "1" "2" "Q" "q".

       01  W-MESSAGE-ERREUR PIC X(80).



      *********************************************************
      * Zone des compteurs et totalisateurs et variables utiles
      * aux différents calculs
      *********************************************************
       01  W-COMPTEURS.
           05  W-COMPTE-LIGNES         PIC 99.
           05  W-COMPTE-FICHES         PIC 999     VALUE 1.
           05  W-COMPTE-EMPLOYE        PIC 9999.
       01  W-CALCULS-DETAIL.
           05  W-SALAIRE-BRUT          PIC 9(6)V99.
      ********VARIABLES POUR LES HEURES SUPLÉMENTAIRES************
           05  W-SALAIRE-TAUX-DEMI     PIC 9(6)V99.
           05  W-NB-HEURES-TAUX-DEMIE  PIC 9(3).
           05  W-TAUX-DEMI             PIC 99V99.
      ************************************************************
           05  W-IMPOT-PROV            PIC 9(4)V99.
           05  W-IMPOT-FEDERAL         PIC 9(4)V99.
           05  W-SALAIRE-NET           PIC 9(5)V99.
       01  W-CALCULS-CUMUL.
           05  W-CUMUL-BRUT            PIC 9(9)V99.
           05  W-CUMUL-NET             PIC 9(9)V99.
       01  W-TAUX-IMPOT.
           05  W-TAUX-FEDERAL          PIC V99     VALUE ZERO.
           05  W-TAUX-PROV             PIC V99     VALUE ZERO.

      *********************************************************
      * Zone de définition des différentes lignes d'impression
      * du rapport
      *********************************************************
       01  W-ZONE-IMRESSION.
           05  W-ENTETE-GENERALE.
               10  FILLER              PIC X(35)   VALUE SPACES.
               10  FILLER              PIC X(53)   VALUE
               "** RAPPORT DES EMPLOYEES ET EMPLOYES DE CRACK-INFO **".
               10  FILLER              PIC X(22)   VALUE SPACES.
               10  FILLER              PIC X(5)    VALUE "PAGE:".
               10  EG-NO-PAGE          PIC Z(3).

           05  W-ENTETE-DETAIL-1.
               10  FILLER              PIC X(40)   VALUE
                   " CODE    NOM                 PRENOM     ".
               10  FILLER              PIC X(40)   VALUE
                   "REGION   DATE      TAUX     HEURES      ".
               10  FILLER              PIC X(40)   VALUE
                   " SALAIRE      IMPOT         SALAIRE     ".

           05  W-ENTETE-DETAIL-2.
               10  FILLER              PIC X(40)   VALUE
                   " EMPLOYE                                ".
               10  FILLER              PIC X(40)   VALUE
                   " ADM.  ENGAGEMENT HORAIRE TRAVAILLEES   ".
               10  FILLER              PIC X(40)   VALUE
                   "  BRUT    PROV.   FEDERAL     NET       ".

           05  W-LIGNE-DETAIL.
               10  FILLER              PIC X       VALUE SPACE.
               10  W-LD-CODE           PIC X(6).
               10  FILLER              PIC X       VALUE SPACE.
               10  W-LD-NOM            PIC X(20).
               10  FILLER              PIC X       VALUE SPACE.
               10  W-LD-PRENOM         PIC X(12).
               10  FILLER              PIC X       VALUE SPACE.
               10  W-LD-REGION         PIC 99.
               10  FILLER              PIC X(3)    VALUE SPACES.
               10  W-LD-DATE-ENGAGEMENT PIC 9999/99/99.
               10  FILLER              PIC XX      VALUE SPACES.
               10  W-LD-TAUX           PIC $$9.99.
               10  FILLER              PIC X(4)    VALUE SPACES.
               10  W-LD-NB-HEURES      PIC ZZ99.
               10  FILLER              PIC X(5)    VALUE SPACES.
               10  W-LD-SALAIRE-BRUT   PIC $$$999.99.
               10  FILLER              PIC XX      VALUE SPACES.
               10  W-LD-IMPOT-PROV     PIC $$99.99.
               10  FILLER              PIC XX      VALUE SPACES.
               10  W-LD-IMPOT-FEDERAL  PIC $$99.99.
               10  FILLER              PIC XX      VALUE SPACES.
               10  W-LD-SALAIRE-NET    PIC $$$999.99.
               10  FILLER              PIC X(4)    VALUE SPACES.

           05  W-LIGNE-SOMMAIRE-1.
               10  FILLER              PIC X(28)   VALUE SPACES.
               10  FILLER              PIC X(25)   VALUE
                   "NOMBRE TOTAL D'EMPLOYÉS: ".
               10  W-LS-NB-EMPLOYE     PIC Z(3).
               10  FILLER              PIC X(22)   VALUE
                  " SALAIRE BRUT MOYEN : ".
               10  W-LS-SALAIRE-BRUT   PIC $$$999.99.
               10  FILLER              PIC X(33)   VALUE SPACES.

           05  W-LIGNE-SOMMAIRE-2.
               10  FILLER              PIC X(57)   VALUE SPACES.
               10  FILLER              PIC X(21)   VALUE
                  "SALAIRE NET  MOYEN : ".
               10  W-LS-SALAIRE-NET    PIC $$$999.99.
               10  FILLER              PIC X(4)    VALUE SPACES.


      ****************************ENTETE********************************
       01  W-ENTETE.
          5  FILLER              PIC X(17)
                                 VALUE "LA CIE CRACK-INFO".
          5  FILLER              PIC X(10).
          5  FILLER              PIC X(29)
                                 VALUE "Gestion des employe(e)s".
          5  FILLER              PIC X(14).
          5  W-ENTETE-DATE       PIC 9999/99/99.


       SCREEN SECTION.

       01  FOND-ECRAN.
           05  BLANK SCREEN FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

           05  LINE  1 COLUMN 1 FOREGROUND-COLOR 7 BACKGROUND-COLOR 4
                                  PIC X(80) FROM W-ENTETE ERASE EOS.
           05  LINE  2 COLUMN 1  BLANK LINE
                                    BACKGROUND-COLOR 4.
           05  LINE  3 COLUMN 1  BLANK LINE
                                    BACKGROUND-COLOR 4.
           05  LINE 23 COLUMN 1 FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 4
                                    PIC X(80) FROM W-MESSAGE-ERREUR.
           05  LINE 24 COLUMN 1 BACKGROUND-COLOR 4
                                    VALUE SPACES SIZE 80.
           05  LINE 25 COLUMN 1 BACKGROUND-COLOR 4
                                    VALUE SPACES SIZE 80.


       01  MENU-PRINCIPAL.
           05  LINE 5   COLUMN 33 VALUE "MENU RAPPORTS" UNDERLINE.
           05  LINE 8   COLUMN 30 VALUE "1) Par Nom / Prenom / Date >= 1
      -"990".

           05  LINE 10  COLUMN 30 VALUE "2) Par Region / Heures Travaill
      -"ees > 40".

           05  LINE 12  COLUMN 30 VALUE "Q) Quitter".
           05  LINE 14  COLUMN 3  VALUE "Votre choix (1, 2, 3, 4, Q): ".
           05  LINE 14  COLUMN 32 PIC X TO W-CHOIX-RAPPORT.


       PROCEDURE DIVISION.

       00000-MAIN.
        PERFORM 00001-MENU-RAPPORT.
        PERFORM UNTIL QUITTER-R
           MOVE SPACE TO W-MESSAGE-ERREUR
           EVALUATE W-CHOIX-RAPPORT
           WHEN "1"
                SORT FICHIER-TRI ON ASCENDING KEY TRI-NOM
                                                  TRI-PRENOM
                                                  TRI-DATE-ENGAGEMENT
                      INPUT PROCEDURE   10000-TRAIT-NOM-PREN-DATE
                      OUTPUT PROCEDURE  30000-TRAIT-SORTIE

           WHEN "2"
                SORT FICHIER-TRI ON ASCENDING KEY TRI-REGION
                                               TRI-NB-HEURES
                       INPUT PROCEDURE   20000-REGION-HEURE
                       OUTPUT PROCEDURE  30000-TRAIT-SORTIE
           END-EVALUATE

           PERFORM 00001-MENU-RAPPORT
        END-PERFORM.

       EXIT PROGRAM.

       00001-MENU-RAPPORT.
           ACCEPT W-ENTETE-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8)  TO  W-ENTETE-DATE.

           MOVE SPACE TO W-CHOIX-RAPPORT.
            PERFORM UNTIL W-CHOIX-R-VALIDE
                DISPLAY FOND-ECRAN
                DISPLAY MENU-PRINCIPAL
                ACCEPT  MENU-PRINCIPAL

                MOVE FUNCTION UPPER-CASE(W-CHOIX-RAPPORT)
                TO W-CHOIX-RAPPORT

                IF NOT W-CHOIX-R-VALIDE
                    MOVE "ENTREZ 1, 2 OU Q" TO W-MESSAGE-ERREUR
                END-IF
            END-PERFORM.

      ******************************************************************
      *             INPUT PROCEDURE NOM/PRENOM/DATE >= 1990
      ******************************************************************
       10000-TRAIT-NOM-PREN-DATE  SECTION.
           OPEN  INPUT  FICHIER-ENTREE.
           MOVE 0 TO W-COMPTE-FICHES.
           MOVE 0 TO W-IND-FIN-FICHIER.
           PERFORM 11000-LECTURE-SEQ.
           PERFORM 12000-TRAIT-VALIDATION-DATE UNTIL W-FIN-FICHIER
           CLOSE FICHIER-ENTREE.
       10000-TRAIT-NOM-PREN-DATE-FIN  SECTION.

       11000-LECTURE-SEQ.
           READ FICHIER-ENTREE INTO W-FICHE-PERSONNELLE
                             AT END MOVE 1 TO W-IND-FIN-FICHIER.

       12000-TRAIT-VALIDATION-DATE.

           IF W-DATE-ENGAGEMENT >= 19900101
               RELEASE TRI-FICHE-PERSONNELLE FROM W-FICHE-PERSONNELLE
               ADD 1 TO W-COMPTE-FICHES
            END-IF.
            PERFORM 11000-LECTURE-SEQ.

      ******************************************************************
      *             INPUT PROCEDURE REGION/HEURE > 40
      ******************************************************************
       20000-REGION-HEURE  SECTION.
           OPEN  INPUT  FICHIER-ENTREE.
           MOVE 0 TO W-COMPTE-FICHES.
           MOVE 0 TO W-IND-FIN-FICHIER.
           PERFORM 11000-LECTURE-SEQ.
           PERFORM 21000-TRAIT-VALIDATION-HEURE UNTIL W-FIN-FICHIER
           CLOSE FICHIER-ENTREE.
       20000-REGION-HEURE-FIN  SECTION.

       21000-TRAIT-VALIDATION-HEURE.
           IF W-NB-HEURES > 40
               RELEASE TRI-FICHE-PERSONNELLE FROM W-FICHE-PERSONNELLE
               ADD 1 TO W-COMPTE-FICHES
           END-IF.
           PERFORM 11000-LECTURE-SEQ.
      ******************************************************************
      *                    OUTPUT PROCEDURE
      ******************************************************************
       30000-TRAIT-SORTIE SECTION.
         OPEN OUTPUT RAPPORT-IMPRIME.
         PERFORM 20000-INITIALISATION
         PERFORM 31000-IMPRESSION-ENTETE
         PERFORM 32000-LECTURE-TRI.
         PERFORM 50000-TRAITEMENT UNTIL W-FIN-FICHIER.
         PERFORM 60000-STATISTIQUE.
         CLOSE RAPPORT-IMPRIME.
         MOVE "LE RAPPORT A ETE PRODUIT !" TO W-MESSAGE-ERREUR.
       30000-TRAIT-SORTIE-FIN SECTION.



      **************************************************************
      *  Écrire les 3 lignes d'entête dans le fichier rapport.
      **************************************************************
       31000-IMPRESSION-ENTETE.

           MOVE W-COMPTE-FICHES TO EG-NO-PAGE
           ADD 1 TO W-COMPTE-FICHES

           WRITE LIGNE-IMPRIME FROM W-ENTETE-GENERALE
                               AFTER ADVANCING PAGE.
           WRITE LIGNE-IMPRIME FROM W-ENTETE-DETAIL-1
                               AFTER ADVANCING 2 LINES.
           WRITE LIGNE-IMPRIME FROM W-ENTETE-DETAIL-2
                               AFTER ADVANCING 1 LINE.

      **************************************************************
      *  Lire un enregistrement du fichier employés
      **************************************************************
       32000-LECTURE-TRI.
           RETURN FICHIER-TRI INTO W-FICHE-PERSONNELLE
                             AT END MOVE 1 TO W-IND-FIN-FICHIER.



      **************************************************************
      *  Initialiser les indicateurs, compteurs et totalisateurs
      **************************************************************
       20000-INITIALISATION.
           MOVE 0    TO W-IND-FIN-FICHIER.
           MOVE ZERO TO W-COMPTE-LIGNES.
           MOVE 1 TO W-COMPTE-FICHES.
           MOVE ZERO TO W-CUMUL-BRUT.
           MOVE ZERO TO W-CUMUL-NET.
           MOVE 0 TO W-COMPTE-EMPLOYE.

      **************************************************************
      *  Pour chaque employé, le calcul du salaire et le calcul
      *  des cumulatifs est effectué.  Les informations à imprimer
      *  sont préparées, puis imprimées.  Si plus de 20 lignes ont
      *  été imprimées, un saut de page est effectué et une nouvelle
      *  entête est imprimée.
      **************************************************************
       50000-TRAITEMENT.
           PERFORM 51000-CALCUL-SALAIRE.
           PERFORM 52000-CALCUL-CUMULATIFS.
           PERFORM 53000-TRANSFERT-INFO.
           IF W-COMPTE-LIGNES > 20
               MOVE ZERO TO W-COMPTE-LIGNES
               PERFORM 31000-IMPRESSION-ENTETE
           END-IF

           PERFORM 54000-ECRITURE-FICHE.
           PERFORM 32000-LECTURE-TRI.
           ADD 1 TO W-COMPTE-EMPLOYE.
      **************************************************************
      * Si le nombre d'heure travaillé par l'employé est plus grand que
      * 40 on multiplie le taux horraire de lemployer par 1.5 et on le
      * stoque dans une variable, on soustrait 40 du nombre d'heure
      * travailler et on stoque le reste dans un variable, on multiplie
      * 40 par le taux horraire normale et on stoque dans une variable,
      * on multiplie le nombre d'heures à taux demi par le
      * taux-horraire à taux demi on stoque dans un variable. On ajoute
      * le salaire à taux demi au salaire brut "normale".
      **************************************************************
       51000-CALCUL-SALAIRE.
            IF W-NB-HEURES > 40
                  MULTIPLY W-TAUX BY 1.5 GIVING W-TAUX-DEMI

                  SUBTRACT 40 FROM W-NB-HEURES
                       GIVING W-NB-HEURES-TAUX-DEMIE
                  SUBTRACT W-NB-HEURES-TAUX-DEMIE FROM W-NB-HEURES

                  MULTIPLY W-TAUX BY W-NB-HEURES GIVING W-SALAIRE-BRUT
                  MULTIPLY W-TAUX-DEMI BY W-NB-HEURES-TAUX-DEMIE
                       GIVING W-SALAIRE-TAUX-DEMI

                  ADD W-SALAIRE-TAUX-DEMI TO W-SALAIRE-BRUT
                  ADD W-NB-HEURES-TAUX-DEMIE TO W-NB-HEURES
            ELSE
                  MULTIPLY W-TAUX BY W-NB-HEURES GIVING W-SALAIRE-BRUT
            END-IF

      *CHOIX DU TAUX D'IMPOSITION SELON LE SALAIRE BRUT TOTAL
            IF W-SALAIRE-BRUT > 500
                  MOVE .15 TO W-TAUX-FEDERAL
                  MOVE .16 TO W-TAUX-PROV
            ELSE
                  MOVE .12 TO W-TAUX-FEDERAL
                  MOVE .14 TO W-TAUX-PROV
            END-IF

      *CALCUL DES IMPOTS A SOUSTRAIRE DU SALAIRE BRUT
            MULTIPLY W-SALAIRE-BRUT BY W-TAUX-FEDERAL
            GIVING W-IMPOT-FEDERAL.

            MULTIPLY W-SALAIRE-BRUT BY W-TAUX-PROV
            GIVING W-IMPOT-PROV.

      *CALCUL DU SALAIRE NET
           SUBTRACT W-IMPOT-FEDERAL W-IMPOT-PROV FROM W-SALAIRE-BRUT
           GIVING W-SALAIRE-NET.


      **************************************************************
      * On ajoute les salaires calculés aux variable cumulatives.
      **************************************************************
       52000-CALCUL-CUMULATIFS.

          ADD W-SALAIRE-BRUT TO W-CUMUL-BRUT.
          ADD W-SALAIRE-NET TO W-CUMUL-NET.


      **************************************************************
      * Transfère des informations "variable" dans leur format édité
      * pour l'affichage sur écran/rapport vers Ligne-detail.
      * 1 lignes par employé

      **************************************************************
       53000-TRANSFERT-INFO.
           MOVE W-CODE            TO W-LD-CODE.
           MOVE W-NOM             TO W-LD-NOM.
           MOVE W-PRENOM          TO W-LD-PRENOM.
           MOVE W-REGION          TO W-LD-REGION.
           MOVE W-TAUX            TO W-LD-TAUX.
           MOVE W-NB-HEURES       TO W-LD-NB-HEURES.
           MOVE W-DATE-ENGAGEMENT TO W-LD-DATE-ENGAGEMENT.

           MOVE W-SALAIRE-BRUT    TO W-LD-SALAIRE-BRUT.
           MOVE W-SALAIRE-NET     TO W-LD-SALAIRE-NET.
           MOVE W-IMPOT-PROV      TO W-LD-IMPOT-PROV.
           MOVE W-IMPOT-FEDERAL   TO W-LD-IMPOT-FEDERAL.

      **************************************************************
      * On écrit dans le document "sorti.doc" la ligne W-LIGNE-DETAIL
      * après avoir avancé 2 ligne (Pour la mise en forme).
      * On ajoute 1 au compteur de lignes.
      **************************************************************
       54000-ECRITURE-FICHE.

           WRITE LIGNE-IMPRIME FROM W-LIGNE-DETAIL
                               AFTER ADVANCING 2 LINES.
           ADD 1 TO W-COMPTE-LIGNES.








      **************************************************************
      *  Transfére de la variable compte employe vers W-Ligne sommaire
      *  pour l'affichage/impression. Si le compteur employé est vide
      *  on imprime "Le fichier est vide" sur le rapport. Si le fichier
      *  n'est pas vide on calcul la moyenne du salaire brut et du
      *  salaire net. On imprime les données sur le rapport.
      **************************************************************
       60000-STATISTIQUE.
             MOVE W-COMPTE-EMPLOYE TO W-LS-NB-EMPLOYE.
             IF W-COMPTE-EMPLOYE > 0

               DIVIDE W-CUMUL-BRUT BY W-COMPTE-EMPLOYE
               GIVING W-LS-SALAIRE-BRUT

               DIVIDE W-CUMUL-NET BY W-COMPTE-EMPLOYE
               GIVING W-LS-SALAIRE-NET


               WRITE LIGNE-IMPRIME FROM W-LIGNE-SOMMAIRE-1
               AFTER ADVANCING 2 LINES
               WRITE LIGNE-IMPRIME FROM W-LIGNE-SOMMAIRE-2

             ELSE
                  WRITE LIGNE-IMPRIME FROM "LE FICHIER EST VIDE"
             END-IF


