      ******************************************************************
      * Author: Nadir Pelletier
      * Date:   2019/04/10
      * Purpose:

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP3-NADIR-PELLETIER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FICHIER-IDX  ASSIGN TO "EMPLOYES.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE RANDOM
                   RECORD KEY IS EMP-CODE
                   ALTERNATE RECORD KEY IS EMP-NOM-PREN
                              WITH DUPLICATES
                   ALTERNATE RECORD KEY IS EMP-DATE-ENGAGEMENT
                              WITH DUPLICATES.



       DATA DIVISION.
       FILE SECTION.

       FD  FICHIER-IDX
       LABEL RECORD STANDARD.
       01  EMP-FICHE-PERSONNELLE.
           05  EMP-CODE                PIC X(6).
           05  EMP-REGION              PIC 9(2).
               88 EMP-REGION-VALIDE    VALUE 01 THRU 06.
           05  EMP-SEXE                PIC X.
               88 EMP-SEXE-VALIDE      VALUE "M" "F".
               88 EMP-SEXE-M           VALUE "M".
               88 EMP-SEXE-F           VALUE "F".
           05  EMP-NOM-PREN.
               10  EMP-NOM             PIC X(20).
                   88 EMP-NOM-VALIDE   VALUE "1".
               10  EMP-PREN            PIC X(15).
                   88 EMP-PREN-VALIDE  VALUE "1".
           05  EMP-DATE-ENGAGEMENT     PIC 9(8).
           05  EMP-TAUX                PIC 99V99.
               88  EMP-TAUX-VALIDE     VALUE 10 THRU 100.
           05  EMP-NB-HEURE            PIC 9(3).
               88  EMP-HEURE-VALIDE    VALUE 0  THRU 60.
      *                                TOTAL (59)
       WORKING-STORAGE SECTION.

       01  W-EMP-ENREGISTREMENT.
           05  W-EMP-CODE                PIC X(6).
           05  W-EMP-REGION              PIC 9(2).
           05  W-EMP-SEXE                PIC X.
               88 W-EMP-SEXE-VALIDE      VALUE "M" "F".
               88 W-EMP-SEXE-M           VALUE "M".
               88 W-EMP-SEXE-F           VALUE "F".
           05  W-EMP-NOM-PREN.
               10  W-EMP-NOM.
                   15  W-3FIRST.
                       20 W-3FIRST-FIRST PIC X.
                       20 W-3FIRST-RESTE PIC X(2).
                   15  W-RESTE-NOM       PIC X(17).
               10  W-EMP-PREN.
                   15  W-FIRST-L-PREN    PIC X.
                   15  W-RESTE-PREN      PIC X(14).
           05  W-EMP-DATE-ENGAGEMENT     PIC 9(8).
           05  W-EMP-TAUX                PIC 99V99.
           05  W-EMP-NB-HEURE            PIC 9(3).

      ****************************VARIABLE******************************
       01  W-VARIABLE.
           05  DUMMY                   PIC X.
           05  NUMKEY                  PIC 99.
           05  W-IND                   PIC 99.
           05  W-LONGUEUR              PIC 99  VALUE 17.
           05  W-LECTURE               PIC 9.
               88  W-LECTURE-TERMINE           VALUE 1.
           05  W-DATE-DU-JOUR          PIC 9(8).

           05  W-CMPT-SPACE-NOM        PIC 9.

           05  W-BOOL-NOM-PREN         PIC 9.
               88 W-NOM-PRENOM-OK              VALUE 1.

           05  W-BOOL-INFO             PIC 9.
               88 W-INFO-OK                    VALUE 1.

           05  W-BOOL-ERREUR           PIC 9.
               88 W-ERREUR-TROUVEE             VALUE 1.

           05 W-BOOL-SUPPRIMER         PIC X.
               88 W-SUPPRIMER-OUI               VALUE "O" "o".
               88 W-SUPPRIMER-NON               VALUE "N" "n".

           05 W-BOOL-RETOUR-MENU       PIC 9.
               88 W-RETOUR-MENU-OK             VALUE 1.

           05 W-BOOL-MODIFIER          PIC 9.
               88 W-MODIFIER-OK                VALUE 1.

      ****************************ENTETE********************************
       01  W-ENTETE.

          5  FILLER              PIC X(17)
                                 VALUE "LA CIE CRACK-INFO".
          5  FILLER              PIC X(10).
          5  FILLER              PIC X(29)
                                 VALUE "Mise a jour des employe(e)s".
          5  FILLER              PIC X(14).
          5  W-ENTETE-DATE       PIC 9999/99/99.

      ****************************MESSAGES******************************
       01 W-MESSAGES.
           05  W-MESSAGE-ERREUR PIC X(80).
           05  W-MESSAGE-ARIANE PIC X(80).

       01 W-TABLEAU.
           05  W-TAB-ERREUR.
      ******************************************************************
      *                ERREURS NOM-PRENOM IND 1 À 4
      ******************************************************************
               10 FILLER PIC X(51)
               VALUE "VEUILLEZ ENTREZ UN NOM.".
               10 FILLER PIC X(51)
               VALUE "VEUILLEZ ENTREZ UN PRENOM.".
               10 FILLER PIC X(51)
               VALUE "LES 3 PREMIERES LETTRE DE NOM DOIVEMT ETRE SAISIE.
      -"".
               10 FILLER PIC X(51)
               VALUE "LA PREMIERE LETTRE DU PRENOM DOIT ETRE SAISIE.".
      ******************************************************************
      *                ERREURS INFORMATION IND 5 À 10
      ******************************************************************
               10 FILLER PIC X(51)
               VALUE "LE SEXE DOIT ETRE M OU F.".
               10 FILLER PIC X(51)
               VALUE "LA REGION DOIT SE SITUE ENTRE 1 ET 6.".
               10 FILLER PIC X(51)
               VALUE "LE TAUX DOIT ETRE ENTRE 10$ ET 100$.".
               10 FILLER PIC X(51)
               VALUE "LES HEURES DOIVENT ETRE INFERIEUR A 60.".
               10 FILLER PIC X(51)
               VALUE "LA DATE DOIT ETRE SUPERIEUR A 1960.".
               10 FILLER PIC X(51)
               VALUE SPACES.


      ******************************************************************
      *                ERREURS BISEXTILE IND 11 ET 12
      ******************************************************************
               10 FILLER PIC X(51)
               VALUE "LE MOIS DE LA DATE EST INVALIDE (1 ET 12).".
               10 FILLER PIC X(51)
               VALUE "LE JOUR DE LA DATE EST INVALIDE.".
      ******************************************************************
      *               ERREURS CLÉ MODIFICATION IND 13
      ******************************************************************
               10 FILLER PIC X(51)
               VALUE "AUCUN EMPLOYE TROUVE.".

      ******************************************************************
      *                MESSAGE SUCCÈS IND 14 15 16
      ******************************************************************
              10 FILLER PIC X(51)
              VALUE "EMPLOYE AJOUTE AVEC SUCCES.".
              10 FILLER PIC X(51)
              VALUE "EMPLOYE MODIFIE AVEC SUCCES.".
              10 FILLER PIC X(51)
              VALUE "EMPLOYE SUPRIME AVEC SUCCES.".
      ******************************************************************
      *              MESSAGE AVANT SUPRESSION IND 17
      ******************************************************************
              10 FILLER PIC X(51)
              VALUE "VOULEZ-VOUS VRAIMENT SUPPRIMER CET EMPLOYE (O/N) ?.
      -"".





      ******************************************************************
      *           TABLEAU REDEFINE TAB-ERREUR.
      ******************************************************************
           05 W-TAB-ERREUR-R REDEFINES W-TAB-ERREUR PIC X(51) OCCURS 17.

           05 W-TAB-IND-ERREUR OCCURS 17.
               10 W-IND-ERREUR PIC 9 VALUE 0.
                   88 IND-ERR VALUE 1.

      *****************************CHOIX********************************
       01  W-CHOIX.
      ************************CHOIX-MENU-PRINCIPAL**********************
           05  W-CHOIX-PRINCIPAL       PIC X       VALUE SPACE.
               88  QUITTER             VALUE "Q" "q".
               88  W-CHOIX-P-VALIDE
                   VALUE "A" "M" "S" "Q" "a" "m" "s" "q".

      ******************************************************************

       SCREEN SECTION.

       01  FOND-ECRAN.
           05  BLANK SCREEN FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

           05  LINE  1 COLUMN 1 FOREGROUND-COLOR 7 BACKGROUND-COLOR 4
                                  PIC X(80) FROM W-ENTETE ERASE EOS.
           05  LINE  2 COLUMN 1  BLANK LINE
                                    BACKGROUND-COLOR 4.
           05  LINE  3 COLUMN 1  BLANK LINE
                                    BACKGROUND-COLOR 4.
           05  LINE  5 COLUMN 1   PIC X(80) FROM W-MESSAGE-ARIANE.
           05  LINE 23 COLUMN 1 FOREGROUND-COLOR 7
                                    BACKGROUND-COLOR 4
                                    PIC X(80) FROM W-MESSAGE-ERREUR.
           05  LINE 24 COLUMN 1 BACKGROUND-COLOR 4
                                    VALUE SPACES SIZE 80.
           05  LINE 25 COLUMN 1 BACKGROUND-COLOR 4
                                    VALUE SPACES SIZE 80.


       01  MENU-PRINCIPAL.
           05  LINE 5   COLUMN 30 VALUE "Mise a jour des Employes"
                                                       UNDERLINE.
           05  LINE 8   COLUMN 30 VALUE "A) Ajout".
           05  LINE 10  COLUMN 30 VALUE "M) Modification".
           05  LINE 12  COLUMN 30 VALUE "S) Supression".
           05  LINE 15  COLUMN 30 VALUE "Q. Quitter".
           05  LINE 17  COLUMN 3  VALUE "Votre choix (A, M, S, Q): ".
           05  LINE 17  COLUMN 29 PIC X TO W-CHOIX-PRINCIPAL.


      *************************SCREEN-TITRE-INFO************************
       01 SCREEN-INFO FOREGROUND-COLOR 0.
           05 SC-INFO-HAUT.
               10 LINE  6 COLUMN 17 VALUE "Numero de l'employe: ".
               10 LINE  8 COLUMN 17 VALUE "Nom: ".
               10 LINE 10 COLUMN 17 VALUE "Prenom: ".

           05 SC-INFO-BAS.
               10 LINE 12 COLUMN 17 VALUE "Sexe: ".
               10 LINE 14 COLUMN 17 VALUE "Region: ".
               10 LINE 16 COLUMN 17 VALUE "Taux Horaire: ".
               10 LINE 18 COLUMN 17 VALUE "Heure travaillee(s): ".
               10 LINE 20 COLUMN 17 VALUE "Date d'embauche: ".


       01 SCREEN-CLE-EMP FOREGROUND-COLOR 0.
           05 SC-CLE-HAUT.
               10 LINE  6 COLUMN 17 VALUE "Numero de l'employe: ".
               10 LINE  6 COLUMN 40 PIC X(6)
               USING  W-EMP-CODE UNDERLINE .



       01 SCREEN-AJOUT FOREGROUND-COLOR 0.
           05 SC-AJOUT-HAUT.
               10 LINE  6 COLUMN 40 PIC X(6)
               FROM  W-EMP-CODE UNDERLINE.
               10 LINE  8 COLUMN 40 PIC X(20)
               USING W-EMP-NOM  UNDERLINE.
               10 LINE 10 COLUMN 40 PIC X(15)
               USING W-EMP-PREN UNDERLINE.

           05 SC-AJOUT-BAS.
               10 LINE 12 COLUMN 40 PIC X
               USING W-EMP-SEXE     UNDERLINE.
               10 LINE 14 COLUMN 40 PIC 99
               USING W-EMP-REGION   UNDERLINE.
               10 LINE 16 COLUMN 40 PIC $99.99
               USING W-EMP-TAUX     UNDERLINE.
               10 LINE 18 COLUMN 40 PIC ZZ9
               USING W-EMP-NB-HEURE UNDERLINE.
               10 LINE 20 COLUMN 40 PIC 9(4)/99/99
               USING W-EMP-DATE-ENGAGEMENT UNDERLINE.


       01 SCREEN-MODIFIER FOREGROUND-COLOR 0.
           05 SC-MODIFER-HAUT.
               10 LINE  6 COLUMN 40 PIC X(6)
               FROM  W-EMP-CODE UNDERLINE.
               10 LINE  8 COLUMN 40 PIC X(20)
               FROM W-EMP-NOM  UNDERLINE.
               10 LINE 10 COLUMN 40 PIC X(15)
               FROM W-EMP-PREN UNDERLINE.

           05 SC-MODIFIER-BAS.
               10 LINE 12 COLUMN 40 PIC X
               USING W-EMP-SEXE     UNDERLINE.
               10 LINE 14 COLUMN 40 PIC 99
               USING W-EMP-REGION   UNDERLINE.
               10 LINE 16 COLUMN 40 PIC $99.99
               USING W-EMP-TAUX     UNDERLINE.
               10 LINE 18 COLUMN 40 PIC ZZ9
               USING W-EMP-NB-HEURE UNDERLINE.
               10 LINE 20 COLUMN 40 PIC 9(4)/99/99
               FROM W-EMP-DATE-ENGAGEMENT UNDERLINE.

       01 SCREEN-SUPPRIMER FOREGROUND-COLOR 0.
           05 SC-SUPPRIMER.
               10 LINE  6 COLUMN 40 PIC X(6)
               USING  W-EMP-CODE UNDERLINE.
               10 LINE  8 COLUMN 40 PIC X(20)
               USING W-EMP-NOM  UNDERLINE.
               10 LINE 10 COLUMN 40 PIC X(15)
               USING W-EMP-PREN UNDERLINE.
               10 LINE 12 COLUMN 40 PIC X
               USING W-EMP-SEXE     UNDERLINE.
               10 LINE 14 COLUMN 40 PIC 99
               USING W-EMP-REGION   UNDERLINE.
               10 LINE 16 COLUMN 40 PIC $99.99
               USING W-EMP-TAUX     UNDERLINE.
               10 LINE 18 COLUMN 40 PIC ZZ9
               USING W-EMP-NB-HEURE UNDERLINE.
               10 LINE 20 COLUMN 40 PIC 9(4)/99/99
               USING W-EMP-DATE-ENGAGEMENT UNDERLINE.

       PROCEDURE DIVISION.
       00000-MAIN-PROCEDURE.
           OPEN I-O FICHIER-IDX.

           PERFORM 10000-MENU-PRINCIPAL.
           PERFORM UNTIL QUITTER
              MOVE SPACE TO W-MESSAGE-ERREUR
              EVALUATE W-CHOIX-PRINCIPAL
                   WHEN "A"
                     PERFORM 20000-AJOUTER

                   WHEN "M"
                     PERFORM 30000-MODIFIER

                   WHEN "S"
                     PERFORM 40000-SUPPRIMER
               END-EVALUATE

               PERFORM 10000-MENU-PRINCIPAL
            END-PERFORM.


            DISPLAY "Appuyer sur ENTRER pour retourner au menu" AT 2004.

            ACCEPT DUMMY.

            CLOSE FICHIER-IDX.
        EXIT PROGRAM.

       10000-MENU-PRINCIPAL.
           ACCEPT W-DATE-DU-JOUR FROM DATE.
           MOVE W-DATE-DU-JOUR TO W-ENTETE-DATE.
           MOVE FUNCTION CURRENT-DATE(1:8)  TO  W-ENTETE-DATE
                                                W-DATE-DU-JOUR.

      ********POUR AFFICHAGE DE L'ERREUR > QUE DATE DU JOUR.***********
           STRING
           "LA DATE NE DOIT PAS DEPASSER: " DELIMITED BY SIZE
           W-ENTETE-DATE DELIMITED BY SIZE
           INTO W-TAB-ERREUR-R(10).


           MOVE SPACE TO W-CHOIX W-MESSAGE-ARIANE.
           PERFORM UNTIL W-CHOIX-P-VALIDE
               DISPLAY FOND-ECRAN
               DISPLAY MENU-PRINCIPAL
               ACCEPT  MENU-PRINCIPAL

               MOVE FUNCTION UPPER-CASE(W-CHOIX-PRINCIPAL)
               TO W-CHOIX-PRINCIPAL

               IF NOT W-CHOIX-P-VALIDE
                   MOVE "ENTREZ A, M, S OU Q" TO W-MESSAGE-ERREUR
               END-IF
           END-PERFORM.

       20000-AJOUTER.

           PERFORM 21000-INITIALISATION-VARIABLE.
           MOVE "<AJOUT>" TO W-MESSAGE-ARIANE.

           PERFORM UNTIL W-NOM-PRENOM-OK OR W-RETOUR-MENU-OK
               DISPLAY FOND-ECRAN
               DISPLAY SC-INFO-HAUT
               DISPLAY SC-AJOUT-HAUT
               ACCEPT  SC-AJOUT-HAUT

               IF (W-EMP-NOM EQUAL TO SPACES)
               OR (W-EMP-NOM EQUAL TO LOW-VALUES)
               AND
                  (W-EMP-PREN EQUAL TO SPACES)
               OR (W-EMP-NOM EQUAL TO LOW-VALUES)

                   MOVE 1 TO W-BOOL-RETOUR-MENU
               ELSE
                   MOVE FUNCTION LOWER-CASE(W-EMP-NOM-PREN)
                   TO W-EMP-NOM-PREN
                   PERFORM 22000-VALIDE-NOM-PRENOM


                   IF NOT W-NOM-PRENOM-OK
                       PERFORM 24000-AFFICHER-MESSAGES
                       PERFORM 25000-REINITIALISER-MESSAGES
                   END-IF
               END-IF
           END-PERFORM


           PERFORM UNTIL W-INFO-OK OR W-RETOUR-MENU-OK
               DISPLAY ALL SPACES AT 2401 BACKGROUND-COLOR 4
               DISPLAY SC-INFO-BAS
               DISPLAY SC-AJOUT-BAS
               ACCEPT  SC-AJOUT-BAS

               MOVE FUNCTION UPPER-CASE(W-EMP-SEXE) TO W-EMP-SEXE

               PERFORM 23000-VALIDE-INFO

               IF NOT W-INFO-OK
                   PERFORM 24000-AFFICHER-MESSAGES
                   PERFORM 25000-REINITIALISER-MESSAGES
               END-IF

           END-PERFORM.

           IF W-INFO-OK
               WRITE EMP-FICHE-PERSONNELLE FROM W-EMP-ENREGISTREMENT
               DISPLAY W-TAB-ERREUR-R(14) AT 2401
                               BACKGROUND-COLOR 4
                               FOREGROUND-COLOR 7
               ACCEPT DUMMY
           END-IF.
      ******************************************************************
      *   Initialise les variable utilisées pour évité les erreurs.
      ******************************************************************
       21000-INITIALISATION-VARIABLE.
           MOVE 0  TO W-LECTURE.
           MOVE 00 TO NUMKEY.
           MOVE 0  TO W-BOOL-NOM-PREN.
           MOVE 0  TO W-BOOL-INFO.
           INITIALIZE W-EMP-ENREGISTREMENT.
           MOVE 0 TO W-BOOL-RETOUR-MENU.



      ******************************************************************
      * VALIDATION DE INFORMATION SAISIE À L'ECRAN NOM ET PRENOM
      * APPELLE LE PARAGRAPHE GÉNÉRER CLÉ PAR LA SUITE.
      ******************************************************************
       22000-VALIDE-NOM-PRENOM.

      **VALIDE SI LES 3 PREMIERE LETTRE DU NOM CONTIENNENT DES ESPACE***
           INITIALIZE W-CMPT-SPACE-NOM.
           INSPECT W-3FIRST TALLYING
           W-CMPT-SPACE-NOM   FOR ALL SPACE.
      ******************************************************************
           IF (W-EMP-NOM EQUAL TO SPACES)
           OR (W-EMP-NOM EQUAL TO LOW-VALUES)

               MOVE 1 TO W-TAB-IND-ERREUR(1)
           END-IF

           IF (W-EMP-PREN EQUAL TO SPACES)
           OR (W-EMP-NOM EQUAL TO LOW-VALUES)

               MOVE 1 TO W-TAB-IND-ERREUR(2)
           END-IF

           IF (W-3FIRST EQUAL TO SPACES)
           OR (W-3FIRST EQUAL TO LOW-VALUES)
           OR (W-CMPT-SPACE-NOM > 0)

               MOVE 1 TO W-TAB-IND-ERREUR(3)
           END-IF

           IF (W-FIRST-L-PREN EQUAL TO SPACES)
           OR (W-FIRST-L-PREN EQUAL TO LOW-VALUES)

               MOVE 1 TO W-TAB-IND-ERREUR(4)
           END-IF

           PERFORM 26000-VERIFIER-ERREURS.

           IF NOT W-ERREUR-TROUVEE
               MOVE FUNCTION UPPER-CASE(W-FIRST-L-PREN)
               TO W-FIRST-L-PREN

               MOVE FUNCTION UPPER-CASE(W-3FIRST)
               TO W-3FIRST

               MOVE 1 TO W-BOOL-NOM-PREN
               PERFORM 22100-GENERER-CLE

           END-IF.

      ******************************************************************
      * GENÈRE ET TESTE UNE CLÉE JUSQUA NON TROUVÉE.
      ******************************************************************
       22100-GENERER-CLE.
           PERFORM 22200-INCREMENTER-NUMKEY.
           PERFORM UNTIL W-LECTURE-TERMINE


               READ FICHIER-IDX KEY IS EMP-CODE
                   INVALID KEY
                       MOVE 1 TO W-LECTURE
                       MOVE EMP-CODE TO W-EMP-CODE

                       MOVE FUNCTION LOWER-CASE(W-3FIRST-RESTE)
                       TO W-3FIRST-RESTE
                       DISPLAY SC-AJOUT-HAUT

                   NOT INVALID KEY
                       PERFORM 22200-INCREMENTER-NUMKEY

               END-READ
           END-PERFORM.
      ******************************************************************
      *AJOUTE 1 AU COMPTEUR NUMKEY ET CONCATÈNE LA CHANE POUR TESTÉ
      *L'EXISTANCE DE CELLE-CI DANS LE PARAGRAPHE 22100.
      ******************************************************************
       22200-INCREMENTER-NUMKEY.

           ADD 1 TO NUMKEY.
           STRING
               W-3FIRST DELIMITED BY SIZE
               W-FIRST-L-PREN DELIMITED BY SIZE
               NUMKEY DELIMITED BY SIZE
           INTO EMP-CODE
           MOVE FUNCTION UPPER-CASE(EMP-CODE) TO EMP-CODE.


       23000-VALIDE-INFO.

           IF  NOT W-EMP-SEXE-VALIDE
               MOVE 1 TO W-TAB-IND-ERREUR(5)
           END-IF

      ******************************************************************
      * ON AURAI PU METTRE PIC 9 POUR LA RÉGION, TOUJOURS ENTRE 1 & 6
      ******************************************************************

           IF W-EMP-REGION < 1 OR W-EMP-REGION > 6
               MOVE 1 TO W-TAB-IND-ERREUR(6)
           END-IF

      ******************************************************************
      *  W-EMP-TAUX > 100 NE SERT A RIEN LE TAUX EST PIC 99.
      ******************************************************************
           IF W-EMP-TAUX < 10 OR W-EMP-TAUX > 100
               MOVE 1 TO W-TAB-IND-ERREUR(7)
           END-IF
      ******************************************************************
           IF W-EMP-NB-HEURE < 0 OR W-EMP-NB-HEURE > 60
               MOVE 1 TO W-TAB-IND-ERREUR(8)
           END-IF

           IF W-EMP-DATE-ENGAGEMENT < 19600101
               MOVE 1 TO W-TAB-IND-ERREUR(9)
           END-IF

           IF W-EMP-DATE-ENGAGEMENT > FUNCTION NUMVAL(W-DATE-DU-JOUR)
               MOVE 1 TO W-TAB-IND-ERREUR(10)
           END-IF

      ******************************************************************
      *        APPELLE AU SOUS-PROGRAMME VALIDE-DATE
      ******************************************************************

           CALL "VALIDE-DATE"
               USING BY REFERENCE W-EMP-DATE-ENGAGEMENT
               W-TAB-IND-ERREUR(11)
               W-TAB-IND-ERREUR(12)

      ******************************************************************

           PERFORM 26000-VERIFIER-ERREURS.

           IF NOT W-ERREUR-TROUVEE
               MOVE 1 TO W-BOOL-INFO
           END-IF.

      ******************************************************************
      *    AFFICHE LES MESSAGE OU LES INDICE SONT À 1 (TRUE)
      ******************************************************************
       24000-AFFICHER-MESSAGES.
           PERFORM VARYING W-IND FROM 1 BY 1 UNTIL W-IND > W-LONGUEUR
               IF IND-ERR(W-IND)
                  DISPLAY W-TAB-ERREUR-R(W-IND) AT LINE 24 COL 1
                           BACKGROUND-COLOR 4
                           FOREGROUND-COLOR 7
                   ACCEPT DUMMY AT LINE 25 COL 80
               END-IF
           END-PERFORM.


      ******************************************************************
      *   Parcour w-indice erreur et remet à false (0) si a true (1).
      ******************************************************************
       25000-REINITIALISER-MESSAGES.
           PERFORM VARYING W-IND FROM 1 BY 1 UNTIL W-IND > W-LONGUEUR
               IF IND-ERR(W-IND)
                   MOVE 0 TO W-IND-ERREUR(W-IND)
               END-IF
           END-PERFORM.

      ******************************************************************
      *            Vérifie la présence d'erreur.
      ******************************************************************
       26000-VERIFIER-ERREURS.
           MOVE 0 TO W-BOOL-ERREUR.
           PERFORM VARYING W-IND FROM 1 BY 1 UNTIL W-IND > W-LONGUEUR
               IF IND-ERR(W-IND)
                  MOVE 1 TO W-BOOL-ERREUR
               END-IF
           END-PERFORM.


       30000-MODIFIER.
            INITIALIZE W-LECTURE W-BOOL-RETOUR-MENU.
            PERFORM 21000-INITIALISATION-VARIABLE.
            MOVE "<MODIFICATION>" TO W-MESSAGE-ARIANE.

            PERFORM UNTIL W-LECTURE-TERMINE OR W-RETOUR-MENU-OK

               INITIALIZE W-EMP-CODE
               DISPLAY FOND-ECRAN
               DISPLAY SC-CLE-HAUT
               ACCEPT  SC-CLE-HAUT

               IF W-EMP-CODE = SPACE
                   MOVE 1 TO W-BOOL-RETOUR-MENU
               ELSE

                   MOVE FUNCTION UPPER-CASE(W-EMP-CODE) TO W-EMP-CODE
                   MOVE W-EMP-CODE TO EMP-CODE

                   READ FICHIER-IDX INTO W-EMP-ENREGISTREMENT
                   KEY IS EMP-CODE
                   INVALID KEY DISPLAY W-TAB-ERREUR-R(13) AT 2401
                                               BACKGROUND-COLOR 4
                                               FOREGROUND-COLOR 7
                       ACCEPT DUMMY

                   NOT INVALID KEY MOVE 1 TO W-LECTURE


                   PERFORM UNTIL W-INFO-OK

                       DISPLAY FOND-ECRAN
                       DISPLAY SC-INFO-HAUT
                       DISPLAY SC-MODIFER-HAUT
                       DISPLAY SC-INFO-BAS
                       DISPLAY SC-MODIFIER-BAS
                       ACCEPT  SC-MODIFIER-BAS

                       PERFORM 23000-VALIDE-INFO

                       IF NOT W-INFO-OK
                           PERFORM 24000-AFFICHER-MESSAGES
                           PERFORM 25000-REINITIALISER-MESSAGES
                       ELSE
                           REWRITE EMP-FICHE-PERSONNELLE
                           FROM W-EMP-ENREGISTREMENT

                           DISPLAY W-TAB-ERREUR-R(15) AT 2401
                                           BACKGROUND-COLOR 4
                                           FOREGROUND-COLOR 7
                           ACCEPT DUMMY
                       END-IF

                   END-PERFORM
            END-PERFORM.

       40000-SUPPRIMER.
            INITIALIZE W-LECTURE W-BOOL-SUPPRIMER W-BOOL-RETOUR-MENU.
            MOVE "<SUPPRESSION>" TO W-MESSAGE-ARIANE.
            PERFORM UNTIL (W-LECTURE-TERMINE
                       OR W-RETOUR-MENU-OK)
      *                OR (W-SUPPRIMER-OUI)

               INITIALIZE W-EMP-CODE W-BOOL-SUPPRIMER
               DISPLAY FOND-ECRAN
               DISPLAY SC-CLE-HAUT
               ACCEPT  SC-CLE-HAUT

               IF W-EMP-CODE = SPACE
                   MOVE 1 TO W-BOOL-RETOUR-MENU
               ELSE
                   MOVE FUNCTION UPPER-CASE(W-EMP-CODE) TO W-EMP-CODE
                   MOVE W-EMP-CODE TO EMP-CODE

                   READ FICHIER-IDX INTO W-EMP-ENREGISTREMENT
                   KEY IS EMP-CODE
                   INVALID KEY DISPLAY W-TAB-ERREUR-R(13) AT 2401
                                       BACKGROUND-COLOR 4
                                       FOREGROUND-COLOR 7
                   ACCEPT DUMMY

                   NOT INVALID KEY MOVE 1 TO W-LECTURE

                   PERFORM UNTIL W-RETOUR-MENU-OK
                   OR W-SUPPRIMER-NON OR W-SUPPRIMER-OUI

                       DISPLAY SC-SUPPRIMER
                       DISPLAY SC-INFO-HAUT
                       DISPLAY SC-INFO-BAS

                       DISPLAY W-TAB-ERREUR-R(17) AT 2201
                       ACCEPT W-BOOL-SUPPRIMER

                    IF W-SUPPRIMER-OUI
                        DELETE FICHIER-IDX
                        DISPLAY W-TAB-ERREUR-R(16) AT 2401
                                        BACKGROUND-COLOR 4
                                        FOREGROUND-COLOR 7
                        ACCEPT DUMMY
                        MOVE 0 TO W-LECTURE
                    ELSE IF W-SUPPRIMER-NON
                        MOVE 0 TO W-LECTURE
                    END-IF

                   END-PERFORM
           END-PERFORM

       END PROGRAM TP3-NADIR-PELLETIER.
