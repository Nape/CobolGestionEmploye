       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TP2NP.
       AUTHOR.         Nadir Pelletier.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FICHIER-IDX  ASSIGN TO "EMPLOYES.DAT"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
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
           05  EMP-SEXE                PIC X.
           05  EMP-NOM-PREN.
               10  EMP-NOM             PIC X(20).
               10  EMP-PREN            PIC X(15).
           05  EMP-DATE-ENGAGEMENT     PIC 9(8).
           05  EMP-TAUX                PIC 99V99.
           05  EMP-NB-HEURE            PIC 9(3).


       WORKING-STORAGE SECTION.
       01  W-INDICATEUR.
           05  W-IND-FIN-FICHIER    PIC 9 VALUE 0.
               88  W-FIN-FICHIER          VALUE 1.

      ****************************ENTETE********************************
       01  W-ENTETE.

          5  FILLER              PIC X(17)
                                 VALUE "LA CIE CRACK-INFO".
          5  FILLER              PIC X(10).
          5  FILLER              PIC X(29)
                                 VALUE "Interrogation des employe(e)s".
          5  FILLER              PIC X(14).
          5  W-ENTETE-DATE       PIC 9999/99/99.

      ****************************MESSAGES******************************
       01 W-MESSAGES.
           05  W-MESSAGE-ERREUR PIC X(80).
           05  W-MESSAGE-ARIANE PIC X(80).

      *****************************CHOIX********************************
       01  W-CHOIX.

      ************************CHOIX-MENU-PRINCIPAL**********************
           05  W-CHOIX-PRINCIPAL       PIC X       VALUE SPACE.
               88  QUITTER             VALUE "Q" "q".
               88  W-CHOIX-P-VALIDE    VALUE "1" "2" "3" "q" "Q".



      *****************************CHOIX-CLE****************************
           05  W-CHOIX-CLE             PIC X       VALUE SPACE.
               88 W-CHOIX-CLE-VALIDE   VALUE "1" "2" "3".





           01 W-ZONE-VARIABLE.
               05 WZV-SEXE                 PIC X       VALUE SPACE.
               05 WZV-NO-DE.
                   10 NO-DE-LETTRE         PIC A(4).
                   10 NO-DE-CHIFFRE        PIC 9(2).

               05 WZV-NO-A.
                   10 NO-A-LETTRE          PIC A(4).
                   10 NO-A-CHIFFRE         PIC 9(2).

               05 WZV-NOM-DE               PIC A(35).
               05 WZV-NOM-A                PIC A(35).

               05 WZV-DATE-DE              PIC 9999/99/99.
               05 WZV-DATE-A               PIC 9999/99/99.

               05 WZV-BOOL                 PIC 9      VALUE 0.

               05 DUMMY                    PIC X      VALUE SPACE.


       SCREEN SECTION.

           01  FOND-ECRAN.
               05  BLANK SCREEN     FOREGROUND-COLOR 1
                                       BACKGROUND-COLOR 7.
               05  LINE 1 COLUMN 1  FOREGROUND-COLOR 7
                                       BACKGROUND-COLOR 4
                                      PIC X(80) FROM W-ENTETE ERASE EOS.
               05  LINE 2 COLUMN 1  BLANK LINE
                                       BACKGROUND-COLOR 4.
               05  LINE 3 COLUMN 1  FOREGROUND-COLOR 1
                                       BACKGROUND-COLOR 8
                                       PIC X(80) FROM W-MESSAGE-ARIANE.
               05  LINE 23 COLUMN 1 FOREGROUND-COLOR 1
                                       BACKGROUND-COLOR 8
                                       PIC X(80) FROM W-MESSAGE-ERREUR.
               05  LINE 24 COLUMN 1 BACKGROUND-COLOR 4
                                       VALUE SPACES SIZE 80.
               05  LINE 25 COLUMN 1 BACKGROUND-COLOR 4
                                           VALUE SPACES SIZE 80.
       01  MENU-PRINCIPAL.
           05  LINE 5   COLUMN 33 VALUE "Menu choix du sexe" UNDERLINE.
           05  LINE 8   COLUMN 30 VALUE "1. Femmes seulement".
           05  LINE 10  COLUMN 30 VALUE "2. Hommes seulement".
           05  LINE 12  COLUMN 30 VALUE "3. Femmes et Hommes".
           05  LINE 15  COLUMN 30 VALUE "Q. Quitter".
           05  LINE 17  COLUMN 3  VALUE "Votre choix (1, 2, 3, Q): ".
           05  LINE 17  COLUMN 29 PIC X TO W-CHOIX-PRINCIPAL.


       01 MENU-CLE.
           05  LINE 5  COLUMN 33 VALUE "MENU CHOIX DE LA CLE".
           05  FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.
              10 LINE 8   COLUMN 30  VALUE "1. Par numero d'employe : ".
              10 LINE 10  COLUMN 30  VALUE "2. Par nom d'employe: ".
              10 LINE 12  COLUMN 30  VALUE "3. Par date d'embauche: ".
              10 LINE 17  COLUMN 3   VALUE "Votre choix (1, 2, 3): ".
              10 LINE 17  COLUMN 29  PIC X TO W-CHOIX-CLE.

       01 MENU-NO.
          05  FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.
             10 LINE 8   COLUMN 30
             VALUE "Entrez l'intervalle des numéro d'employes".

             10 LINE 10  COLUMN 30  VALUE "De :".
             10 LINE 10  COLUMN 40  PIC X(6) USING WZV-NO-DE.
             10 LINE 12  COLUMN 30  VALUE "A :".
             10 LINE 12  COLUMN 40  PIC X(6) USING WZV-NO-A.


       01 MENU-NOM.
          05  FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.
             10 LINE 8   COLUMN 30
             VALUE "Entrez l'intervalle des noms de famille".

             10 LINE 10  COLUMN 30  VALUE "De :".
             10 LINE 10  COLUMN 40  PIC X(8) USING WZV-NOM-DE.
             10 LINE 12  COLUMN 30  VALUE "A :".
             10 LINE 12  COLUMN 40  PIC X(8) USING WZV-NOM-A.


       01 MENU-DATE.
          05  FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.
             10 LINE 8   COLUMN 30
             VALUE "Entrez l'intervalle des noms de famille".

             10 LINE 10  COLUMN 30  VALUE "De :".
             10 LINE 10  COLUMN 40  PIC X(8) USING WZV-DATE-DE.
             10 LINE 12  COLUMN 30  VALUE "A :".
             10 LINE 12  COLUMN 40  PIC X(8) USING WZV-DATE-A.

       01 MENU-AFFICHAGE.
           05  LINE 5   COLUMN 23 VALUE "DETAIL DE L'EMPLOYE" UNDERLINE.
           05  LINE 8   COLUMN 23 VALUE "Numero: ".
           05  LINE 8   COLUMN 47 PIC X(6) FROM EMP-CODE.

           05  LINE 9   COLUMN 23 VALUE "Nom:".
           05  LINE 9   COLUMN 47 PIC X(20) FROM EMP-NOM.

           05  LINE 10   COLUMN 23 VALUE "Prenom:".
           05  LINE 10   COLUMN 47 PIC X(15) FROM EMP-PREN.

           05  LINE 11  COLUMN 23 VALUE "Sexe:".
           05  LINE 11  COLUMN 47 PIC X FROM EMP-SEXE.

           05  LINE 12  COLUMN 23 VALUE "Region:".
           05  LINE 12  COLUMN 47 PIC X(2) FROM EMP-REGION.

           05  LINE 13  COLUMN 23 VALUE "Taux horaire".
           05  LINE 13  COLUMN 47 PIC $99.99 FROM EMP-TAUX.

           05  LINE 14  COLUMN 23 VALUE "Nombre d'heures:".
           05  LINE 14  COLUMN 47 PIC Z(3) FROM EMP-NB-HEURE.

           05  LINE 15  COLUMN 23 VALUE "Date d'embauche:".
           05  LINE 15  COLUMN 47 PIC 9(4)/9(2)/9(2)
                                  FROM EMP-DATE-ENGAGEMENT.

           05  LINE 17  COLUMN 14 VALUE "Appuyez sur ENTER pour continue
      -"r.."..

           05 LINE 25   COLUMN 80 PIC X USING DUMMY.





      ******************************************************************

       PROCEDURE DIVISION.

       00000-PRINCIPAL.
          OPEN  INPUT  FICHIER-IDX.
          MOVE SPACE   TO EMP-CODE.

          PERFORM 10000-MENU-PRINCIPAL.
          PERFORM UNTIL QUITTER


               PERFORM 11000-MENU-CLE
               EVALUATE W-CHOIX-CLE
                       WHEN "1"
                           PERFORM 11100-MENU-NO   UNTIL WZV-BOOL = 1
                           PERFORM 20000-LIRE-FICHIER-CODE UNTIL
                           W-FIN-FICHIER
                       WHEN "2"
                           PERFORM 11200-MENU-NOM  UNTIL WZV-BOOL = 1
                           PERFORM 20000-LIRE-FICHIER-CODE UNTIL
                           W-FIN-FICHIER
                       WHEN "3"
                           PERFORM 11300-MENU-DATE UNTIL WZV-BOOL = 1
                           PERFORM 20000-LIRE-FICHIER-CODE UNTIL
                           W-FIN-FICHIER
               END-EVALUATE
               PERFORM 10000-MENU-PRINCIPAL

           END-PERFORM
           CLOSE FICHIER-IDX
           EXIT PROGRAM.

       10000-MENU-PRINCIPAL.
           DISPLAY FOND-ECRAN.
      ***GET LA DATE ET FORMAT POUR 2019.***
           ACCEPT W-ENTETE-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8)  TO  W-ENTETE-DATE.

      ***AFFICHE LE MENU JUSQUA CHOIX VALIDE*********
          PERFORM UNTIL W-CHOIX-P-VALIDE
              DISPLAY FOND-ECRAN
              DISPLAY MENU-PRINCIPAL
              ACCEPT  MENU-PRINCIPAL
              IF NOT W-CHOIX-P-VALIDE
                 MOVE "VOTRE CHOIX EST INVALIDE" TO W-MESSAGE-ERREUR
              END-IF
           END-PERFORM

      ***CHOIX DU SEXE SELON L'ENTRÉE****************
           EVALUATE W-CHOIX-PRINCIPAL
                WHEN "1"
                    MOVE "F" TO WZV-SEXE
                    MOVE "<Femmes seulement>" TO W-MESSAGE-ARIANE
                WHEN "2"
                    MOVE "M" TO WZV-SEXE
                    MOVE "<Hommes seulement>" TO W-MESSAGE-ARIANE
                WHEN "3"
                    MOVE "<Femme et Hommes>" TO W-MESSAGE-ARIANE
           END-EVALUATE.


       11000-MENU-CLE.
           PERFORM UNTIL W-CHOIX-CLE-VALIDE
               DISPLAY FOND-ECRAN
               DISPLAY MENU-CLE
               ACCEPT MENU-CLE
               IF NOT W-CHOIX-CLE-VALIDE
                  MOVE "ENTREZ 1, 2 OU 3" TO W-MESSAGE-ERREUR
               END-IF
            END-PERFORM.


       11100-MENU-NO.
           DISPLAY FOND-ECRAN.
           DISPLAY MENU-NO.
           ACCEPT  MENU-NO.

           PERFORM 11110-VALIDE-NO.

       11110-VALIDE-NO.
            IF  WZV-NO-DE = SPACE OR WZV-NO-DE = LOW-VALUE
                AND
                WZV-NO-A  = SPACE OR WZV-NO-A  = LOW-VALUE
                   MOVE SPACES TO W-CHOIX
                   PERFORM 10000-MENU-PRINCIPAL

            ELSE IF WZV-NO-DE = SPACE OR WZV-NO-DE = LOW-VALUE
                MOVE "    00" TO WZV-NO-DE

            ELSE IF WZV-NO-A  = SPACE OR WZV-NO-A  = LOW-VALUE
                MOVE"ZZZZ99" TO WZV-NO-A
            END-IF.


            MOVE FUNCTION UPPER-CASE(WZV-NO-DE) TO WZV-NO-DE.
            MOVE FUNCTION UPPER-CASE(WZV-NO-A)  TO WZV-NO-A.


         EVALUATE TRUE
            WHEN WZV-NO-DE > WZV-NO-A
                 MOVE " CLE INVALIDE" TO W-MESSAGE-ERREUR

            WHEN NO-DE-LETTRE  IS NOT ALPHABETIC
              OR NO-DE-CHIFFRE IS NOT NUMERIC
                 MOVE "La cle DE: doit être de 4 lettres et 2 chiffres"
                 TO W-MESSAGE-ERREUR

            WHEN NO-A-LETTRE IS NOT ALPHABETIC
              OR NO-A-CHIFFRE  IS NOT NUMERIC
                 MOVE "La cle A: doit être de 4 lettres et 2 chiffres"
                 TO W-MESSAGE-ERREUR

            WHEN OTHER
               MOVE 1 TO WZV-BOOl
               MOVE WZV-NO-DE TO EMP-CODE

           END-EVALUATE.



       11200-MENU-NOM.
           DISPLAY FOND-ECRAN.
           DISPLAY MENU-NOM.
           ACCEPT  MENU-NOM.

           PERFORM 11210-VALIDE-NOM.

       11210-VALIDE-NOM.
           IF
               WZV-NOM-DE = SPACE OR WZV-NOM-DE = LOW-VALUE
               AND
               WZV-NOM-A  = SPACE OR WZV-NOM-A  = LOW-VALUE
                   MOVE SPACES TO W-CHOIX
                   PERFORM 10000-MENU-PRINCIPAL
           END-IF.

           MOVE FUNCTION UPPER-CASE(WZV-NOM-DE) TO WZV-NOM-DE.
           MOVE FUNCTION UPPER-CASE(WZV-NOM-A)  TO WZV-NOM-A.

           IF  WZV-NOM-DE    IS NOT ALPHABETIC
               OR WZV-NOM-A  IS NOT ALPHABETIC
                MOVE "LE OU LES NOM(S) DE FAMILLE(S) INVALIDE(S)"
                TO W-MESSAGE-ERREUR

           ELSE
               MOVE 1 TO WZV-BOOl
               MOVE WZV-NOM-DE TO EMP-NOM-PREN

           END-IF.

       11300-MENU-DATE.
           DISPLAY FOND-ECRAN.
           DISPLAY MENU-DATE.
           ACCEPT  MENU-DATE.

           PERFORM  11310-VALIDE-DATE.

       11310-VALIDE-DATE.
           IF
               WZV-DATE-DE = SPACE OR WZV-DATE-DE = LOW-VALUE
               AND
               WZV-DATE-A  = SPACE OR WZV-DATE-A  = LOW-VALUE
                   MOVE SPACES TO W-CHOIX
                   PERFORM 10000-MENU-PRINCIPAL
           END-IF.

           IF WZV-DATE-DE > WZV-DATE-A
               MOVE "DATE DE DEBUT PLUS GRANDE QUE LA DATE DE FIN"
               TO W-MESSAGE-ERREUR

           ELSE IF WZV-DATE-DE IS NOT NUMERIC OR
                   WZV-DATE-A  IS NOT NUMERIC
                MOVE "LES DATES NE DOIVENT CONTENIR QUE DES CHIFFRES"
               TO W-MESSAGE-ERREUR
           ELSE
               MOVE 1 TO WZV-BOOl
               MOVE WZV-DATE-DE TO EMP-DATE-ENGAGEMENT
           END-IF.
      *************************LECTURE**********************************
       20000-LIRE-FICHIER-CODE.
             START  FICHIER-IDX   KEY >  EMP-CODE
                 INVALID KEY MOVE "NO EMP INVALIDE"
                 TO W-MESSAGE-ERREUR.

             READ FICHIER-IDX
               AT END MOVE 1 TO W-IND-FIN-FICHIER
               NOT AT END PERFORM 30000-AFFICHAGE
             END-READ.

       21000-LIRE-FICHIER-NOM.
              START  FICHIER-IDX   KEY >  EMP-NOM
                  INVALID KEY MOVE "NOM EMP INVALIDE"
                  TO W-MESSAGE-ERREUR.

              READ FICHIER-IDX
               AT END MOVE 1 TO W-IND-FIN-FICHIER
               NOT AT END PERFORM 30000-AFFICHAGE
            END-READ.

       22000-LIRE-FICHIER-DATE.
             START  FICHIER-IDX   KEY >  EMP-DATE-ENGAGEMENT
                 INVALID KEY MOVE "DATE EMP INVALIDE"
                 TO W-MESSAGE-ERREUR.

             READ FICHIER-IDX
                AT END MOVE 1 TO W-IND-FIN-FICHIER
                NOT AT END PERFORM 30000-AFFICHAGE
             END-READ.

      **************************AFFICHAGE*******************************

       30000-AFFICHAGE.
           IF EMP-SEXE = WZV-SEXE OR WZV-SEXE = SPACE
               DISPLAY FOND-ECRAN
               DISPLAY MENU-AFFICHAGE
               ACCEPT  MENU-AFFICHAGE
           END-IF.


