      ******************************************************************
      * Author: Nadir Pelletier
      * Date:   2019/04/10
      * Purpose:

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP4-NADIR-PELLETIER.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

      ****************************BACKUP********************************
       01 W-NOUV-FICH-DAT.
           05 W-NOUV-FICH-DAT-REP      PIC X(7)      VALUE "BACKUP\".
           05 W-NOUV-FICH-DAT-NOM      PIC X(9)      VALUE "EMPLOYES_".
           05 W-NOUV-FICH-DAT-DATE     PIC X(8).
           05 FILLER                   PIC X         VALUE "_".
           05 W-NOUV-FICH-DAT-HEURE    PIC X(8).
           05 W-NOUV-FICH-DAT-EXT      PIC X(5)      VALUE ".DAT ".

       01 W-NOUV-FICH-IDX.
           05 W-NOUV-FICH-IDX-REP      PIC X(7)      VALUE "BACKUP\".
           05 W-NOUV-FICH-IDX-NOM      PIC X(9)      VALUE "EMPLOYES_".
           05 W-NOUV-FICH-IDX-DATE     PIC X(8).
           05 FILLER                   PIC X         VALUE "_".
           05 W-NOUV-FICH-IDX-HEURE    PIC X(8).
           05 W-NOUV-FICH-IDX-EXT      PIC X(5)      VALUE ".IDX ".
      *****************************VARIABLES****************************

       01  W-VARIABLE.
           05  W-DATE-DU-JOUR          PIC 9(8).
           05  DUMMY                   PIC X.

       01 BOOL-BACKUP-DAT              PIC 9.
           88  DAT-OK                                VALUE 1.

       01 BOOL-BACKUP-IDX              PIC 9.
           88  IDX-OK                                VALUE 1.



      *****************************CHOIX********************************
       01  W-CHOIX.
      ************************CHOIX-MENU-PRINCIPAL**********************
           05  W-CHOIX-PRINCIPAL       PIC X       VALUE SPACE.
               88  QUITTER-P           VALUE "Q" "q".
               88  W-CHOIX-P-VALIDE    VALUE "1" "2" "3" "4" "Q" "q".

           05  W-CHOIX-UTILITAIRE      PIC X       VALUE SPACE.
               88  QUITTER-U           VALUE "Q" "q".
               88  W-CHOIX-U-VALIDE    VALUE "1" "2" "Q" "q".

      ****************************ENTETE********************************
       01  W-ENTETE.
          5  FILLER              PIC X(17)
                                 VALUE "LA CIE CRACK-INFO".
          5  FILLER              PIC X(10).
          5  FILLER              PIC X(29)
                                 VALUE "Gestion des employe(e)s".
          5  FILLER              PIC X(14).
          5  W-ENTETE-DATE       PIC 9999/99/99.

      ****************************MESSAGES******************************
       01 W-MESSAGES.
           05  W-MESSAGE-ERREUR PIC X(80).
           05  W-MESSAGE-ARIANE PIC X(80).

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
           05  LINE 5   COLUMN 33 VALUE "MENU PRINCIPAL" UNDERLINE.
           05  LINE 8   COLUMN 30 VALUE "1) Calcul de la paye".
           05  LINE 10  COLUMN 30 VALUE "2) Interrogation des Employes".
           05  LINE 12  COLUMN 30 VALUE "3) Mise a jour des Employes".
           05  LINE 14  COLUMN 30 VALUE "4) Utilitaires".
           05  LINE 16  COLUMN 30 VALUE "Q) Quitter".
           05  LINE 19  COLUMN 3  VALUE "Votre choix (1, 2, 3, 4, Q): ".
           05  LINE 19  COLUMN 32 PIC X TO W-CHOIX-PRINCIPAL.

       01  MENU-UTILITAIRE.
           05  LINE 5   COLUMN 33 VALUE "MENU UTILITAIRES" UNDERLINE.
           05  LINE 8   COLUMN 30 VALUE "1) Reconstruire EMPLOYE.DAT".
           05  LINE 10  COLUMN 30 VALUE "2) Copier EMPLOYE.DAT".
           05  LINE 12  COLUMN 30 VALUE "Q) Quitter".
           05  LINE 19  COLUMN 3  VALUE "Votre choix (1, 2, Q): ".
           05  LINE 19  COLUMN 28 PIC X TO W-CHOIX-UTILITAIRE.




       PROCEDURE DIVISION.

       00000-MAIN-PROCEDURE.
            PERFORM 10000-MENU-PRINCIPAL.
            PERFORM UNTIL QUITTER-P
               MOVE SPACE TO W-MESSAGE-ERREUR
               EVALUATE W-CHOIX-PRINCIPAL
                    WHEN "1"
                      CALL"CALCULSALAIRE"

                    WHEN "2"
                      CALL"TP2NADIRPELLETIER" *>

                    WHEN "3"
                      CALL"TP3NADIRPELLETIER" *>

                    WHEN "4"
                      PERFORM 20000-MENU-UTILITAIRE
                      PERFORM UNTIL QUITTER-U
                           EVALUATE W-CHOIX-UTILITAIRE
                               WHEN "1"
                                   PERFORM 30000-BACKUP
                                   CALL "REBUILD" USING W-MESSAGE-ERREUR
                               WHEN "2"
                                   PERFORM 30000-BACKUP*>
                           END-EVALUATE
                      PERFORM 20000-MENU-UTILITAIRE
                      END-PERFORM

                END-EVALUATE

                PERFORM 10000-MENU-PRINCIPAL
             END-PERFORM.


              DISPLAY "Au revoir !"  AT 2404 FOREGROUND-COLOR 7
                                             BACKGROUND-COLOR 4.
              ACCEPT DUMMY          AT 2580.


       STOP RUN.

       10000-MENU-PRINCIPAL.
           ACCEPT W-DATE-DU-JOUR FROM DATE.
           MOVE W-DATE-DU-JOUR TO W-ENTETE-DATE.
           MOVE FUNCTION CURRENT-DATE(1:8)  TO  W-ENTETE-DATE
                                                W-DATE-DU-JOUR.


           MOVE SPACE TO W-CHOIX-PRINCIPAL W-MESSAGE-ERREUR.
           PERFORM UNTIL W-CHOIX-P-VALIDE
               DISPLAY FOND-ECRAN
               DISPLAY MENU-PRINCIPAL
               ACCEPT  MENU-PRINCIPAL

               MOVE FUNCTION UPPER-CASE(W-CHOIX-PRINCIPAL)
               TO W-CHOIX-PRINCIPAL

               IF NOT W-CHOIX-P-VALIDE
                   MOVE "ENTREZ 1, 2, 3, 4, OU Q" TO W-MESSAGE-ERREUR
               END-IF
           END-PERFORM.

       20000-MENU-UTILITAIRE.

            MOVE SPACE TO W-CHOIX-UTILITAIRE W-MESSAGE-ARIANE.
            PERFORM UNTIL W-CHOIX-U-VALIDE
                DISPLAY FOND-ECRAN
                DISPLAY MENU-UTILITAIRE
                ACCEPT  MENU-UTILITAIRE

                MOVE FUNCTION UPPER-CASE(W-CHOIX-UTILITAIRE)
                TO W-CHOIX-UTILITAIRE

                IF NOT W-CHOIX-U-VALIDE
                    MOVE "ENTREZ 1, 2 OU Q" TO W-MESSAGE-ERREUR
                END-IF
            END-PERFORM.


       30000-BACKUP.
           INITIALIZE BOOL-BACKUP-DAT BOOL-BACKUP-IDX.
           ACCEPT W-NOUV-FICH-DAT-HEURE FROM TIME.
           ACCEPT W-NOUV-FICH-IDX-HEURE FROM TIME.
           MOVE W-DATE-DU-JOUR
           TO W-NOUV-FICH-DAT-DATE W-NOUV-FICH-IDX-DATE.

           CALL "CBL_COPY_FILE" USING "EMPLOYES.DAT " W-NOUV-FICH-DAT.
           IF RETURN-CODE = 0
               MOVE 1 TO BOOL-BACKUP-DAT

              CALL "CBL_COPY_FILE" USING "EMPLOYES.IDX " W-NOUV-FICH-IDX
               IF RETURN-CODE = 0
                   MOVE 1 TO BOOL-BACKUP-IDX
               ELSE
                 MOVE "ERREUR LORS DE LA COPIE DU (.IDX)"
                   TO W-MESSAGE-ERREUR
               END-IF
           ELSE
               MOVE "ERREUR LORS DE LA COPIE DU (.DAT)"
               TO W-MESSAGE-ERREUR
           END-IF
      **************************VERIFICATION****************************
           IF  NOT DAT-OK AND NOT IDX-OK
               MOVE "ERREUR LORS DE LA COPIE DU (.DAT) ET DU (.IDX)"
               TO W-MESSAGE-ERREUR
           END-IF

           IF  DAT-OK AND IDX-OK
               MOVE "BACKUP EFFECTUE AVEC SUCCES" TO W-MESSAGE-ERREUR
           END-IF

           MOVE SPACE TO W-CHOIX-UTILITAIRE.

