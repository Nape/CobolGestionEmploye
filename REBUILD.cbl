      ******************************************************************
      * Author: Nadir Pelletier
      * Date:   2019/04/27
      * Purpose: RECONSTRUIRE UN FICHIER INDEXÉ.

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REBUILD.

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

       SELECT FICHIER-TEMP-IDX  ASSIGN TO "TEMP-EMPLOYES.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TEMP-CODE
               ALTERNATE RECORD KEY IS TEMP-NOM-PREN
                          WITH DUPLICATES
               ALTERNATE RECORD KEY IS TEMP-DATE-ENGAGEMENT
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

       FD  FICHIER-TEMP-IDX
       LABEL RECORD STANDARD.
       01  TEMP-FICHE-PERSONNELLE.
           05  TEMP-CODE                PIC X(6).
           05  TEMP-REGION              PIC 9(2).
           05  TEMP-SEXE                PIC X.
           05  TEMP-NOM-PREN.
               10  TEMP-NOM             PIC X(20).
               10  TEMP-PREN            PIC X(15).
           05  TEMP-DATE-ENGAGEMENT     PIC 9(8).
           05  TEMP-TAUX                PIC 99V99.
           05  TEMP-NB-HEURE            PIC 9(3).



       WORKING-STORAGE SECTION.
       01  W-INDICATEUR.
           05  W-IND-FIN-FICHIER    PIC 9   VALUE 0.
               88  W-FIN-FICHIER            VALUE 1.

       01 BOOL-VERIFIER-DAT         PIC 9.
           88  DAT-OK                       VALUE 1.

       01 BOOL-VERIFIER-IDX         PIC 9.
           88  IDX-OK                       VALUE 1.

       01  W-NOM-DAT-TEMP      PIC X(18)    VALUE "TEMP-EMPLOYES.DAT ".
       01  W-NOM-IDX-TEMP      PIC X(18)    VALUE "TEMP-EMPLOYES.IDX ".
       01  W-FILE-DETAIL.
           05  W-SIZE          PIC  X(8)    COMP-X.



       LINKAGE SECTION.
       01  W-MESSAGE-ERREUR PIC X(80).


       PROCEDURE DIVISION USING W-MESSAGE-ERREUR.

       00000-MAIN.
           OPEN
               INPUT  FICHIER-IDX
               OUTPUT FICHIER-TEMP-IDX .

              MOVE 0 TO W-IND-FIN-FICHIER
              PERFORM 10000-LECTURE UNTIL W-FIN-FICHIER

           CLOSE FICHIER-IDX FICHIER-TEMP-IDX.

           PERFORM 30000-VERIFIER.
           IF DAT-OK AND IDX-OK
              PERFORM 40000-SUPPRIMER
           END-IF.

           IF DAT-OK AND IDX-OK
              PERFORM 50000-RENOMMER
              MOVE "REBUILD EFFECTUE AVEC SUCCES" TO W-MESSAGE-ERREUR
           END-IF.

           EXIT PROGRAM.
       10000-LECTURE.
           READ FICHIER-IDX INTO EMP-FICHE-PERSONNELLE
               AT END MOVE 1 TO W-IND-FIN-FICHIER
               NOT AT END PERFORM 20000-ECRITURE
           END-READ.

       20000-ECRITURE.
           WRITE TEMP-FICHE-PERSONNELLE FROM EMP-FICHE-PERSONNELLE.

       30000-VERIFIER.
      ******************************************************************
      *                    1-VERIFIE L'EXISTANCE DU .DAT
      *                    2-VERIFIE L'EXISTANCE DU .IDX
      ******************************************************************
           INITIALIZE BOOL-VERIFIER-DAT BOOL-VERIFIER-IDX.

           CALL "CBL_CHECK_FILE_EXIST" USING W-NOM-DAT-TEMP W-SIZE.
           IF RETURN-CODE = 0
               MOVE 1 TO BOOL-VERIFIER-DAT

               CALL "CBL_CHECK_FILE_EXIST" USING W-NOM-IDX-TEMP W-SIZE
               IF RETURN-CODE = 0
                   MOVE 1 TO BOOL-VERIFIER-IDX
               ELSE
                   MOVE "FICHIER (.IDX) INEXISTANT ARRET DE LA RECONSTRU
      -"CTION"     TO W-MESSAGE-ERREUR

               END-IF
           ELSE
               MOVE "FICHIER (.DAT) INEXISTANT ARRET DE LA RECONSTRUCTIO
      -"N"     TO W-MESSAGE-ERREUR
           END-IF.





       40000-SUPPRIMER.
           INITIALIZE BOOL-VERIFIER-DAT BOOL-VERIFIER-IDX.
           CALL "CBL_DELETE_FILE" USING "EMPLOYES.DAT "
           IF RETURN-CODE = 0
               MOVE 1 TO BOOL-VERIFIER-DAT
               MOVE "LE FICHIER (.DAT) EST DETRUIT."
               TO W-MESSAGE-ERREUR

               CALL "CBL_DELETE_FILE" USING "EMPLOYES.IDX "
               IF RETURN-CODE = 0
                   MOVE 1 TO BOOL-VERIFIER-IDX
                   MOVE "LE FICHIER (.IDX) EST DETRUIT."
                   TO W-MESSAGE-ERREUR
               ELSE
                  MOVE "ERREUR LORS DE LA DESTRUCTION DU FICHIER (.IDX)"
                  TO W-MESSAGE-ERREUR
               END-IF
           ELSE
               MOVE "ERREUR LORS DE LA DESTRUCTION DU FICHIER (.DAT)"
               TO W-MESSAGE-ERREUR
           END-IF.



       50000-RENOMMER.

           CALL "CBL_RENAME_FILE" USING "TEMP-EMPLOYES.DAT "
                                  "EMPLOYES.DAT ".

           CALL "CBL_RENAME_FILE" USING "TEMP-EMPLOYES.IDX "
                                  "EMPLOYES.IDX ".

