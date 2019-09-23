      ******************************************************************
      * Author: NADIR PELLETIER
      * Date:   2019-04-17
      * Purpose: SOUS-PROGRAMME-TP3 (vérifie si l'année est bisextile)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDE-DATE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 W-DATE.
           05  W-ANNEE             PIC 9(4).
           05  W-MOIS              PIC 99.
               88 W-MOIS-VALIDE            VALUE 01 THRU 12.
           05  W-JOUR              PIC 99.
               88 W-JOUR-VALIDE            VALUE 01 THRU 31.

       01 W-BOOL-BISEXTILE         PIC 9   VALUE 0.
           88 W-IS-BISEXTILE               VALUE 1.





       LINKAGE SECTION.

       01  W-EMP-DATE-ENGAGEMENT   PIC 9(8).

       01  W-TAB-IND-ERREUR-MOIS   PIC 9.
       01  W-TAB-IND-ERREUR-JOUR   PIC 9.




       PROCEDURE DIVISION USING W-EMP-DATE-ENGAGEMENT
                                W-TAB-IND-ERREUR-MOIS
                                W-TAB-IND-ERREUR-JOUR.

       00000-MAIN-PROCEDURE.

           MOVE W-EMP-DATE-ENGAGEMENT TO W-DATE.

           IF NOT W-MOIS-VALIDE
               MOVE 1 TO W-TAB-IND-ERREUR-MOIS
           END-IF

           IF NOT W-JOUR-VALIDE
               MOVE 1 TO W-TAB-IND-ERREUR-JOUR
           END-IF

      ******************************************************************
      *Vérifie si l'année entrée est bisextile ou non
      ******************************************************************
           IF ((FUNCTION MOD(W-ANNEE,4) = 0
               AND
               FUNCTION MOD(W-ANNEE,100) NOT = 100)
               OR
               (FUNCTION MOD(W-ANNEE,400) = 0))

                   MOVE 1 TO W-BOOL-BISEXTILE
           END-IF


           IF NOT W-IS-BISEXTILE AND W-MOIS = 02 AND W-JOUR > 28
               MOVE 1 TO W-TAB-IND-ERREUR-JOUR
           END-IF

           IF W-IS-BISEXTILE AND W-MOIS = 02 AND W-JOUR > 29
               MOVE 1 TO W-TAB-IND-ERREUR-JOUR
           END-IF


           IF ((W-MOIS = 04 OR 06 OR 09 OR 11) AND W-JOUR > 30)
                MOVE 1 TO W-TAB-IND-ERREUR-JOUR
           END-IF







            EXIT PROGRAM.

