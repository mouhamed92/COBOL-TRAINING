       IDENTIFICATION DIVISION.
       PROGRAM-ID. Annuaire-2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  TABLE-ANNUAIRE.
         05 WS-CONTACT  OCCURS 100 TIMES.
           10  WS-NOM  PIC X(20).
           10  WS-TEL  PIC X(20).

       01  WS-I PIC 9(3).
       01  WS-NOM-TEM  PIC X(20) VALUES SPACES.
       01  WS-TEL-TEM  PIC X(20) VALUES SPACES.
       01  WS-CHOIX    PIC X(1).
       01  WS-VERIF    PIC X(1) VALUES "0".
       01  COUNTER PIC 9(2) VALUES 1 .

       PROCEDURE DIVISION.


           PERFORM MENU THRU FIN-MENU


      /     PERFORM UNTIL

                ACCEPT WS-CHOIX.

            EVALUATE WS-CHOIX
               WHEN  "1"
                PERFORM AJOUT-CONTACT THRU FIN-AJOUT

               WHEN  "2"
                PERFORM CHERCHER THRU FIN-CHERCHER

               WHEN  "3"
                PERFORM SUPPRIMER THRU FIN-SUPPRIMER

               WHEN "0"
                PERFORM EXIT-PGM THROUGH FIN-EXIT

               WHEN OTHER
                DISPLAY "CHOIX ERRONEE !"
             END-EVALUATE.

      /     END-PERFORM.


           STOP RUN.


      /    *************************MENU********************************
           MENU.
              DISPLAY "**************MENU**************".
              DISPLAY "*       1-AJOUTER CONTACT      *".
              DISPLAY "*       2-CHERCHER CONTACT     *".
              DISPLAY "*       3-SUPPRIMER CONTACT    *".
              DISPLAY "*       0-QUITTER              *".
              DISPLAY "********************************".
           FIN-MENU.
      /    *************************************************************


      /    **********************PRECED AJOUT***************************
           AJOUT-CONTACT.
           DISPLAY "Nom :".
           ACCEPT WS-NOM-TEM.

           DISPLAY "TELEPHONE :".
           ACCEPT WS-TEL-TEM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               IF WS-NOM(WS-I)= SPACES
                   MOVE WS-NOM-TEM TO WS-NOM(WS-I)
                   MOVE WS-TEL-TEM TO WS-TEL(WS-I)

      /           ************TESTE D'AJOUT***************
                        DISPLAY WS-CONTACT(WS-I)
                        DISPLAY WS-I
      /           ****************************************

                   GOBACK
               END-IF
           END-PERFORM.
           FIN-AJOUT.
      /    *************************************************************


      /    **********************PRECED RECHERCHE***********************
           CHERCHER.
           DISPLAY "CONTACT A TROUVER ?"
           ACCEPT WS-NOM-TEM

           IF WS-CONTACT(1)= " "
               DISPLAY " LISTE DES CONTACT VIDE"
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-VERIF ="1"
               IF WS-NOM(WS-I) = WS-NOM-TEM
                   DISPLAY WS-CONTACT(WS-I)
                   MOVE "1" TO WS-VERIF
               END-IF
           END-PERFORM

               IF WS-VERIF = "1"
                   DISPLAY "CONTACT :" WS-CONTACT(WS-I)
               ELSE
                   DISPLAY "CONTACT N'EXISTE PAS"
               END-IF.
           FIN-CHERCHER.
      /    *************************************************************


      /    **********************PRECED SUPPRESSION*********************
           SUPPRIMER.
           DISPLAY "CONTACT A SUPPRIMER ?".
           ACCEPT WS-NOM-TEM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               IF WS-CONTACT(WS-I)= WS-NOM-TEM
                   DISPLAY "CONTACT :"WS-CONTACT(WS-I)
                   EXIT PERFORM
               ELSE
                   DISPLAY "CONTACT N'EXISTE PAS !"
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           FIN-SUPPRIMER.

      /    *************************************************************


      /    **********************PRECED EXIT-PMG************************
           EXIT-PGM.
               EXIT.
           FIN-EXIT.
      /    *************************************************************


       END PROGRAM Annuaire-2.
