       IDENTIFICATION DIVISION.
       PROGRAM-ID. Annuaire.

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


       PROCEDURE DIVISION.


           PERFORM MENU THRU FIN-MENU

            ACCEPT WS-CHOIX.

            EVALUATE WS-CHOIX
               WHEN  "1"
                PERFORM AJOUT-CONTACT THRU FIN-AJOUT

               WHEN "0"
                PERFORM EXIT-PGM THROUGH FIN-EXIT

               WHEN OTHER
                DISPLAY "CHOIX ERRONEE !"
             END-EVALUATE

           STOP RUN.

           MENU.
              DISPLAY "**************MENU**************".
              DISPLAY "*       1-AJOUTER CONTACT      *".
              DISPLAY "*       2-CHERCHER CONTACT     *".
              DISPLAY "*       3-SUPPRIMER CONTACT    *".
              DISPLAY "*       0-QUITTER              *".
              DISPLAY "********************************".
           FIN-MENU.


           AJOUT-CONTACT.
           DISPLAY "Nom :".
           ACCEPT WS-NOM-TEM.

           DISPLAY "TELEPHONE :".
           ACCEPT WS-TEL-TEM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               IF WS-NOM(WS-I)= SPACES
                   MOVE WS-NOM-TEM TO WS-NOM(WS-I)
                   MOVE WS-TEL-TEM TO WS-TEL(WS-I)

      /    ************TESTE D'AJOUT***************
                   DISPLAY WS-CONTACT(WS-I)
      /    ****************************************
                   GOBACK
               END-IF
           END-PERFORM.
           FIN-AJOUT.

           EXIT-PGM.
               EXIT.
           FIN-EXIT.

       END PROGRAM Annuaire.
