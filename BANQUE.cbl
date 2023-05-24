       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQUE.
       AUTHOR.    MOHAMED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-TABLE-CLIENT.
         05 WS-CLIENT  OCCURS 3 TIMES.
           10 WS-MATRICULE PIC X(8).
           10 WS-NOM PIC X(10).
           10 WS-PRENOM PIC X(10).
           10 WS-AGE PIC 9(2).
           10 WS-MAIL PIC X(30).

       01  WS-TABLE-COMPTE.
         05 WS-COMPTE  OCCURS 3 TIMES.
           10 WS-NUMCPTE PIC X(8).
           10 WS-DATE PIC X(8).
           10 WS-SOLDE PIC 9(10).
           10 WS-TITULAIRE PIC X(20).
           10 WS-DEVISE PIC X(10).

       01  WS-TABLE-CLT-CPT.
         05 WS-CPT-CLT  OCCURS 3 TIMES.
           10 WS-NUMCPTE-CLT PIC X(8).
           10 WS-SOLDE-CLT PIC 9(10).
           10 WS-DATE-CLT PIC X(8).


       01  WS-I PIC 9(1).
       01  WS-MAT-CLT PIC X(8).
       01  WS-SOMME PIC 9(10).
       01  WS-YEAR PIC 9(4).
       01  WS-MONTH PIC X(9).
       01  WS-DAY PIC 9(2).
       01  WS-COUNTER PIC 9(1) VALUES 1 .

       01  WS-MONTANT  PIC 9(5).
       01  WS-MONTANT-CONVERTI  PIC 9(7).
       01  WS-DEVISEC  PIC X(3).
       01  WS-CALLED-PMG PIC X(20) VALUES 'CONVERSION-DEVISE'.

       PROCEDURE DIVISION.

           CALL WS-CALLED-PMG.

           DISPLAY "SAISIRE LES CLIENTS".
           PERFORM SAISIE-CLIENT THRU FIN-SAISIE-CLIENT

           DISPLAY "SAISIR LES COMPTE".
           PERFORM SAISIE-COMPTE THRU SAISIE-COMPTE

           DISPLAY "SOMME DES SOLDE"
           PERFORM CALCULE-SOMME THRU FIN-CALCULE-SOMME

           DISPLAY "LISTE DES COMPTE CLIENT"
           PERFORM AFFICHE-COMPTE THRU FIN-AFFICHE

           DISPLAY "COVERTIR DATE"
           PERFORM CONV-DATE THRU FIN-CENV-DATE



       STOP RUN.

           SAISIE-CLIENT.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
            DISPLAY "DONNER LE MATRICULE"
            ACCEPT WS-MATRICULE(WS-I)

            DISPLAY "DONNER LE NOM"
            ACCEPT WS-NOM(WS-I)

            DISPLAY "DONNER LE PRENOM"
            ACCEPT WS-PRENOM(WS-I)

            DISPLAY "DONNER L'AGE"
            ACCEPT WS-AGE(WS-I)

            DISPLAY "DONNER L'E-MAIL"
            ACCEPT WS-MAIL(WS-I)
           END-PERFORM.
           FIN-SAISIE-CLIENT.
           EXIT.

           SAISIE-COMPTE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
            DISPLAY "DONNER NUMCPT"
            ACCEPT WS-NUMCPTE(WS-I)

            DISPLAY "DONNER DATE DE CREATION"
            ACCEPT WS-DATE(WS-I)

            DISPLAY "DONNER LE SOLDE"
            ACCEPT WS-SOLDE(WS-I)

            DISPLAY "DONNER TITULAIRE"
            ACCEPT WS-TITULAIRE(WS-I)

            DISPLAY "DONNER DEVISE"
            ACCEPT WS-DEVISE(WS-I)
           END-PERFORM.
           FIN-SAISIE-COMPTE.
           EXIT.

           CALCULE-SOMME.
           DISPLAY "DONNER LE CLIENT A CHERCHER ".
           ACCEPT WS-MAT-CLT.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               IF WS-TITULAIRE(WS-I) = WS-MAT-CLT
                   ADD WS-SOLDE(WS-I) TO WS-SOMME
           END-PERFORM.
               DISPLAY "LA SOMME DES SOLDES EST  " WS-SOMME.
           FIN-CALCULE-SOMME.
           EXIT.

           CONV-DATE.

              DISPLAY "DONNER LE CLIENT A CHERCHER ".
              ACCEPT WS-MAT-CLT.

              PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
                IF WS-TITULAIRE(WS-I)= WS-MAT-CLT
                    MOVE WS-DATE(WS-I)(1:4) TO WS-YEAR
                    MOVE WS-DATE(WS-I)(5:2) TO WS-MONTH
                    MOVE WS-DATE(WS-I)(7:2) TO WS-DAY
                END-IF
                               EVALUATE WS-MONTH
                WHEN '01'
                   MOVE 'JANVIER' TO WS-MONTH
                WHEN '02'
                   MOVE 'FEVRIER' TO WS-MONTH
                WHEN '03'
                   MOVE 'MARS' TO WS-MONTH
                WHEN '04'
                   MOVE 'AVRIL' TO WS-MONTH
                WHEN '05'
                   MOVE 'MAI' TO WS-MONTH
                WHEN '06'
                   MOVE 'JUIN' TO WS-MONTH
                WHEN '07'
                   MOVE 'JUILLET' TO WS-MONTH
                WHEN '08'
                   MOVE 'AOUT' TO WS-MONTH
                WHEN '09'
                   MOVE 'SEPTEMBRE' TO WS-MONTH
                WHEN '02'
                   MOVE 'OCTOBRE' TO WS-MONTH
                WHEN '03'
                   MOVE 'NOVOMBRE' TO WS-MONTH
                WHEN '04'
                   MOVE 'DECEMBRE' TO WS-MONTH
               END-EVALUATE

               DISPLAY WS-YEAR"/"WS-MONTH"/"WS-DAY

              END-PERFORM.
           FIN-CENV-DATE.
           EXIT.

           AFFICHE-COMPTE.

            DISPLAY "ENTRER LE MATRICULE D'UN CLIENT"
            ACCEPT WS-MAT-CLT

            PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
                IF WS-MATRICULE(WS-I) = WS-MAT-CLT
                   MOVE WS-NUMCPTE(WS-I) TO WS-NUMCPTE-CLT( WS-COUNTER)
                   MOVE WS-SOLDE(WS-I) TO WS-SOLDE-CLT( WS-COUNTER)
                   MOVE WS-DATE(WS-I) TO WS-DATE-CLT( WS-COUNTER)
                   ADD 1 TO WS-COUNTER
                END-IF
            END-PERFORM.

             PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNTER
                 DISPLAY "COMPTE "WS-I" :"WS-NUMCPTE-CLT(WS-I)","
                           WS-SOLDE-CLT( WS-I)","WS-DATE-CLT( WS-I)
             END-PERFORM.
           FIN-AFFICHE.
           EXIT.

       END PROGRAM BANQUE.
