       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPERATIONS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-OPE         PIC  X(4).
       01  WS-MONTANT     PIC 9(3).
       01  WS-SOLDE    PIC 9(3).
       01  WS-COMPTE-C1    PIC 9(3).
       01  WS-COMPTE-C2    PIC 9(3).
       01  WS-I  PIC 9(2).
       01  WS-J  PIC 9(2).
       01  WS-MONT-TEMP   PIC 9(10).
       01  WS-NUM-COMPTE  PIC X(6).

       01  TABLE-INTERMIDIAIRE.
         05 CPT-INT  OCCURS 100 TIMES.
           10  WS-NUM-CPTI     PIC X(6).
           10  WS-DATE-CPTI    PIC X(8).
           10  WS-SOLDE-CPTI   PIC 9(10).
           10  WS-TYPE-CPTI    PIC X(10).
           10  WS-TITUL-CPTI   PIC X(6).
           10  WS-DEVISE-CPTI  PIC X(3).

       PROCEDURE DIVISION.

            DISPLAY "DONNER CODE OPERATION "
            DISPLAY "OPE1:Retrait"
            DISPLAY "OPE2:Versement"
            DISPLAY "OPE3:Virement"
            DISPLAY "OPE4:Consultation Solde"

            ACCEPT WS-OPE

              EVALUATE WS-OPE

                WHEN "OPE1"
                   PERFORM Retrait

                WHEN "OPE2"
                   PERFORM  Versement

                WHEN "OPE3"
                   PERFORM  Virement

                WHEN "OPE4"
                   PERFORM  Consultation-Solde

                WHEN OTHER
                    DISPLAY "CHOIX ERRONE !"

             END-EVALUATE.

           STOP RUN.

           Retrait.
             DISPLAY "MONTANT DE RETRAIT:".
             ACCEPT WS-MONTANT.

             IF WS-SOLDE < WS-MONTANT
               DISPLAY "SOLDE INSUFFISAsNT"
             ELSE
               SUBTRACT WS-MONTANT FROM WS-SOLDE
             END-IF.
            EXIT.
           FIN-Retrait.

           Versement.
             DISPLAY "MONTANT DE VERSEMENT:".
             ACCEPT WS-MONTANT.
             ADD WS-MONTANT TO WS-SOLDE.
             EXIT.
           FIN-Versement.

           Virement.
                DISPLAY "DONNER LE COMPTE C1 "
                ACCEPT WS-COMPTE-C1.

                DISPLAY "MONTANT A VIRER:".
                ACCEPT WS-MONTANT.

             PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5

               IF WS-COMPTE-C1 = WS-TITUL-CPTI(WS-I)

                    IF WS-SOLDE-CPTI(WS-I) < WS-MONTANT
                       DISPLAY "SOLDE INSUFFISANT"
                  ELSE
                     SUBTRACT WS-MONTANT FROM WS-SOLDE-CPTI(WS-I)
                                                  GIVING WS-MONT-TEMP
                  END-IF
               ELSE
                   DISPLAY "VERIFIER NUM COMPTE !"
               END-IF

             END-PERFORM

                DISPLAY "DONNER LE COMPTE C2 "
                ACCEPT WS-COMPTE-C2.

             PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-I > 5

               IF WS-COMPTE-C2 = WS-TITUL-CPTI(WS-J)

                   ADD WS-MONT-TEMP TO WS-SOLDE-CPTI(WS-J)

               ELSE
                   DISPLAY "VERIFIER NUM COMPTE !"
               END-IF

             END-PERFORM
             EXIT.
           FIN-Virement.

           Consultation-Solde.

                DISPLAY " DONNER UN NUM DE COMPTE A CONSULTER :"
                ACCEPT WS-NUM-COMPTE

                PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5

                   IF WS-NUM-COMPTE = WS-NUM-CPTI(WS-I)
                       DISPLAY " LES SOLDE EST: " WS-SOLDE-CPTI(WS-I)
                   ELSE
                       DISPLAY "VERIFIER NUM COMPTE !"
                   END-IF
               END-PERFORM
               EXIT.

           FIN-Consultation-Solde.

       END PROGRAM OPERATIONS.
