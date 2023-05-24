       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOMME-SOLDE.
       AUTHOR. MOHAMED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-I           PIC 9(2).
       01  WS-CPT-CLT     PIC X(6).
       01  WS-SM-SOLDE    PIC 9(10).

       LINKAGE SECTION.

       01  TABLE-INTERMIDIAIRE.
         05 CPT-INT  OCCURS 100 TIMES.
           10  WS-NUM-CPTI     PIC X(6).
           10  WS-DATE-CPTI    PIC X(8).
           10  WS-SOLDE-CPTI   PIC 9(10).
           10  WS-TYPE-CPTI    PIC X(10).
           10  WS-TITUL-CPTI   PIC X(6).
           10  WS-DEVISE-CPTI  PIC X(3).


       PROCEDURE DIVISION.

           DISPLAY " CALCUL DE SOMME DES SOLDE DU CLIENT :"
           ACCEPT WS-CPT-CLT.


           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5

               IF WS-CPT-CLT = WS-TITUL-CPTI(WS-I)
                   ADD WS-SOLDE-CPTI(WS-I) TO WS-SM-SOLDE
               END-IF

           END-PERFORM

                 DISPLAY "LA SOMME DES SOLDE DU CLIENT: "WS-CPT-CLT
                                               " EST "WS-SM-SOLDE

           EXIT.

       END PROGRAM SOMME-SOLDE.
