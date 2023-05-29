       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOMME-SLD.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-I  PIC 9(2).
       01  WS-NUM-CLT   PIC X(6).

       *> LINKAGE SECTION.

       *> 01  WS-SOMME-SLD PIC 9(3).

       *> 01  TABLE-COMPTE.
         *> 05 COMPTE  OCCURS 3 TIMES.
           *> 10  WS-NUM-CPT     PIC X(6).
           *> 10  WS-DATE-CPT    PIC X(8).
           *> 10  WS-SOLDE-CPT   PIC 9(10).
           *> 10  WS-TYPE-CPT    PIC X(10).
           *> 10  WS-TITUL-CPT   PIC X(6).
           *> 10  WS-DEVISE-CPT  PIC X(3).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           DISPLAY "DONNER LE NUM DU CLIENT :"
           *> ACCEPT WS-NUM-CLT

           *> PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >3
               *> IF WS-TITUL-CPT(WS-I) = WS-NUM-CLT
                   *> ADD WS-SOLDE-CPT(WS-I) TO WS-SOMME-SLD
               *> END-IF
           *> END-PERFORM

            *> DISPLAY "LE SOLDE DE :"WS-NUM-CLT" EST "WS-SOMME-SLD

            EXIT.

       END PROGRAM SOMME-SLD.
