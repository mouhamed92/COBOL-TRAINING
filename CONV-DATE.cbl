       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONV-DATE.
       AUTHOR. MOHAMED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-YEAR PIC 9(4).
       01  WS-MONTH PIC X(9).
       01  WS-DAY PIC 9(2).
       01  WS-MAT-CLT PIC X(6).
       01  WS-I PIC 9(1).

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

        DISPLAY "DONNER LE CLIENT A CHERCHER ".
              ACCEPT WS-MAT-CLT.

              PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
                IF WS-TITUL-CPTI(WS-I)= WS-MAT-CLT
                    MOVE WS-DATE-CPTI(WS-I)(1:4) TO WS-YEAR
                    MOVE WS-DATE-CPTI(WS-I)(5:2) TO WS-MONTH
                    MOVE WS-DATE-CPTI(WS-I)(7:2) TO WS-DAY
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

       END PROGRAM CONV-DATE.
