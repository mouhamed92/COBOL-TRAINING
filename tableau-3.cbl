       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEAU-3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  TABLEAU.
           05 WS-TAB PIC 9(02) OCCURS 10 TIMES.
       01  WS-NUMBER PIC 9(2).
       01  WS-I      PIC 9(02).

       PROCEDURE DIVISION.

           DISPLAY "ENTRER 10 NOMBRE"

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10

               ACCEPT WS-NUMBER
               MOVE WS-NUMBER TO WS-TAB(WS-I)

           END-PERFORM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10

               DISPLAY "L'ELEMENT "WS-I" EST = " WS-TAB(WS-I)

           END-PERFORM.

       STOP RUN.

       END PROGRAM TABLEAU-3.
