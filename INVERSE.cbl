       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVERSE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-INITIAL PIC X(20) VALUE SPACES.
       01  WS-FINAL   PIC X(20) VALUE SPACES.
       01  WS-COUNTER PIC 9(2).
       01  WS-COUNTER-F PIC 9(2).
       01  WS-LENGTH-STR PIC 9(2).

       PROCEDURE DIVISION.

       DISPLAY "DONNER UNE CHAINE DE CARACTERE".
       ACCEPT WS-INITIAL.

       MOVE FUNCTION LENGTH(WS-INITIAL) TO WS-LENGTH-STR.
       MOVE 1 TO WS-COUNTER-F.

       PERFORM VARYING WS-COUNTER FROM WS-LENGTH-STR BY -1
                   UNTIL WS-COUNTER < 1

               MOVE WS-INITIAL(WS-COUNTER:1) TO WS-FINAL(WS-COUNTER-F:1)
                  ADD 1 TO WS-COUNTER-F

       END-PERFORM


               DISPLAY WS-FINAL.
       STOP RUN.

       END PROGRAM INVERSE.
