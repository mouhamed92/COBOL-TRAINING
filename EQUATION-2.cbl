       IDENTIFICATION DIVISION.
       PROGRAM-ID. EQUATION-2.
       AUTHOR.    MOHAMED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-A PIC 9(2)     VALUE ZEROS.
       01  WS-B PIC 9(2)     VALUE ZEROS.
       01  WS-C PIC 9(2)     VALUE ZEROS.
       01  WS-DELTA PIC 9(2) VALUE ZEROS.
       01  WS-X1 PIC 9(2)    VALUE ZEROS.
       01  WS-X2 PIC 9(2)    VALUE ZEROS.

       PROCEDURE DIVISION.

           DISPLAY "DONNER UN ENTIER A".
           ACCEPT WS-A.

           DISPLAY "DONNER UN ENTIER B".
           ACCEPT WS-B.

           DISPLAY "DONNER UN ENTIER C".
           ACCEPT WS-C.


       COMPUTE WS-DELTA = (WS-B**2 - 4*(WS-A*WS-C)).

           IF WS-DELTA > 0
               COMPUTE WS-X1 = (- WS-B + FUNCTION SQRT(WS-DELTA))
                                                             /2*WS-A
               COMPUTE WS-X2 = (- WS-B - FUNCTION SQRT(WS-DELTA))
                                                            /2*WS-A

               DISPLAY "X1 = "WS-X1
               DISPLAY "X2 = "WS-X2

           ELSE
               IF WS-DELTA = 0
               COMPUTE WS-X1 = (- WS-B )/(2 * WS-A)
               DISPLAY "x1 = "WS-X1

           ELSE
               DISPLAY "PAS DE SOLUTION REEL"
           END-IF

           STOP RUN.



       END PROGRAM EQUATION-2.
