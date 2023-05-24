      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TABLEAU-CPY.


       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.

      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

      *-----------------------
       WORKING-STORAGE SECTION.
       01  TABLE-INTERMIDIAIRE.
         05 CPT-INT  OCCURS 100 TIMES.
           10  WS-NUM-CPTI     PIC X(6).
           10  WS-DATE-CPTI    PIC X(8).
           10  WS-SOLDE-CPTI   PIC 9(10).
           10  WS-TYPE-CPTI    PIC X(10).
           10  WS-TITUL-CPTI   PIC X(6).
           10  WS-DEVISE-CPTI  PIC X(3).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program

       END PROGRAM TABLEAU-CPY.
