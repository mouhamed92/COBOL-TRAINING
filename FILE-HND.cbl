 !       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-HND.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *assign physique file to loqique file
            SELECT ETUDIANT ASSIGN TO
            "C:\work space\Cobol path\labs\youTube labs\ETUDIANTS.txt"
            ORGANIZATION IS LINE SEQUENTIAL
            FILE STATUS IS ET-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.
      *declaration of file and record content
       FD ETUDIANT.
       01  ETUDIANT-RECORD.
         05 ETD-ID    PIC   X(10).
         05 FILLER    PIC   X(1).
         05 ETD-NAME  PIC   X(10).
         05 FILLER    PIC   X(1).
         05 EMP-EXP   PIC   X(1).

       WORKING-STORAGE SECTION.
      *
       01  ET-FILESTATUS   PIC X(2).
       01  ETD-REQUEST     PIC 9(1) VALUE ZERO.
       01  ETD-READ-STATUS PIC X(1).
           88 END-OF-FILE  VALUE 'Y'.
           88 NOT-END-OF-FILE  VALUE 'N'.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       OPEN INPUT ETUDIANT
             EVALUATE TRUE
              WHEN ET-FILESTATUS  ="35"
                 DISPLAY "FILE NOT FOUND"
                 DISPLAY "FILE STATUS IS :"ET-FILESTATUS

              WHEN ET-FILESTATUS = "00"
                PERFORM  UNTIL END-OF-FILE
                     READ ETUDIANT INTO ETUDIANT-RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                     DISPLAY ETUDIANT-RECORD
                     ADD 1 TO ETD-REQUEST
                     ON SIZE ERROR
                     DISPLAY "SIZE ERROR AT PAS-A"

                     END-ADD
                        IF ETD-REQUEST = 0 THEN
                            DISPLAY "NO REQUEST FOR THE DAY"
                        ELSE
                             DISPLAY "NUMBER OF REQUEST :"ETD-REQUEST
                        END-IF
                       CLOSE ETUDIANT
                    END-READ
                   END-PERFORM

                 WHEN OTHER
                  DISPLAY "ERROR FOUND..!"
                  DISPLAY " FILE STATUS IS : "ET-FILESTATUS
              END-EVALUATE.




            STOP RUN.
       END PROGRAM FILE-HND.
