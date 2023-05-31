       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-HANDLING.
       AUTHOR.    MOHAMED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Employee ASSIGN TO
            "C:\work space\Cobol path\labs\youTube labs\Employees.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Employee.
           01 Employee-FILE.
             05 Employee-ID PIC 9(5).
             05 NAME PIC A(25).
             05 DATE-NES PIC X(10).
             05 SALAIRE  PIC X(7).
             05 FONCTION PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-Employee.
         05 WS-Employee-ID PIC 9(5).
         05 WS-NAME PIC A(25).
         05 WS-DATE-NES PIC X(10).
         05 WS-SALAIRE  PIC X(7).
         05 WS-FONCTION PIC X(20).
       01 WS-EOF PIC A(1).

       PROCEDURE DIVISION.

            OPEN INPUT Employee.
               PERFORM UNTIL WS-EOF='Y'
                READ Employee INTO WS-Employee
                  AT END MOVE 'Y' TO WS-EOF
                      NOT AT END DISPLAY WS-Employee
                  END-READ
               END-PERFORM.
               CLOSE Employee.
            STOP RUN.

       END PROGRAM FILE-HANDLING.
