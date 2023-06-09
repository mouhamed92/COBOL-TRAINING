       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-FILE.
       AUTHOR.    MOHAMED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

               SELECT Employee ASSIGN TO
             "C:\work space\Cobol path\labs\youTube labs\Employees.txt"
                  ORGANIZATION IS LINE  SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Employee.
           01 Employee-FILE.
             05 Employee-ID  PIC  9(5).
             05 FILLER       PIC  X(1).
             05 Name         PIC  X(25).
             05 FILLER       PIC  X(1).
             05 Date-nes     PIC  X(10).
             05 FILLER       PIC  X(1).
             05 FILLER       PIC  X(1).
             05 Salaire      PIC  X(7).
             05 FILLER       PIC  X(1).
             05 Fonction     PIC  X(20).


       WORKING-STORAGE SECTION.

           01 WS-Employee.
             05 WS-Employee-ID  PIC  9(5).
             05 FILLER       PIC  X(1).
             05 WS-Name         PIC  X(25).
             05 FILLER       PIC  X(1).
             05 WS-Date-nes     PIC  X(10).
             05 FILLER       PIC  X(1).
             05 WS-Salaire      PIC  X(7).
             05 FILLER       PIC  X(1).
             05 WS-Fonction     PIC  X(20).

           01 WS-EOF            PIC  A(1).

       PROCEDURE DIVISION.

           OPEN INPUT Employee.
              PERFORM UNTIL WS-EOF = 'Y'
                  READ Employee INTO WS-Employee
                  AT END
                     MOVE 'Y' TO WS-EOF
                  NOT AT END
                     DISPLAY WS-Employee
                  END-READ
              END-PERFORM
              CLOSE Employee.


              Display "DONNER EM-ID"
              Accept   Employee-ID
              Display "NOM"
              Accept   Name
              Display "DATE NESSANCE"
              Accept   Date-nes
              Display "SALAIRE"
              Accept   Salaire
              Display "FONCTION"
              ACCEPT  Fonction

           OPEN EXTEND Employee.
                 MOVE Employee-ID TO EMPLOYEE-ID.
                 MOVE Date-nes TO Date-nes.
                 MOVE Salaire TO Salaire.
                 MOVE Fonction TO Fonction.
                 WRITE EMPLOYEE-FILE
                 END-WRITE.
           CLOSE EMPLOYEE.

              STOP RUN.

       END PROGRAM EMP-FILE.
