       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQUE-CLT.
       AUTHOR.    MOHAMED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.


       01  TABLE-CLIENTS.
         05 CLIENT PIC X(86)  OCCURS 100 TIMES.

       01  REC-CLT.
         05  WS-MAT-CLT     PIC X(6).
         05  WS-NOM-CLT     PIC X(20).
         05  WS-PRENOM-CLT  PIC X(20).
         05  WS-AGE-CLT     PIC X(20).
         05  WS-EMAIL-CLT   PIC X(20).


       01  TABLE-COMPTES.
         05 COMPTE PIC X(45) OCCURS 100 TIMES.

       01  REC-CPT.
         05  WS-NUM-CPT     PIC X(6).
         05  WS-DATE-CPT    PIC X(8).
         05  WS-SOLDE-CPT   PIC 9(10).
         05  WS-TYPE-CPT    PIC X(10).
         05  WS-TITUL-CPT   PIC X(6).
         05  WS-DEVISE-CPT  PIC X(3).

       01  TABLE-INTERMIDIAIRE.
         05 CPT-INT  OCCURS 100 TIMES.
           10  WS-NUM-CPTI     PIC X(6).
           10  WS-DATE-CPTI    PIC X(8).
           10  WS-SOLDE-CPTI   PIC 9(10).
           10  WS-TYPE-CPTI    PIC X(10).
           10  WS-TITUL-CPTI   PIC X(6).
           10  WS-DEVISE-CPTI  PIC X(3).

       01  WS-NBR-CLT     PIC 9(2).
       01  WS-NBR-CPT     PIC 9(2).
       01  WS-I           PIC 9(2).
       01  WS-J           PIC 9(2).
       01  WS-CPT-CLT     PIC X(6).
       01  WS-NBR-CPT-CLT PIC 9(2).
       01  WS-COUNT-CPT   PIC 9(2).

       01  FUNC-SOMME     PIC X(100) VALUES 'SOMME-SOLDE'.
       01  FUNC-DATE     PIC X(100) VALUES  'CONV-DATE'.



       PROCEDURE DIVISION.

           PERFORM CONVERSION-DATE.
           PERFORM SOMME.
           PERFORM REMPLIR-TAB-CPT.
           PERFORM CHERCHER-CPT.

       STOP RUN.


      /    *********************SAISIR-CLIENTT**************************
       SAISIR-CLT.
            DISPLAY "MATRICULE CLIENT :".
            ACCEPT WS-MAT-CLT.

            DISPLAY "NOM CLIENT :".
            ACCEPT WS-NOM-CLT.

            DISPLAY "PRENOM CLIENT :".
            ACCEPT WS-PRENOM-CLT.

            DISPLAY "AGE CLIENT :".
            ACCEPT WS-AGE-CLT.

            DISPLAY "EMAIL CLIENT :".
            ACCEPT WS-EMAIL-CLT.
            EXIT.
       FIN-SAISIR-CLT.
      /    *************************************************************



      /    *********************SAISIR-COMPTE**************************
       SAISIR-CPT.
            DISPLAY "NUM COMPTE :".
            ACCEPT WS-NUM-CPT.

            DISPLAY "DATE CREATION :".
            ACCEPT WS-DATE-CPT.

            DISPLAY "SOLDE :".
            ACCEPT WS-SOLDE-CPT.

            DISPLAY "TYPE COMPTE :".
            ACCEPT WS-TYPE-CPT.

            DISPLAY "TITULAIRE CPT :".
            ACCEPT WS-TITUL-CPT.

            DISPLAY "DEVISE :".
            ACCEPT WS-DEVISE-CPT.
            EXIT.
       FIN-SAISIR-CPT.
      /    *************************************************************



      /    **********************REMPLIR-TAB-CLIENT*********************
       REMPLIR-TAB-CLT.
            DISPLAY "NOMBRE DES CLIENT A SAISIR :"
            ACCEPT WS-NBR-CLT.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NBR-CLT

             PERFORM SAISIR-CLT
             MOVE  REC-CLT TO CLIENT(WS-I)

           END-PERFORM.
           EXIT.
       FIN-REMP-TAB-CLT.
      /    *************************************************************



      /    *********************REMPLIR-TAB-COMPTE**********************
       REMPLIR-TAB-CPT.
            DISPLAY "NOMBRE DES COMPTES A SAISIR :"
            ACCEPT WS-NBR-CPT

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NBR-CPT

             PERFORM SAISIR-CPT
             MOVE  REC-CPT TO COMPTE(WS-J)

           END-PERFORM.
           EXIT.
       FIN-REMP-TAB-CPT.
      /    *************************************************************



      /    *********************NBR-COMPTE-CLIENT***********************
       CHERCHER-CPT.
            DISPLAY "NUM MATRICULE CLIENT"
            ACCEPT WS-CPT-CLT

      *    ici j'ai pas pu acceder aux attribues table compte directement
      *    donc j'ai utiliser un tableau intermédiaire : CPT-INT
      *    j'ai utiliser COMPTE(WS-J).WS-TITUL-CPTI il génere une erreur

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NBR-CPT
                    MOVE COMPTE(WS-J) TO CPT-INT(WS-J)
           END-PERFORM.


           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NBR-CPT

      *    ici j'ai tester que le table intermédiaire est rempli
                  DISPLAY WS-TITUL-CPTI(WS-J)

                  IF WS-TITUL-CPTI(WS-J) = WS-CPT-CLT
                      ADD 1 TO WS-NBR-CPT-CLT
                  END-IF

           END-PERFORM.

      *    Affichage le nombre  et les numeros des comptes d'un client x
               DISPLAY "LE CLIENT :" WS-CPT-CLT" POSSEDE "
                                              WS-NBR-CPT-CLT" COMPTES :"


           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NBR-CPT
                  IF WS-TITUL-CPTI(WS-J) = WS-CPT-CLT
                      ADD 1 TO WS-COUNT-CPT
                      DISPLAY "COMPTE "WS-COUNT-CPT" : "
                                            WS-NUM-CPTI(WS-J)
                  END-IF
           END-PERFORM.
           EXIT.
       FIN-CHERCHER-CPT.
      /    *************************************************************


       /   *********************SOMME-SOLDE-CLIENT**********************
       SOMME.

      *    ici j'ai utiliser le tableau intermediaire CPT-INT pour le
      *    passaer a la fonction.
      *    est ce que je dois utiliser COPY "COPYBOOK" et declarer
      *    le tableau dans un fichier .CPY et faire un passage par reference?

            PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NBR-CPT
                    MOVE COMPTE(WS-J) TO CPT-INT(WS-J)
            END-PERFORM.

            CALL FUNC-SOMME USING TABLE-INTERMIDIAIRE.

            EXIT.

       FIN-SOMME.
      /    *************************************************************


      /    *********************CONV-DATE-CREATION**********************
       CONVERSION-DATE.

            PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NBR-CPT
             MOVE COMPTE(WS-J) TO CPT-INT(WS-J)
            END-PERFORM.

            CALL FUNC-DATE USING  TABLE-INTERMIDIAIRE.

            EXIT.

       FIN-CONVERSION-DATE.
      /    *************************************************************

       END PROGRAM BANQUE-CLT.
