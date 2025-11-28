      ******************************************************************
      * Author: Guilherme Alves Direnzi
      * Author: Arthur Selingin
      * Author: Rai Felipe
      * Date: 22/11/2025
      * Purpose: Ler dados externos em COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. testeCSV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CSV
               ASSIGN TO "C:\data\sales_data_t.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT ARQ-CSV-FEATURES
               ASSIGN TO "C:\data\features_data.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQ-REL
               ASSIGN TO "C:\data\relatorio_vendas.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CSV.
       01 LINHA-CSV        PIC X(300).

       FD ARQ-CSV-FEATURES.
       01 LINHA-CSV-FEATURES        PIC X(300).

       FD ARQ-REL.
       01 LINHA-REL        PIC X(300).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG         PIC X VALUE "N".

       01 F-STORE          PIC X(20).
       01 F-DEPT           PIC X(20).
       01 F-DATE           PIC X(20).
       01 F-W_SALES        PIC X(20).
       01 F-ISHOLIDAY      PIC X(20).

       01 TOTAL-VENDAS     PIC 9(10)V99 VALUE 0.
       01 TOTAL-FMT        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01 TOTAL-BR         PIC X(40).
       01 POS-ULTIMO       PIC 9(4) COMP.
       01 I                PIC 9(4) COMP.

       01 CONT-REGISTROS    PIC 9(10) VALUE 0.
       01 MEDIA-FMT         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01 MEDIA-BR          PIC X(40).
       01 MEDIA-VENDAS     PIC 9(10)V99 VALUE 0.
 


       



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "=== LENDO CSV ===".

           OPEN INPUT ARQ-CSV
           OPEN INPUT ARQ-CSV-FEATURES
           OPEN OUTPUT ARQ-REL

      *    *> Primeira linha (cabe�alho)
           READ ARQ-CSV
           READ ARQ-CSV-FEATURES



           MOVE "RELATORIO DE VENDAS (05/02/2010 - 26/02/2010)"
               TO LINHA-REL
           WRITE LINHA-REL

           MOVE "---------------------------------------------"
               TO LINHA-REL
           WRITE LINHA-REL

           MOVE SPACES TO LINHA-REL

      ****************************************************************
           *> PROCESSA CSV
      ****************************************************************
           PERFORM UNTIL EOF-FLAG = "S"

               READ ARQ-CSV
                   AT END MOVE "S" TO EOF-FLAG
               END-READ

               IF EOF-FLAG NOT = "S"

                   MOVE SPACES TO
                       F-STORE
                       F-DEPT
                       F-DATE
                       F-W_SALES
                       F-ISHOLIDAY

                   UNSTRING LINHA-CSV
                       DELIMITED BY ","
                       INTO F-STORE
                            F-DEPT
                            F-DATE
                            F-W_SALES
                            F-ISHOLIDAY
                   END-UNSTRING

                   IF F-W_SALES NOT = SPACES
                       COMPUTE TOTAL-VENDAS =
                           TOTAL-VENDAS + FUNCTION NUMVAL(F-W_SALES)
                   END-IF

                   STRING
                       F-STORE      DELIMITED BY SIZE
                       " "          DELIMITED BY SIZE
                       F-DEPT       DELIMITED BY SIZE
                       " "          DELIMITED BY SIZE
                       F-DATE       DELIMITED BY SIZE
                       " "          DELIMITED BY SIZE
                       F-W_SALES    DELIMITED BY SIZE
                       " "          DELIMITED BY SIZE
                       F-ISHOLIDAY  DELIMITED BY SIZE
                       INTO LINHA-REL
                   END-STRING

                   WRITE LINHA-REL


                   IF F-W_SALES NOT = SPACES
                       COMPUTE TOTAL-VENDAS =
                           TOTAL-VENDAS + FUNCTION NUMVAL(F-W_SALES)
                       ADD 1 TO CONT-REGISTROS
                   END-IF


               END-IF
                   
           END-PERFORM

           MOVE "---------------------------------------------"
               TO LINHA-REL
           WRITE LINHA-REL


           MOVE TOTAL-VENDAS TO TOTAL-FMT

      *  Passando para formato br
           INSPECT TOTAL-FMT REPLACING ALL "," BY ".".

           MOVE 0 TO POS-ULTIMO.
           PERFORM VARYING I FROM LENGTH OF TOTAL-FMT BY -1 UNTIL I = 1
               IF TOTAL-FMT(I:1) = "."
                MOVE I TO POS-ULTIMO
                EXIT PERFORM
               END-IF
           END-PERFORM.

           IF POS-ULTIMO > 0
               MOVE "," TO TOTAL-FMT(POS-ULTIMO:1)
           END-IF.

           MOVE TOTAL-FMT TO TOTAL-BR.
      * Fim formata��o

           DISPLAY TOTAL-BR

           MOVE "TOTAL DE VENDAS: " TO LINHA-REL
           STRING
               "TOTAL DE VENDAS: R$ "
               TOTAL-BR
               DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING
           WRITE LINHA-REL


              *> Calcula m��dia de vendas
           IF CONT-REGISTROS > 0
               COMPUTE MEDIA-VENDAS = TOTAL-VENDAS / CONT-REGISTROS
           END-IF.

           MOVE MEDIA-VENDAS TO MEDIA-FMT

           INSPECT MEDIA-FMT REPLACING ALL "," BY ".".

           MOVE 0 TO POS-ULTIMO.
           PERFORM VARYING I FROM LENGTH OF MEDIA-FMT BY -1 UNTIL I = 1
               IF MEDIA-FMT(I:1) = "."
                   MOVE I TO POS-ULTIMO
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF POS-ULTIMO > 0
               MOVE "," TO MEDIA-FMT(POS-ULTIMO:1)
           END-IF

           MOVE MEDIA-FMT TO MEDIA-BR.

           MOVE "MEDIA DE VENDA SEMANAIS: " TO LINHA-REL
           STRING
               "MEDIA DE VENDAS SEMANAIS: R$ "
               MEDIA-BR
               DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING
           WRITE LINHA-REL
           


           MOVE "Programa: testeCSV" TO LINHA-REL
           WRITE LINHA-REL

           CLOSE ARQ-CSV
           CLOSE ARQ-CSV-FEATURES
           CLOSE ARQ-REL

           DISPLAY "Relatorio gerado com sucesso!"
           GOBACK.

       END PROGRAM testeCSV.
