      ******************************************************************
      * Author: Guilherme Alves Direnzi
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
               ASSIGN TO "C:\Users\raife\Downloads\sales_data_t.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQ-REL
               ASSIGN TO "C:\Users\raife\Downloads\relatorio_vendas.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CSV.
       01 LINHA-CSV        PIC X(300).

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

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "=== LENDO CSV ===".

           OPEN INPUT ARQ-CSV
           OPEN OUTPUT ARQ-REL

           MOVE "RELATÓRIO DETALHADO DAS VENDAS DE CADA SEMANA"
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

               END-IF

           END-PERFORM

           MOVE "---------------------------------------------"
               TO LINHA-REL
           WRITE LINHA-REL

           MOVE "TOTAL DE VENDAS: " TO LINHA-REL
           STRING
               "TOTAL DE VENDAS: "
               FUNCTION NUMVAL-C (TOTAL-VENDAS)
               DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING
           WRITE LINHA-REL

           MOVE "Programa: testeCSV" TO LINHA-REL
           WRITE LINHA-REL

           CLOSE ARQ-CSV
           CLOSE ARQ-REL

           DISPLAY "Relatório gerado com sucesso!"
           GOBACK.

       END PROGRAM testeCSV.
