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
       01 TOTAL-FMT        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01 TOTAL-BR         PIC X(40).
       01 POS-ULTIMO       PIC 9(4) COMP.
       01 I                PIC 9(4) COMP.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "=== LENDO CSV ===".

           OPEN INPUT ARQ-CSV
           OPEN OUTPUT ARQ-REL

      *    *> Primeira linha (cabeçalho)
           READ ARQ-CSV



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
      * Fim formatação

           DISPLAY TOTAL-BR

           MOVE "TOTAL DE VENDAS: " TO LINHA-REL
           STRING
               "TOTAL DE VENDAS: R$ "
               TOTAL-BR
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
