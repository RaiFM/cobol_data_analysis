      ******************************************************************
      * Author: Guilherme Alves Direnzi
      * Date: 22/11/2025
      * Purpose: Ler dados externos em COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. testeTXT.



       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-ENTRADA
           ASSIGN TO "C:\Users\Direnzi\Downloads\Features data set.csv"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD ARQ-ENTRADA.
       01 REG-LINHA    PIC X(500).


       WORKING-STORAGE SECTION.
       01 EOF-FLAG     PIC X VALUE "N".


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


            DISPLAY "=== INICIANDO LEITURA DE ARQUIVO ===".
            OPEN INPUT ARQ-ENTRADA


           PERFORM UNTIL EOF-FLAG = "S"
           READ ARQ-ENTRADA
           AT END
           MOVE "S" TO EOF-FLAG
           NOT AT END
           DISPLAY "LINHA: " REG-LINHA
           END-READ
           END-PERFORM


           CLOSE ARQ-ENTRADA


           DISPLAY "=== leitura finalizada ===".


            GOBACK.

       END PROGRAM teste.
