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
            ASSIGN TO "C:\Users\Direnzi\Downloads\Features data set.csv"
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CSV.
       01 LINHA-CSV        PIC X(300).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG         PIC X VALUE "N".

       01 F-STORE          PIC X(20).
       01 F-DATE           PIC X(20).
       01 F-TEMPERATURE    PIC X(20).
       01 F-FUELPRICE      PIC X(20).
       01 F-MD1            PIC X(20).
       01 F-MD2            PIC X(20).
       01 F-MD3            PIC X(20).
       01 F-MD4            PIC X(20).
       01 F-MD5            PIC X(20).
       01 F-CPI            PIC X(20).
       01 F-UNEMP          PIC X(20).
       01 F-ISHOLIDAY      PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "=== LENDO CSV ===".

           OPEN INPUT ARQ-CSV

           PERFORM UNTIL EOF-FLAG = "S"

               READ ARQ-CSV
                   AT END
                       MOVE "S" TO EOF-FLAG
               END-READ

               IF EOF-FLAG NOT = "S"

                   MOVE SPACES TO
                       F-STORE F-DATE F-TEMPERATURE F-FUELPRICE
                       F-MD1 F-MD2 F-MD3 F-MD4 F-MD5
                       F-CPI F-UNEMP F-ISHOLIDAY

                   UNSTRING LINHA-CSV
                       DELIMITED BY ","
                       INTO F-STORE
                            F-DATE
                            F-TEMPERATURE
                            F-FUELPRICE
                            F-MD1
                            F-MD2
                            F-MD3
                            F-MD4
                            F-MD5
                            F-CPI
                            F-UNEMP
                            F-ISHOLIDAY
                   END-UNSTRING

                   DISPLAY "Registro:"
                   DISPLAY "  Store:       " F-STORE
                   DISPLAY "  Date:        " F-DATE
                   DISPLAY "  Temperature: " F-TEMPERATURE
                   DISPLAY "  Fuel Price:  " F-FUELPRICE
                   DISPLAY "  MD1:         " F-MD1
                   DISPLAY "  MD2:         " F-MD2
                   DISPLAY "  MD3:         " F-MD3
                   DISPLAY "  MD4:         " F-MD4
                   DISPLAY "  MD5:         " F-MD5
                   DISPLAY "  CPI:         " F-CPI
                   DISPLAY "  Unemployment:" F-UNEMP
                   DISPLAY "  IsHoliday:   " F-ISHOLIDAY
                   DISPLAY "-------------------------"

               END-IF

           END-PERFORM

           CLOSE ARQ-CSV

           DISPLAY "=== FINALIZADO ===".
           GOBACK.

       END PROGRAM testeCSV.
