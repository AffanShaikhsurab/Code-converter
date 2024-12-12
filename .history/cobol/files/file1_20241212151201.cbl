       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO 'file1.dat'.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01  FILE1-RECORD.
           05 NUMBER     PIC 9(4).
       
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE        PIC X VALUE 'N'.
       01 WS-TOTAL              PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       OPEN INPUT FILE1
       PERFORM READ-NUMBERS
       CLOSE FILE1
       STOP RUN.

       READ-NUMBERS.
           READ FILE1 INTO FILE1-RECORD
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE
               NOT AT END
                   ADD NUMBER TO WS-TOTAL
                   DISPLAY 'Current Total: ' WS-TOTAL
                   PERFORM READ-NUMBERS
           END-READ.
