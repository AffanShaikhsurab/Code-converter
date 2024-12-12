       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO 'file1.dat'.
           SELECT FILE2 ASSIGN TO 'file2.dat'.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01  FILE1-RECORD.
           05 NUMBER     PIC 9(4).
       
       FD  FILE2.
       01  FILE2-RECORD.
           05 TOTAL      PIC 9(6).
       
       WORKING-STORAGE SECTION.
       01 WS-END-OF-FILE        PIC X VALUE 'N'.
       01 WS-TOTAL              PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       OPEN INPUT FILE1
       OPEN OUTPUT FILE2
       PERFORM READ-FILE1
       CLOSE FILE1
       CLOSE FILE2
       STOP RUN.

       READ-FILE1.
           READ FILE1 INTO FILE1-RECORD
               AT END
                   MOVE 'Y' TO WS-END-OF-FILE
               NOT AT END
                   ADD NUMBER TO WS-TOTAL
                   DISPLAY 'Adding: ' NUMBER
                   DISPLAY 'Total So Far: ' WS-TOTAL
                   MOVE WS-TOTAL TO TOTAL
                   WRITE FILE2-RECORD
                   PERFORM READ-FILE1
           END-READ.
