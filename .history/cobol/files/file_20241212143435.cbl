       IDENTIFICATION DIVISION.
       PROGRAM-ID. File1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VAR1 PIC 9(4) VALUE 1000.
       01 VAR2 PIC 9(4) VALUE 2000.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "File1: Start of program".
           PERFORM ADD-VARS.
           STOP RUN.

       ADD-VARS.
           COMPUTE VAR1 = VAR1 + VAR2.
           DISPLAY "File1: VAR1 after addition = ", VAR1.
