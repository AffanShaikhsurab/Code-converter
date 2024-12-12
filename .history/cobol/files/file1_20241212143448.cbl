       IDENTIFICATION DIVISION.
       PROGRAM-ID. File2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VAR3 PIC 9(4).
       
       LINKAGE SECTION.
       01 FILE1-VAR1 PIC 9(4).

       PROCEDURE DIVISION USING FILE1-VAR1.
       MAIN-LOGIC.
           DISPLAY "File2: Received VAR1 = ", FILE1-VAR1.
           COMPUTE VAR3 = FILE1-VAR1 * 2.
           DISPLAY "File2: VAR3 after computation = ", VAR3.
           STOP RUN.
