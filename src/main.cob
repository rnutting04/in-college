      IDENTIFICATION DIVISION.
           PROGRAM-ID. InCollege.
      ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT InputFile ASSIGN TO "InCollege-Input.txt"
                       ORGANIZATION IS LINE SEQUENTIAL.
                   SELECT OutputFile ASSIGN TO "InCollege-Output.txt"
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS IS SEQUENTIAL.
      DATA DIVISION.
           FILE SECTION.
               FD  InputFile.
               *>File description
               01  InputRecord PIC X(100).
               FD  OutputFile.
               01  OutputRecord PIC X(100).

           WORKING-STORAGE SECTION.
               *> boolean initialized to N which means we haven reached the end of file
               01  EndOfFile   PIC X VALUE "N".

      PROCEDURE DIVISION.
           OPEN INPUT InputFile
           OPEN OUTPUT OutputFile


           DISPLAY "Iowa InCollege project"

           PERFORM UNTIL EndOfFile = "Y"
               READ InputFile
                   AT END
                       MOVE "Y" TO EndOfFile
                   NOT AT END
                       DISPLAY InputRecord
               END-READ
           END-PERFORM


           CLOSE InputFile
           CLOSE OutputFile


           STOP RUN.