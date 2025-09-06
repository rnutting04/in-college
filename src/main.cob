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
                   *> Sequential file to store Users data
                   SELECT UsersFile ASSIGN TO "InCollege-Users.txt"
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS IS SEQUENTIAL.
      DATA DIVISION.
           FILE SECTION.
               FD  InputFile.
               *>File description
               01  InputRecord PIC X(100).
               FD  OutputFile.
               01  OutputRecord PIC X(100).

               FD UsersFile.
               01 UserRecord.
                   *> Sub-records start with 05
                   05 UR-Username PIC X(20).
                   05 Space-In-Between PIC X VALUE SPACE.
                   05 UR-Password PIC X(12).

           WORKING-STORAGE SECTION.
           *> Flag variable initialized to N (havent reached the end of file)
           01 WS-EOF-Flag PIC X VALUE "N".
               88 EOF VALUE "Y".

            01 WS-Number-Users PIC 9 VALUE 0.

           01 WS-User-Table.
               05 WS-User OCCURS 5 TIMES.
                   10 WS-Username PIC X(20).
                   10 WS-Password PIC X(12).

           01 WS-Line             PIC X(100).

      PROCEDURE DIVISION.
           MAIN.
               OPEN INPUT InputFile
               OPEN OUTPUT OutputFile

               PERFORM LOAD-USERS

               PERFORM DISPLAY-MAIN-MENU

               *>PERFORM PROCESS-INITIAL-CHOICE

               *>IF Logged-In
                   *>PERFORM POST-LOGIN-MENU
               *>END-IF

               *>PERFORM SAVE-USERS

               CLOSE InputFile
               CLOSE OutputFile
               STOP RUN.

           LOAD-USERS.
               OPEN INPUT UsersFile
               *> Initialize count to 0
               MOVE 0 TO WS-Number-Users
               PERFORM UNTIL WS-Number-Users = 5 OR EOF
                   READ UsersFile INTO UserRecord
                       AT END SET EOF TO TRUE
                       NOT AT END
                           ADD 1 TO WS-Number-Users
                           MOVE UR-Username TO WS-Username(WS-Number-Users)
                           MOVE UR-Password TO WS-Password(WS-Number-Users)
                   END-READ
               END-PERFORM
               CLOSE UsersFile.



           DISPLAY-MAIN-MENU.
               MOVE "Welcome to InCollege!" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Log In" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Create New Account" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Enter your choice:" TO WS-Line
               PERFORM OUTPUT-LINE.


           OUTPUT-LINE.
               MOVE WS-Line TO OutputRecord
               DISPLAY WS-Line
               WRITE OutputRecord.