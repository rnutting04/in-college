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

           *> Flag variable specifically for InputFile to avoid conflicts.
           01 WS-EOF-Flag-Input PIC X VALUE "N".
               88 EOF-Input VALUE "Y".

           01 WS-Number-Users PIC 9 VALUE 0.

           01 WS-User-Table.
               05 WS-User OCCURS 5 TIMES.
                   10 WS-Username PIC X(20).
                   10 WS-Password PIC X(12).

           01 WS-Line             PIC X(100).

           *> General-purpose counter for loops
           01 COUNTER PIC 9(2) VALUE 0.

           *> Buffer for username input
           01 Input-Username PIC X(20).
           *> Buffer for password input
           01 Input-Password PIC X(12).
           *> Flag to indicate if a user is logged in
           01 WS-Logged-In PIC X VALUE "N".
               88 Logged-In VALUE "Y".

           01 Unique-Username-Flag PIC X VALUE "Y".
               88 Unique-Username VALUE "Y".

           01 WS-Char             PIC X.
           01 WS-Password-Valid   PIC X VALUE "N".
               88 Password-Valid VALUE "Y".
           01 WS-Has-Upper        PIC X VALUE "N".
               88 Has-Upper VALUE "Y".
           01 WS-Has-Digit        PIC X VALUE "N".
               88 Has-Digit VALUE "Y".
           01 WS-Has-Special      PIC X VALUE "N".
               88 Has-Special VALUE "Y".
           01 WS-Password-Length  PIC 9(3) VALUE 0.

      PROCEDURE DIVISION.
           PERFORM MAIN.

           MAIN.
               OPEN INPUT InputFile
               OPEN OUTPUT OutputFile

               *> Load existing users from the users file into memory
               PERFORM LOAD-USERS

               *> Continue running until end of input file
               PERFORM UNTIL EOF-Input
                   PERFORM MAIN-MENU
               END-PERFORM

               *> Save any changes to the user table back to the users file
               PERFORM SAVE-USERS

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

           MAIN-MENU.
               PERFORM UNTIL EOF-Input
                       OR InputRecord = "Create New Account"
                       OR InputRecord = "Log In"
                   *> Display menu header and options
                   MOVE "Welcome to InCollege!" TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "Log In" TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "Create New Account" TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "Enter your choice:" TO WS-Line
                   PERFORM OUTPUT-LINE

                   *> Read user input for menu selection
                   PERFORM READ-INPUT

                   *> If input is not a valid menu option, show error
                   IF NOT (InputRecord = "Log In" OR InputRecord = "Create New Account")
                       MOVE "Invalid choice. Please try again." TO WS-Line
                       PERFORM OUTPUT-LINE
                   END-IF
               END-PERFORM

               *> Handle menu selection after loop
               EVALUATE TRUE
                   WHEN InputRecord = "Log In"
                       PERFORM LOGIN
                   WHEN InputRecord = "Create New Account" AND WS-Number-Users = 5
                       MOVE "All permitted accounts have been created, please come back later" TO WS-Line
                       PERFORM OUTPUT-LINE
                       *> Clear input to avoid looping
                       MOVE SPACES TO InputRecord
                   WHEN InputRecord = "Create New Account"
                       PERFORM CREATE-ACCOUNT
                END-EVALUATE.

           OUTPUT-LINE.
               MOVE WS-Line TO OutputRecord
               DISPLAY WS-Line
               WRITE OutputRecord.

           READ-INPUT.
                *> Attempt to read the next line from InputFile into InputRecord
                READ InputFile INTO InputRecord
                     AT END
                       *> If end of file is reached, set EOF flag and save users
                       SET EOF-Input TO TRUE
                       PERFORM SAVE-USERS
                       CLOSE InputFile
                       CLOSE OutputFile
                       STOP RUN
                     NOT AT END
                         *> If a line was read, copy it to WS-Line for output or processing
                         MOVE InputRecord TO WS-Line
                END-READ.

           LOGIN.
               *> Repeat until the user successfully logs in
                PERFORM UNTIL Logged-In
                   MOVE "Please enter your username:" TO WS-Line
                   PERFORM OUTPUT-LINE

                   PERFORM READ-INPUT
                   MOVE InputRecord TO Input-Username

                   MOVE "Please enter your password:" TO WS-Line
                   PERFORM OUTPUT-LINE

                   PERFORM READ-INPUT
                   MOVE InputRecord TO Input-Password

                   *> Initialize count to 1
                   MOVE 1 TO COUNTER
                   *> Search for matching username and password in user table
                   PERFORM UNTIL COUNTER > WS-Number-Users
                       IF Input-Username = WS-Username(COUNTER)
                          AND Input-Password = WS-Password(COUNTER)
                           MOVE "You have successfully logged in" TO WS-Line
                           PERFORM OUTPUT-LINE
                           MOVE "Y" TO WS-Logged-In
                           PERFORM LOGGED-IN-MENU
                           EXIT PERFORM
                       END-IF

                       *> Move to next user in table
                       ADD 1 TO COUNTER
                   END-PERFORM

                   IF NOT Logged-In
                       MOVE "Incorrect username/password, please try again" TO WS-Line
                       PERFORM OUTPUT-LINE
                   END-IF
                END-PERFORM.

                *>PERFORM POST-LOGIN-MENU

           CREATE-ACCOUNT.
               MOVE "Enter new username" TO WS-Line
               PERFORM OUTPUT-LINE

               PERFORM READ-INPUT
               MOVE InputRecord TO Input-Username

               SET Unique-Username TO TRUE
               *> Initialize count to 1
               MOVE 1 TO COUNTER
               *> Check if username already exists in user table
               PERFORM UNTIL COUNTER > WS-Number-Users
                   IF Input-Username = WS-Username(COUNTER)
                       MOVE "Username already exists. Please try again" TO WS-Line
                       PERFORM OUTPUT-LINE

                       MOVE "N" TO Unique-Username-Flag
                       EXIT PERFORM
                   END-IF
                   *> Move to next user in table
                   ADD 1 TO COUNTER
               END-PERFORM

               IF Unique-Username
                   *> Loop until a valid password is entered
                   PERFORM UNTIL Password-Valid
                       MOVE "Enter new password" TO WS-Line
                       PERFORM OUTPUT-LINE
                       MOVE "Password must be 8-12 characters with at least one uppercase letter, one digit, and one special character." TO WS-Line
                       PERFORM OUTPUT-LINE

                       PERFORM READ-INPUT
                       MOVE InputRecord TO Input-Password

                       *> Validate password according to requirements
                       PERFORM VALIDATE-PASSWORD

                       IF NOT Password-Valid
                           MOVE "Invalid password format. Please try again." TO WS-Line
                           PERFORM OUTPUT-LINE
                       END-IF
                   END-PERFORM

                   *> Add the new user to the table
                   ADD 1 TO WS-Number-Users
                   MOVE Input-Username TO WS-Username(WS-Number-Users)
                   MOVE Input-Password TO WS-Password(WS-Number-Users)
               END-IF.

           VALIDATE-PASSWORD.
               MOVE "N" TO WS-Password-Valid
               MOVE "N" TO WS-Has-Upper
               MOVE "N" TO WS-Has-Digit
               MOVE "N" TO WS-Has-Special

               *> Calculate password length
               MOVE 0 TO WS-Password-Length

               *> Calculate the length of the password
               *> (Don't count the spaces that are automatically added for padding)
               PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 12
                   IF Input-Password(COUNTER:1) NOT = SPACE
                       ADD 1 TO WS-Password-Length
                   END-IF
               END-PERFORM

               *> Check password criteria
               IF WS-Password-Length >= 8 AND WS-Password-Length <= 12
                   PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Password-Length
                       *> Get the next character from the password
                       MOVE Input-Password(COUNTER:1) TO WS-Char
                       *> Check for uppercase, digit, or special character
                       EVALUATE TRUE
                           WHEN WS-Char >= "A" AND WS-Char <= "Z"
                               MOVE "Y" TO WS-Has-Upper
                           WHEN WS-Char >= "0" AND WS-Char <= "9"
                               MOVE "Y" TO WS-Has-Digit
                           WHEN WS-Char >= "a" AND WS-Char <= "z"
                               CONTINUE
                           WHEN WS-Char NOT = SPACE
                               MOVE "Y" TO WS-Has-Special
                       END-EVALUATE
                   END-PERFORM
               END-IF

               *> Set password valid flag if all criteria are met
               IF WS-Has-Upper = "Y" AND WS-Has-Digit = "Y"
                  AND WS-Has-Special = "Y" AND WS-Password-Length >= 8
                   MOVE "Y" TO WS-Password-Valid
               END-IF.

           SAVE-USERS.
               OPEN OUTPUT UsersFile
               *> Loop through all users in the user table
               PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Number-Users
                   *> Copy username and password from table to record fields
                   MOVE WS-Username(COUNTER) TO UR-Username
                   MOVE WS-Password(COUNTER) TO UR-Password
                   *> Write the user record to the file
                   WRITE UserRecord
               END-PERFORM
               CLOSE UsersFile.

           LOGGED-IN-MENU.
               *> Placeholder for post-login menu
               MOVE "Inside Login menu" TO WS-Line
               PERFORM OUTPUT-LINE.
