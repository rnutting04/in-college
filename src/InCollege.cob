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
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-Users-Status.

           SELECT ProfilesFile ASSIGN TO "InCollege-Profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-Profiles-Status.

           *> Sequential file to store Pending Connection Requests
           SELECT ConnectionsFile ASSIGN TO "InCollege-Connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-Connections-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  InputRecord                 PIC X(100).
       FD  OutputFile.
       01  OutputRecord                PIC X(100).

       FD  UsersFile.
       01  UserRecord.
           05  UR-Username             PIC X(20).
           05  UR-Password             PIC X(12).

       FD  ProfilesFile.
       01  ProfileRecord.
           05 PR-Username              PIC X(20).
           05 PR-FirstName             PIC X(20).
           05 PR-LastName              PIC X(20).
           05 PR-University            PIC X(40).
           05 PR-Major                 PIC X(30).
           05 PR-GradYear              PIC 9(4).
           05 PR-About                 PIC X(200).
           05 PR-Exp-Count             PIC 9.
           05 PR-Exp OCCURS 3 TIMES.
              10 PR-Exp-Title          PIC X(30).
              10 PR-Exp-Company        PIC X(30).
              10 PR-Exp-Dates          PIC X(30).
              10 PR-Exp-Desc           PIC X(100).
           05 PR-Edu-Count             PIC 9.
           05 PR-Edu OCCURS 3 TIMES.
              10 PR-Edu-Degree         PIC X(30).
              10 PR-Edu-University     PIC X(40).
              10 PR-Edu-Years          PIC X(15).

       FD  ConnectionsFile.
       01  ConnectionRecord.
           05 CR-Sender                PIC X(20).
           05 CR-Recipient             PIC X(20).
           05 CR-Status                PIC X(8).


       WORKING-STORAGE SECTION.

       *> --- File status
       01 WS-Users-Status     PIC XX VALUE "00".
       01 WS-Profiles-Status  PIC XX VALUE "00".
       *>-------
       01 WS-EOF-Flag                  PIC X VALUE "N".
           88 EOF                      VALUE "Y".
       01 WS-EOF-Flag-Input            PIC X VALUE "N".
           88 EOF-Input                VALUE "Y".

       01 WS-Number-Users              PIC 9 VALUE 0.

       01 WS-User-Table.
           05 WS-User OCCURS 5 TIMES.
              10 WS-Username           PIC X(20).
              10 WS-Password           PIC X(12).

       *> --- Connections: status + table
       01  WS-Connections-Status      PIC XX VALUE "00".
       01  WS-Num-Requests            PIC 9(3) VALUE 0.

       01  WS-Requests-Tbl.
           05 WS-Req OCCURS 50 TIMES.
              10 WR-Sender            PIC X(20).
              10 WR-Recipient         PIC X(20).
              10 WR-Status            PIC X(8).

       *> Reusable constants (padding as fixed width)
       01  CONST.
           05 CONST-PENDING           PIC X(8) VALUE 'PENDING  '.

       *> Search/flow state for Epic 4
       01  WS-Search-Target-User      PIC X(20) VALUE SPACES.
       01  WS-Search-Target-Fullname  PIC X(50) VALUE SPACES.

       01 WS-Line                      PIC X(100).
       01 COUNTER                      PIC 9(2) VALUE 0.
       01 Input-Username               PIC X(20).
       01 Input-Password               PIC X(12).
       01 WS-Logged-In                 PIC X VALUE "N".
           88 Logged-In                VALUE "Y".
       01 Unique-Username-Flag         PIC X VALUE "Y".
           88 Unique-Username          VALUE "Y".
       01 WS-Char                      PIC X.
       01 WS-Password-Valid            PIC X VALUE "N".
           88 Password-Valid           VALUE "Y".
       01 WS-Has-Upper                 PIC X VALUE "N".
           88 Has-Upper                VALUE "Y".
       01 WS-Has-Digit                 PIC X VALUE "N".
           88 Has-Digit                VALUE "Y".
       01 WS-Has-Special               PIC X VALUE "N".
           88 Has-Special              VALUE "Y".
       01 WS-Password-Length           PIC 9(3) VALUE 0.
       01 WS-Current-Username          PIC X(20).

       *> Profiles
       01 WS-Number-Profiles           PIC 9 VALUE 0.
       01 WS-Profile-Table.
           05 WS-Profile OCCURS 5 TIMES.
              10 PF-Username           PIC X(20).
              10 PF-FirstName          PIC X(20).
              10 PF-LastName           PIC X(20).
              10 PF-University         PIC X(40).
              10 PF-Major              PIC X(30).
              10 PF-GradYear           PIC 9(4).
              10 PF-About              PIC X(200).
              10 PF-Exp-Count          PIC 9.
              10 PF-Exp OCCURS 3 TIMES.
                 15 PF-Exp-Title       PIC X(30).
                 15 PF-Exp-Company     PIC X(30).
                 15 PF-Exp-Dates       PIC X(30).
                 15 PF-Exp-Desc        PIC X(100).
              10 PF-Edu-Count          PIC 9.
              10 PF-Edu OCCURS 3 TIMES.
                 15 PF-Edu-Degree      PIC X(30).
                 15 PF-Edu-University  PIC X(40).
                 15 PF-Edu-Years       PIC X(15).

       01 WS-Found-Index               PIC 9 VALUE 0.
       01 WS-Year-OK                   PIC X VALUE "N".
           88 Year-OK                  VALUE "Y".
       01 WS-ZeroLine                  PIC X(100) VALUE SPACES.
       01 WS-Num-Edit                  PIC ZZ9.
       01 WS-ANS                       PIC X VALUE SPACE.  *> Holds Y/N answers

       01 WS-INPUT-TRIM PIC X(100).
       01 WS-Years-OK     PIC X VALUE "N".
       88 Years-OK     VALUE "Y".
       01 WS-Year-Start   PIC 9(4).
       01 WS-Year-End     PIC 9(4).


       01 WS-Search-FullName   PIC X(50).
       01 WS-Display-Index     PIC 9 VALUE 0.
       01 WS-FullName-Build    PIC X(50).

       PROCEDURE DIVISION.
           PERFORM MAIN.
           STOP RUN.

       MAIN.
           OPEN INPUT  InputFile
           OPEN OUTPUT OutputFile

           PERFORM LOAD-USERS
           PERFORM LOAD-PROFILES
           PERFORM LOAD-REQUESTS

           PERFORM UNTIL EOF-Input
               PERFORM MAIN-MENU
           END-PERFORM

           PERFORM SAVE-USERS
           PERFORM SAVE-PROFILES
           PERFORM SAVE-REQUESTS

           CLOSE InputFile
           CLOSE OutputFile
           GOBACK.

           LOAD-USERS.
               MOVE "00" TO WS-Users-Status
               OPEN INPUT UsersFile
               IF WS-Users-Status = "35"
                   *> File missing — create it empty, then reopen for input
                   OPEN OUTPUT UsersFile
                   CLOSE UsersFile
                   OPEN INPUT UsersFile
               END-IF
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

       LOAD-PROFILES.
           MOVE "00" TO WS-Profiles-Status
           OPEN INPUT ProfilesFile
           IF WS-Profiles-Status = "35"
               OPEN OUTPUT ProfilesFile
               CLOSE ProfilesFile
               OPEN INPUT ProfilesFile
           END-IF
           MOVE 0 TO WS-Number-Profiles
           MOVE "N" TO WS-EOF-Flag
           PERFORM UNTIL WS-Number-Profiles = 5 OR EOF
               READ ProfilesFile INTO ProfileRecord
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-Number-Profiles
                       MOVE PR-Username    TO PF-Username(WS-Number-Profiles)
                       MOVE PR-FirstName   TO PF-FirstName(WS-Number-Profiles)
                       MOVE PR-LastName    TO PF-LastName(WS-Number-Profiles)
                       MOVE PR-University  TO PF-University(WS-Number-Profiles)
                       MOVE PR-Major       TO PF-Major(WS-Number-Profiles)
                       MOVE PR-GradYear    TO PF-GradYear(WS-Number-Profiles)
                       MOVE PR-About       TO PF-About(WS-Number-Profiles)
                       MOVE PR-Exp-Count   TO PF-Exp-Count(WS-Number-Profiles)
                       MOVE PR-Edu-Count   TO PF-Edu-Count(WS-Number-Profiles)
                       MOVE PR-Exp(1)      TO PF-Exp(WS-Number-Profiles,1)
                       MOVE PR-Exp(2)      TO PF-Exp(WS-Number-Profiles,2)
                       MOVE PR-Exp(3)      TO PF-Exp(WS-Number-Profiles,3)
                       MOVE PR-Edu(1)      TO PF-Edu(WS-Number-Profiles,1)
                       MOVE PR-Edu(2)      TO PF-Edu(WS-Number-Profiles,2)
                       MOVE PR-Edu(3)      TO PF-Edu(WS-Number-Profiles,3)
               END-READ
           END-PERFORM
           CLOSE ProfilesFile
           MOVE "N" TO WS-EOF-Flag.

       SAVE-PROFILES.
           OPEN OUTPUT ProfilesFile
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Number-Profiles
               MOVE PF-Username(COUNTER)    TO PR-Username
               MOVE PF-FirstName(COUNTER)   TO PR-FirstName
               MOVE PF-LastName(COUNTER)    TO PR-LastName
               MOVE PF-University(COUNTER)  TO PR-University
               MOVE PF-Major(COUNTER)       TO PR-Major
               MOVE PF-GradYear(COUNTER)    TO PR-GradYear
               MOVE PF-About(COUNTER)       TO PR-About
               MOVE PF-Exp-Count(COUNTER)   TO PR-Exp-Count
               MOVE PF-Edu-Count(COUNTER)   TO PR-Edu-Count
               MOVE PF-Exp(COUNTER,1)       TO PR-Exp(1)
               MOVE PF-Exp(COUNTER,2)       TO PR-Exp(2)
               MOVE PF-Exp(COUNTER,3)       TO PR-Exp(3)
               MOVE PF-Edu(COUNTER,1)       TO PR-Edu(1)
               MOVE PF-Edu(COUNTER,2)       TO PR-Edu(2)
               MOVE PF-Edu(COUNTER,3)       TO PR-Edu(3)
               WRITE ProfileRecord
           END-PERFORM
           CLOSE ProfilesFile.

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


       LOAD-REQUESTS.
           MOVE "00" TO WS-Connections-Status
           OPEN INPUT ConnectionsFile
           IF WS-Connections-Status = "35"
               *> File missing — create empty then reopen
               OPEN OUTPUT ConnectionsFile
               CLOSE ConnectionsFile
               OPEN INPUT ConnectionsFile
           END-IF
           MOVE 0 TO WS-Num-Requests
           MOVE "N" TO WS-EOF-Flag
           PERFORM UNTIL WS-Num-Requests = 50 OR EOF
               READ ConnectionsFile INTO ConnectionRecord
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-Num-Requests
                       MOVE CR-Sender    TO WR-Sender(WS-Num-Requests)
                       MOVE CR-Recipient TO WR-Recipient(WS-Num-Requests)
                       MOVE CR-Status    TO WR-Status(WS-Num-Requests)
               END-READ
           END-PERFORM
           CLOSE ConnectionsFile
           MOVE "N" TO WS-EOF-Flag.

       SAVE-REQUESTS.
           OPEN OUTPUT ConnectionsFile
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Num-Requests
               MOVE WR-Sender(COUNTER)    TO CR-Sender
               MOVE WR-Recipient(COUNTER) TO CR-Recipient
               MOVE WR-Status(COUNTER)    TO CR-Status
               WRITE ConnectionRecord
           END-PERFORM
           CLOSE ConnectionsFile.

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
                       PERFORM SAVE-PROFILES
                       PERFORM SAVE-REQUESTS
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
                           MOVE Input-Username TO WS-Current-Username
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
                   *> Reset per-account password state
                   MOVE "N" TO WS-Password-Valid
                   MOVE SPACES TO Input-Password
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
               PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 13
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

       LOGGED-IN-MENU.
           MOVE WS-ZeroLine TO WS-Line
           STRING
               "Welcome, "               DELIMITED BY SIZE
               WS-Current-Username       DELIMITED BY SPACE
               "!"                       DELIMITED BY SIZE
            INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           PERFORM UNTIL EOF-Input
               MOVE "Create/Edit My Profile" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "View My Profile" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Search for a job" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Find someone you know" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "View My Pending Connection Requests" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Learn a new skill" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Enter your choice:" TO WS-Line
               PERFORM OUTPUT-LINE

               PERFORM READ-INPUT

               EVALUATE InputRecord
                   WHEN "Create/Edit My Profile"
                       PERFORM CREATE-OR-EDIT-PROFILE
                   WHEN "View My Profile"
                       PERFORM VIEW-MY-PROFILE
                   WHEN "Search for a job"
                       MOVE "Job search/internship is under construction." TO WS-Line
                       PERFORM OUTPUT-LINE
                   WHEN "Find someone you know"
                       PERFORM FIND-SOMEONE-YOU-KNOW
                       PERFORM OUTPUT-LINE
                   WHEN "View My Pending Connection Requests"
                       PERFORM VIEW-PENDING-REQUESTS
                   WHEN "Learn a new skill"
                       PERFORM LEARN-SKILL-MENU
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-Line
                       PERFORM OUTPUT-LINE
               END-EVALUATE
           END-PERFORM.

       FIND-PROFILE-INDEX.
           MOVE 0 TO WS-Found-Index
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Profiles
               IF WS-Current-Username = PF-Username(COUNTER)
                   MOVE COUNTER TO WS-Found-Index
                   EXIT PERFORM
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM.
       GET-FULLNAME-BY-USERNAME.
           MOVE SPACES TO WS-FullName-Build
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Profiles
               IF PF-Username(COUNTER) = WS-Search-Target-User
                   STRING
                       PF-FirstName(COUNTER) DELIMITED BY SPACE
                       " "                     DELIMITED BY SIZE
                       PF-LastName(COUNTER)   DELIMITED BY SPACE
                    INTO WS-FullName-Build
                   END-STRING
                   EXIT PERFORM
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM.
       CREATE-OR-EDIT-PROFILE.
           PERFORM FIND-PROFILE-INDEX
           IF WS-Found-Index = 0
               IF WS-Number-Profiles < 5
                   ADD 1 TO WS-Number-Profiles
                   MOVE WS-Number-Profiles TO WS-Found-Index
                   MOVE WS-Current-Username TO PF-Username(WS-Found-Index)
               ELSE
                   MOVE "Profile storage limit reached. Cannot create new profile." TO WS-Line
                   PERFORM OUTPUT-LINE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           MOVE " --- Create/Edit Profile --- " TO WS-Line
           PERFORM OUTPUT-LINE

           *> -------- First Name (X(20)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter First Name:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "First Name is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO PF-FirstName(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:20) TO PF-FirstName(WS-Found-Index)

           *> -------- Last Name (X(20)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Last Name:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Last Name is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO PF-LastName(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:20) TO PF-LastName(WS-Found-Index)

           *> -------- University (X(40)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter University/College Attended:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "University is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO PF-University(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:40) TO PF-University(WS-Found-Index)

           *> -------- Major (X(30)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Major:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Major is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO PF-Major(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:30) TO PF-Major(WS-Found-Index)

           *> -------- Graduation Year (9(4)) --------
           MOVE "Enter Graduation Year (YYYY):" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "N" TO WS-Year-OK
           PERFORM UNTIL Year-OK
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM(1:4) NUMERIC
                  AND WS-INPUT-TRIM(5:1) = SPACE
                  AND FUNCTION NUMVAL(WS-INPUT-TRIM(1:4)) >= 1900
                  AND FUNCTION NUMVAL(WS-INPUT-TRIM(1:4)) <= 2099
                   MOVE "Y" TO WS-Year-OK
                   MOVE WS-INPUT-TRIM(1:4) TO PF-GradYear(WS-Found-Index)
               ELSE
                   MOVE "Invalid year. Enter a 4-digit year between 1900 and 2099:" TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM

           *> -------- About Me (X(200); input is 100 chars max) --------
           MOVE "Enter About Me (optional, max 200 chars, blank to skip):" TO WS-Line
           PERFORM OUTPUT-LINE
           PERFORM READ-INPUT
           MOVE InputRecord TO WS-INPUT-TRIM
           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
           IF WS-INPUT-TRIM = SPACES
               MOVE SPACES TO PF-About(WS-Found-Index)
           ELSE
               MOVE SPACES TO PF-About(WS-Found-Index)
               MOVE WS-INPUT-TRIM(1:100) TO PF-About(WS-Found-Index)
           END-IF

           *> ================= EXPERIENCES with strict Y/N and required fields =================
           MOVE 0 TO PF-Exp-Count(WS-Found-Index)
           PERFORM UNTIL PF-Exp-Count(WS-Found-Index) >= 3
               MOVE "Add an experience? (Y/N)" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord(1:1) TO WS-ANS
               IF WS-ANS = "y" MOVE "Y" TO WS-ANS END-IF
               IF WS-ANS = "n" MOVE "N" TO WS-ANS END-IF

               EVALUATE WS-ANS
                   WHEN "N"
                       EXIT PERFORM

                   WHEN "Y"
                       ADD 1 TO PF-Exp-Count(WS-Found-Index)
                       MOVE PF-Exp-Count(WS-Found-Index) TO WS-Num-Edit

                       *> -------- Title (X(30)) [REQUIRED]
                       MOVE SPACES TO WS-INPUT-TRIM
                       PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
                           STRING "Experience #" DELIMITED BY SIZE
                                  WS-Num-Edit    DELIMITED BY SIZE
                                  " - Title:"    DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE
                           PERFORM READ-INPUT
                           MOVE InputRecord TO WS-INPUT-TRIM
                           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
                           IF WS-INPUT-TRIM = SPACES
                               MOVE "Title is required. Please try again." TO WS-Line
                               PERFORM OUTPUT-LINE
                           END-IF
                       END-PERFORM
                       MOVE SPACES TO PF-Exp-Title(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
                       MOVE WS-INPUT-TRIM(1:30)
                            TO PF-Exp-Title(WS-Found-Index, PF-Exp-Count(WS-Found-Index))

                       *> -------- Company (X(30)) [REQUIRED]
                       MOVE SPACES TO WS-INPUT-TRIM
                       PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
                           STRING "Experience #"              DELIMITED BY SIZE
                                  WS-Num-Edit                 DELIMITED BY SIZE
                                  " - Company/Organization:"  DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE
                           PERFORM READ-INPUT
                           MOVE InputRecord TO WS-INPUT-TRIM
                           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
                           IF WS-INPUT-TRIM = SPACES
                               MOVE "Company/Organization is required. Please try again." TO WS-Line
                               PERFORM OUTPUT-LINE
                           END-IF
                       END-PERFORM
                       MOVE SPACES TO PF-Exp-Company(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
                       MOVE WS-INPUT-TRIM(1:30)
                            TO PF-Exp-Company(WS-Found-Index, PF-Exp-Count(WS-Found-Index))

                       *> -------- Dates (X(30)) [REQUIRED]
                       MOVE SPACES TO WS-INPUT-TRIM
                       PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
                           STRING "Experience #" DELIMITED BY SIZE
                                  WS-Num-Edit    DELIMITED BY SIZE
                                  " - Dates (e.g., Summer 2025):" DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE
                           PERFORM READ-INPUT
                           MOVE InputRecord TO WS-INPUT-TRIM
                           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
                           IF WS-INPUT-TRIM = SPACES
                               MOVE "Dates are required. Please try again." TO WS-Line
                               PERFORM OUTPUT-LINE
                           END-IF
                       END-PERFORM
                       MOVE SPACES TO PF-Exp-Dates(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
                       MOVE WS-INPUT-TRIM(1:30)
                            TO PF-Exp-Dates(WS-Found-Index, PF-Exp-Count(WS-Found-Index))

                       *> -------- Description (X(100)) [OPTIONAL]
                       STRING "Experience #" DELIMITED BY SIZE
                              WS-Num-Edit    DELIMITED BY SIZE
                              " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE
                         INTO WS-Line
                       END-STRING
                       PERFORM OUTPUT-LINE
                       PERFORM READ-INPUT
                       MOVE InputRecord TO WS-INPUT-TRIM
                       MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
                       IF WS-INPUT-TRIM = SPACES
                           MOVE SPACES TO PF-Exp-Desc(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
                       ELSE
                           MOVE SPACES TO PF-Exp-Desc(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
                           MOVE WS-INPUT-TRIM(1:100)
                                TO PF-Exp-Desc(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
                       END-IF

                   WHEN OTHER
                       MOVE "Invalid input. Please enter Y or N." TO WS-Line
                       PERFORM OUTPUT-LINE
                       *> do not change count; loop repeats
               END-EVALUATE
           END-PERFORM


           *> ================= EDUCATION with strict Y/N and required fields =================
           MOVE 0 TO PF-Edu-Count(WS-Found-Index)
           PERFORM UNTIL PF-Edu-Count(WS-Found-Index) >= 3
               MOVE "Add an education entry? (Y/N)" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord(1:1) TO WS-ANS
               IF WS-ANS = "y" MOVE "Y" TO WS-ANS END-IF
               IF WS-ANS = "n" MOVE "N" TO WS-ANS END-IF

               EVALUATE WS-ANS
                   WHEN "N"
                       EXIT PERFORM

                   WHEN "Y"
                       ADD 1 TO PF-Edu-Count(WS-Found-Index)
                       MOVE PF-Edu-Count(WS-Found-Index) TO WS-Num-Edit

                       *> -------- Degree (X(30)) [REQUIRED]
                       MOVE SPACES TO WS-INPUT-TRIM
                       PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
                           STRING "Education #" DELIMITED BY SIZE
                                  WS-Num-Edit   DELIMITED BY SIZE
                                  " - Degree:"  DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE
                           PERFORM READ-INPUT
                           MOVE InputRecord TO WS-INPUT-TRIM
                           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
                           IF WS-INPUT-TRIM = SPACES
                               MOVE "Degree is required. Please try again." TO WS-Line
                               PERFORM OUTPUT-LINE
                           END-IF
                       END-PERFORM
                       MOVE SPACES TO PF-Edu-Degree(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
                       MOVE WS-INPUT-TRIM(1:30)
                            TO PF-Edu-Degree(WS-Found-Index, PF-Edu-Count(WS-Found-Index))

                       *> -------- University (X(40)) [REQUIRED]
                       MOVE SPACES TO WS-INPUT-TRIM
                       PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
                           STRING "Education #"     DELIMITED BY SIZE
                                  WS-Num-Edit       DELIMITED BY SIZE
                                  " - University:"  DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE
                           PERFORM READ-INPUT
                           MOVE InputRecord TO WS-INPUT-TRIM
                           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
                           IF WS-INPUT-TRIM = SPACES
                               MOVE "University is required. Please try again." TO WS-Line
                               PERFORM OUTPUT-LINE
                           END-IF
                       END-PERFORM
                       MOVE SPACES TO PF-Edu-University(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
                       MOVE WS-INPUT-TRIM(1:40)
                            TO PF-Edu-University(WS-Found-Index, PF-Edu-Count(WS-Found-Index))

                       *> -------- Years (X(15)) [REQUIRED, validated YYYY-YYYY, 1900..2099, end>=start] --------
                       MOVE "N" TO WS-Years-OK
                       PERFORM UNTIL Years-OK
                           STRING "Education #"             DELIMITED BY SIZE
                                  WS-Num-Edit               DELIMITED BY SIZE
                                  " - Years (e.g., 2022-2026):" DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE

                           PERFORM READ-INPUT
                           MOVE InputRecord TO WS-INPUT-TRIM
                           MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM

                           IF WS-INPUT-TRIM = SPACES
                               MOVE "Years are required. Please try again." TO WS-Line
                               PERFORM OUTPUT-LINE
                           ELSE
                               *> Expect exactly 'YYYY-YYYY' (9 chars); allow nothing after position 9
                               IF WS-INPUT-TRIM(1:4) NUMERIC
                                  AND WS-INPUT-TRIM(5:1) = "-"
                                  AND WS-INPUT-TRIM(6:4) NUMERIC
                                  AND (FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-TRIM)) = 9)
                                   MOVE WS-INPUT-TRIM(1:4) TO WS-Year-Start
                                   MOVE WS-INPUT-TRIM(6:4) TO WS-Year-End

                                   IF WS-Year-End   >= WS-Year-Start
                                       SET Years-OK TO TRUE
                                   ELSE
                                       MOVE "Invalid year range. Use 1900-2099 and ensure end year >= start year." TO WS-Line
                                       PERFORM OUTPUT-LINE
                                   END-IF
                               ELSE
                                   MOVE "Invalid format. Please enter as YYYY-YYYY (e.g., 2022-2026)." TO WS-Line
                                   PERFORM OUTPUT-LINE
                               END-IF
                           END-IF
                       END-PERFORM

                       MOVE SPACES TO PF-Edu-Years(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
                       MOVE WS-INPUT-TRIM(1:10)
                            TO PF-Edu-Years(WS-Found-Index, PF-Edu-Count(WS-Found-Index))

                   WHEN OTHER
                       MOVE "Invalid input. Please enter Y or N." TO WS-Line
                       PERFORM OUTPUT-LINE
               END-EVALUATE
           END-PERFORM


           MOVE "Profile saved successfully!" TO WS-Line
           PERFORM OUTPUT-LINE.

       VIEW-PROFILE-BY-INDEX.
           IF WS-Display-Index = 0 OR WS-Display-Index > WS-Number-Profiles
               MOVE "No profile found." TO WS-Line
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE " --- User Profile --- " TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-FirstName(WS-Display-Index) TRAILING) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-LastName(WS-Display-Index) TRAILING)  DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "University: " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-University(WS-Display-Index) TRAILING) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Major: " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-Major(WS-Display-Index) TRAILING) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Graduation Year: " DELIMITED BY SIZE
                  PF-GradYear(WS-Display-Index) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           IF PF-About(WS-Display-Index) NOT = SPACES
               MOVE SPACES TO WS-Line
               STRING "About Me: " DELIMITED BY SIZE
                      PF-About(WS-Display-Index) DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
           END-IF

           MOVE "Experience:" TO WS-Line
           PERFORM OUTPUT-LINE
           IF PF-Exp-Count(WS-Display-Index) = 0
               MOVE "  (none)" TO WS-Line
               PERFORM OUTPUT-LINE
           ELSE
               PERFORM VARYING COUNTER FROM 1 BY 1
                       UNTIL COUNTER > PF-Exp-Count(WS-Display-Index)
                   MOVE SPACES TO WS-Line
                   STRING "  Title: " DELIMITED BY SIZE
                          PF-Exp-Title(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  Company: " DELIMITED BY SIZE
                          PF-Exp-Company(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  Dates: " DELIMITED BY SIZE
                          PF-Exp-Dates(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   IF PF-Exp-Desc(WS-Display-Index, COUNTER) NOT = SPACES
                       MOVE SPACES TO WS-Line
                       STRING "  Description: " DELIMITED BY SIZE
                              PF-Exp-Desc(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                         INTO WS-Line
                       END-STRING
                       PERFORM OUTPUT-LINE
                   END-IF
               END-PERFORM
           END-IF

           MOVE "Education:" TO WS-Line
           PERFORM OUTPUT-LINE
           IF PF-Edu-Count(WS-Display-Index) = 0
               MOVE "  (none)" TO WS-Line
               PERFORM OUTPUT-LINE
           ELSE
               PERFORM VARYING COUNTER FROM 1 BY 1
                       UNTIL COUNTER > PF-Edu-Count(WS-Display-Index)
                   MOVE SPACES TO WS-Line
                   STRING "  Degree: " DELIMITED BY SIZE
                          PF-Edu-Degree(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  University: " DELIMITED BY SIZE
                          PF-Edu-University(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  Years: " DELIMITED BY SIZE
                          PF-Edu-Years(WS-Display-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE
               END-PERFORM
           END-IF

           MOVE "--------------------" TO WS-Line
           PERFORM OUTPUT-LINE.
       VIEW-MY-PROFILE.
           PERFORM FIND-PROFILE-INDEX
           IF WS-Found-Index = 0
               MOVE "No profile found. Use 'Create/Edit My Profile' first." TO WS-Line
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-Found-Index TO WS-Display-Index
           PERFORM VIEW-PROFILE-BY-INDEX.

       FIND-SOMEONE-YOU-KNOW.
           *> Prompt for full name (required)
           MOVE SPACES TO WS-Search-FullName
           PERFORM UNTIL WS-Search-FullName NOT = SPACES
               MOVE "Enter the full name of the person you are looking for:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-Search-FullName
               MOVE FUNCTION TRIM(WS-Search-FullName TRAILING) TO WS-Search-FullName
               IF WS-Search-FullName = SPACES
                   MOVE "Full name is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM

           *> Search exact full-name match across profiles
           MOVE 0 TO WS-Display-Index
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Profiles OR WS-Display-Index > 0
               MOVE SPACES TO WS-FullName-Build
               STRING
                   FUNCTION TRIM(PF-FirstName(COUNTER) TRAILING) DELIMITED BY SIZE
                   " "                                         DELIMITED BY SIZE
                   FUNCTION TRIM(PF-LastName(COUNTER) TRAILING)  DELIMITED BY SIZE
                 INTO WS-FullName-Build
               END-STRING

               IF WS-FullName-Build = WS-Search-FullName
                   MOVE COUNTER TO WS-Display-Index
               ELSE
                   ADD 1 TO COUNTER
               END-IF
           END-PERFORM

           IF WS-Display-Index > 0
               MOVE " --- Found User Profile --- " TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM VIEW-PROFILE-BY-INDEX

               *> [Epic4] Remember target user for connection request
               MOVE PF-Username(WS-Display-Index) TO WS-Search-Target-User
               MOVE SPACES TO WS-Search-Target-Fullname
               STRING
                   FUNCTION TRIM(PF-FirstName(WS-Display-Index) TRAILING) DELIMITED BY SIZE
                   " "                                                   DELIMITED BY SIZE
                   FUNCTION TRIM(PF-LastName(WS-Display-Index) TRAILING)  DELIMITED BY SIZE
                INTO WS-Search-Target-Fullname
               END-STRING

               *> [Epic4] Offer to send a connection request
               MOVE "1. Send Connection Request" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "2. Back to Main Menu" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Enter your choice:" TO WS-Line
               PERFORM OUTPUT-LINE

               PERFORM READ-INPUT

               EVALUATE InputRecord
                   WHEN "1"
                       PERFORM SEND-CONNECTION-REQUEST
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           ELSE
               MOVE "No one by that name could be found." TO WS-Line
               PERFORM OUTPUT-LINE
           END-IF.

       SEND-CONNECTION-REQUEST.
           *> Guard: target must exist
           IF WS-Search-Target-User = SPACES
               MOVE "No target user selected." TO WS-Line
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Guard: cannot send to self
           PERFORM CANNOT-SEND-TO-SELF
               USING WS-Current-Username WS-Search-Target-User
               GIVING WS-ANS
           IF WS-ANS = "Y"
               MOVE "You cannot send a connection request to yourself." TO WS-Line
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Guard: duplicate or cross-pending exists
           PERFORM REQUEST-EXISTS-BETWEEN
               USING WS-Current-Username WS-Search-Target-User
               GIVING WS-ANS
           IF WS-ANS = "Y"
               MOVE "A pending connection request already exists between you and " TO WS-Line
               PERFORM OUTPUT-LINE
               IF WS-Search-Target-Fullname NOT = SPACES
                   MOVE WS-Search-Target-Fullname TO WS-Line
               ELSE
                   MOVE WS-Search-Target-User TO WS-Line
               END-IF
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Append new request
           IF WS-Num-Requests < 50
               ADD 1 TO WS-Num-Requests
               MOVE WS-Current-Username   TO WR-Sender(WS-Num-Requests)
               MOVE WS-Search-Target-User TO WR-Recipient(WS-Num-Requests)
               MOVE CONST-PENDING         TO WR-Status(WS-Num-Requests)

               *> Durability: save now
               PERFORM SAVE-REQUESTS

               *> Success message
               IF WS-Search-Target-Fullname NOT = SPACES
                   STRING
                       "Connection request sent to " DELIMITED BY SIZE
                       WS-Search-Target-Fullname    DELIMITED BY SIZE
                       "."                           DELIMITED BY SIZE
                    INTO WS-Line
                   END-STRING
               ELSE
                   STRING
                       "Connection request sent to " DELIMITED BY SIZE
                       WS-Search-Target-User        DELIMITED BY SPACE
                       "."                           DELIMITED BY SIZE
                    INTO WS-Line
                   END-STRING
               END-IF
               PERFORM OUTPUT-LINE
           ELSE
               MOVE "Cannot send request: request storage is full." TO WS-Line
               PERFORM OUTPUT-LINE
           END-IF.

       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE 0 TO WS-Found-Index
           MOVE 0 TO COUNTER
           MOVE 0 TO WS-Display-Index

           *> Scan requests for those sent TO current user and still pending
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Num-Requests
               IF WR-Recipient(COUNTER) = WS-Current-Username
                  AND WR-Status(COUNTER) = CONST-PENDING
                   ADD 1 TO WS-Display-Index
                   *> Try to get sender's full name if profile exists
                   PERFORM GET-FULLNAME-BY-USERNAME
                       USING WR-Sender(COUNTER)
                       GIVING WS-FullName-Build
                   IF WS-FullName-Build NOT = SPACES
                       MOVE WS-FullName-Build TO WS-Line
                   ELSE
                       MOVE WR-Sender(COUNTER) TO WS-Line
                   END-IF
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM

           IF WS-Display-Index = 0
               MOVE "You have no pending connection requests at this time." TO WS-Line
               PERFORM OUTPUT-LINE
           END-IF.

       LEARN-SKILL-MENU.
           PERFORM UNTIL EOF-Input
               MOVE "Learn a New Skill:" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Write resume" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Mock interview tips" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Recommended certifications" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Volunteer opportunities" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Data Analysis" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Go Back" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Enter your choice:" TO WS-Line
               PERFORM OUTPUT-LINE

               PERFORM READ-INPUT

               EVALUATE InputRecord
                   WHEN "Write resume" WHEN "Mock interview tips"
                   WHEN "Recommended certifications" WHEN "Volunteer opportunities"
                   WHEN "Data Analysis"
                       MOVE "This skill is under construction." TO WS-Line
                       PERFORM OUTPUT-LINE
                   WHEN "Go Back"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-Line
                       PERFORM OUTPUT-LINE
               END-EVALUATE
           END-PERFORM.
