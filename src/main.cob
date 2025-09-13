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
           *> Sequential file to store Profiles data
           SELECT ProfilesFile ASSIGN TO "InCollege-Profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  InputRecord                 PIC X(100).
       FD  OutputFile.
       01  OutputRecord                PIC X(100).

       FD  UsersFile.
       01  UserRecord.
           05  UR-Username             PIC X(20).
           05  Space-In-Between        PIC X VALUE SPACE.
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

       WORKING-STORAGE SECTION.
       01 WS-EOF-Flag                  PIC X VALUE "N".
           88 EOF                      VALUE "Y".
       01 WS-EOF-Flag-Input            PIC X VALUE "N".
           88 EOF-Input                VALUE "Y".

       01 WS-Number-Users              PIC 9 VALUE 0.

       01 WS-User-Table.
           05 WS-User OCCURS 5 TIMES.
              10 WS-Username           PIC X(20).
              10 WS-Password           PIC X(12).

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


       PROCEDURE DIVISION.
           PERFORM MAIN.
           STOP RUN.

       MAIN.
           OPEN INPUT  InputFile
           OPEN OUTPUT OutputFile

           PERFORM LOAD-USERS
           PERFORM LOAD-PROFILES

           PERFORM UNTIL EOF-Input
               PERFORM MAIN-MENU
           END-PERFORM

           PERFORM SAVE-USERS
           PERFORM SAVE-PROFILES

           CLOSE InputFile
           CLOSE OutputFile
           GOBACK.

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
           OPEN INPUT ProfilesFile
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
                       MOVE "Find someone you know is under construction." TO WS-Line
                       PERFORM OUTPUT-LINE
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

           *> ================= EXPERIENCES with Y/N =================
           MOVE 0 TO PF-Exp-Count(WS-Found-Index)
           PERFORM UNTIL PF-Exp-Count(WS-Found-Index) >= 3
               MOVE "Add an experience? (Y/N)" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord(1:1) TO WS-ANS
               IF WS-ANS NOT = "Y"
                   EXIT PERFORM
               END-IF

               ADD 1 TO PF-Exp-Count(WS-Found-Index)
               MOVE PF-Exp-Count(WS-Found-Index) TO WS-Num-Edit

               *> Title (X(30))
               STRING "Experience #" DELIMITED BY SIZE
                      WS-Num-Edit    DELIMITED BY SIZE
                      " - Title:"    DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               MOVE SPACES TO PF-Exp-Title(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
               MOVE WS-INPUT-TRIM(1:30)
                    TO PF-Exp-Title(WS-Found-Index, PF-Exp-Count(WS-Found-Index))

               *> Company (X(30))
               STRING "Experience #"              DELIMITED BY SIZE
                      WS-Num-Edit                 DELIMITED BY SIZE
                      " - Company/Organization:"  DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               MOVE SPACES TO PF-Exp-Company(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
               MOVE WS-INPUT-TRIM(1:30)
                    TO PF-Exp-Company(WS-Found-Index, PF-Exp-Count(WS-Found-Index))

               *> Dates (X(30))
               STRING "Experience #" DELIMITED BY SIZE
                      WS-Num-Edit    DELIMITED BY SIZE
                      " - Dates (e.g., Summer 2025):" DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               MOVE SPACES TO PF-Exp-Dates(WS-Found-Index, PF-Exp-Count(WS-Found-Index))
               MOVE WS-INPUT-TRIM(1:30)
                    TO PF-Exp-Dates(WS-Found-Index, PF-Exp-Count(WS-Found-Index))

               *> Description (X(100), optional)
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
           END-PERFORM

           *> ================= EDUCATION with Y/N =================
           MOVE 0 TO PF-Edu-Count(WS-Found-Index)
           PERFORM UNTIL PF-Edu-Count(WS-Found-Index) >= 3
               MOVE "Add an education entry? (Y/N)" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord(1:1) TO WS-ANS
               IF WS-ANS NOT = "Y"
                   EXIT PERFORM
               END-IF

               ADD 1 TO PF-Edu-Count(WS-Found-Index)
               MOVE PF-Edu-Count(WS-Found-Index) TO WS-Num-Edit

               *> Degree (X(30))
               STRING "Education #" DELIMITED BY SIZE
                      WS-Num-Edit   DELIMITED BY SIZE
                      " - Degree:"  DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               MOVE SPACES TO PF-Edu-Degree(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
               MOVE WS-INPUT-TRIM(1:30)
                    TO PF-Edu-Degree(WS-Found-Index, PF-Edu-Count(WS-Found-Index))

               *> University (X(40))
               STRING "Education #"     DELIMITED BY SIZE
                      WS-Num-Edit       DELIMITED BY SIZE
                      " - University:"  DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               MOVE SPACES TO PF-Edu-University(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
               MOVE WS-INPUT-TRIM(1:40)
                    TO PF-Edu-University(WS-Found-Index, PF-Edu-Count(WS-Found-Index))

               *> Years (X(15))
               STRING "Education #" DELIMITED BY SIZE
                      WS-Num-Edit    DELIMITED BY SIZE
                      " - Years (e.g., 2022-2026):" DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               MOVE SPACES TO PF-Edu-Years(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
               MOVE WS-INPUT-TRIM(1:15)
                    TO PF-Edu-Years(WS-Found-Index, PF-Edu-Count(WS-Found-Index))
           END-PERFORM

           MOVE "Profile saved successfully!" TO WS-Line
           PERFORM OUTPUT-LINE.


       VIEW-MY-PROFILE.
           PERFORM FIND-PROFILE-INDEX
           IF WS-Found-Index = 0
               MOVE "No profile found. Use 'Create/Edit My Profile' first." TO WS-Line
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE " --- Your Profile --- " TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Name: " DELIMITED BY SIZE
                  PF-FirstName(WS-Found-Index) DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  PF-LastName(WS-Found-Index)  DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "University: " DELIMITED BY SIZE
                  PF-University(WS-Found-Index) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Major: " DELIMITED BY SIZE
                  PF-Major(WS-Found-Index) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Graduation Year: " DELIMITED BY SIZE
                  PF-GradYear(WS-Found-Index) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           IF PF-About(WS-Found-Index) NOT = SPACES
               MOVE SPACES TO WS-Line
               STRING "About Me: " DELIMITED BY SIZE
                      PF-About(WS-Found-Index) DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
           END-IF

           MOVE "Experience:" TO WS-Line
           PERFORM OUTPUT-LINE
           IF PF-Exp-Count(WS-Found-Index) = 0
               MOVE "  (none)" TO WS-Line
               PERFORM OUTPUT-LINE
           ELSE
               PERFORM VARYING COUNTER FROM 1 BY 1
                       UNTIL COUNTER > PF-Exp-Count(WS-Found-Index)
                   MOVE SPACES TO WS-Line
                   STRING "  Title: " DELIMITED BY SIZE
                          PF-Exp-Title(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  Company: " DELIMITED BY SIZE
                          PF-Exp-Company(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  Dates: " DELIMITED BY SIZE
                          PF-Exp-Dates(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   IF PF-Exp-Desc(WS-Found-Index, COUNTER) NOT = SPACES
                       MOVE SPACES TO WS-Line
                       STRING "  Description: " DELIMITED BY SIZE
                              PF-Exp-Desc(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                         INTO WS-Line
                       END-STRING
                       PERFORM OUTPUT-LINE
                   END-IF
               END-PERFORM
           END-IF

           MOVE "Education:" TO WS-Line
           PERFORM OUTPUT-LINE
           IF PF-Edu-Count(WS-Found-Index) = 0
               MOVE "  (none)" TO WS-Line
               PERFORM OUTPUT-LINE
           ELSE
               PERFORM VARYING COUNTER FROM 1 BY 1
                       UNTIL COUNTER > PF-Edu-Count(WS-Found-Index)
                   MOVE SPACES TO WS-Line
                   STRING "  Degree: " DELIMITED BY SIZE
                          PF-Edu-Degree(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  University: " DELIMITED BY SIZE
                          PF-Edu-University(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

                   MOVE SPACES TO WS-Line
                   STRING "  Years: " DELIMITED BY SIZE
                          PF-Edu-Years(WS-Found-Index, COUNTER) DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE
               END-PERFORM
           END-IF

           MOVE "--------------------" TO WS-Line
           PERFORM OUTPUT-LINE.

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
