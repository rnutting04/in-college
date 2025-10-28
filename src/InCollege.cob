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

           SELECT ConnectionsFile ASSIGN TO "InCollege-Connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-Connections-Status.

           SELECT ActiveConnsFile ASSIGN TO "InCollege-ActiveConns.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-ActiveConns-Status.

           SELECT JobsFile ASSIGN TO "InCollege-Jobs.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-Jobs-Status.

           SELECT ApplicationsFile ASSIGN TO "InCollege-Applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-Applications-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  InputRecord                 PIC X(200).
       FD  OutputFile.
       01  OutputRecord                PIC X(200).

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
           05 CR-From-Username         PIC X(20).
           05 CR-To-Username           PIC X(20).
           05 CR-Status                PIC X(10).
       FD  ActiveConnsFile.
       01  ActiveConnRecord.
           05 ACR-User1                PIC X(20).
           05 ACR-User2                PIC X(20).

       FD  JobsFile.
       01  JobRecord.
           05 JR-ID                    PIC 9(3).
           05 JR-Title                 PIC X(30).
           05 JR-Desc                  PIC X(200).
           05 JR-Emp-Name              PIC X(30).
           05 JR-Location              PIC X(30).
           05 JR-Salary                PIC X(30).

       FD  ApplicationsFile.
       01  ApplicationRecord.
           05 AR-Username              PIC X(20).
           05 AR-JobID                 PIC 9(3).

       WORKING-STORAGE SECTION.

       *> --- File status
       01 WS-Users-Status     PIC XX VALUE "00".
       01 WS-Profiles-Status  PIC XX VALUE "00".
       01 WS-Connections-Status PIC XX VALUE "00".
       01 WS-ActiveConns-Status PIC XX VALUE "00".
       01 WS-Jobs-Status       PIC XX VALUE "00".
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

       01 WS-Line                      PIC X(200).
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

       *> Connections
       01 WS-Number-Connections        PIC 99 VALUE 0.
       01 WS-Connection-Table.
           05 WS-Connection OCCURS 20 TIMES.
              10 CN-From-Username      PIC X(20).
              10 CN-To-Username        PIC X(20).
              10 CN-Status             PIC X(10).

       01 WS-Temp-Username             PIC X(20).
       01 WS-Connection-Valid          PIC X VALUE "Y".
           88 Connection-Valid         VALUE "Y".
       01 WS-Has-Pending               PIC X VALUE "N".
           88 Has-Pending              VALUE "Y".

       *> For established connections
       01 WS-Number-Active-Conns       PIC 99 VALUE 0.
       01 WS-Active-Conn-Table.
           05 WS-Active-Conn OCCURS 20 TIMES.
              10 AC-User1              PIC X(20).
              10 AC-User2              PIC X(20).

       *> Temporary table for processing connection request deletions
       01 WS-Temp-Number-Connections   PIC 99 VALUE 0.
       01 WS-Temp-Connection-Table.
           05 WS-Temp-Connection OCCURS 20 TIMES.
              10 Temp-CN-From-Username PIC X(20).
              10 Temp-CN-To-Username   PIC X(20).
              10 Temp-CN-Status        PIC X(10).

       *> Jobs
       01 WS-Number-Jobs           PIC 9 VALUE 0.
       01 WS-Job-Table.
           05 WS-Job OCCURS 50 TIMES.
               10 JB-ID            PIC 9(3).
               10 JB-Title         PIC X(30).
               10 JB-Desc          PIC X(200).
               10 JB-Emp-Name      PIC X(30).
               10 JB-Location      PIC X(30).
               10 JB-Salary        PIC X(30).

       01 WS-Max-Job-ID              PIC 9(3) VALUE 0.

       *> Application storage

       01 WS-Applications-Status       PIC XX VALUE "00".
       01 WS-Number-Applications       PIC 9(4) VALUE 0.
       01 WS-Applications-Loaded       PIC X VALUE "N".
          88 Apps-Loaded               VALUE "Y".

       01 WS-Application-Table.
           05 WS-Application OCCURS 500 TIMES.
               10 AP-Username         PIC X(20).
               10 AP-JobID            PIC 9(3).

       01 WS-Selected-Job-Index        PIC 9(3) VALUE 0.
       01 WS-Selected-Job-ID           PIC 9(3) VALUE 0.

       01 WS-Tmp-Num                   PIC 9(3) VALUE 0.
       01 WS-My-App-Count         PIC 9(4) VALUE 0.
       01 WS-Idx-3dig               PIC Z(3).
       01 WS-JobID-3dig             PIC Z(3).

       01 WS-App-Applied-Flag        PIC X VALUE "N".
          88 App-Applied        VALUE "Y".
          88 App-Not-Applied            VALUE "N".

       01 JB-Poster OCCURS 500 TIMES.
          05 JB-Poster-User          PIC X(20).

       *> menu strings for job menu
       01 STR-JOB-MENU-HDR             PIC X(50) VALUE "--- Job Search/Internship Menu ---".
       01 STR-BROWSE-HDR               PIC X(50) VALUE "--- Available Job Listings ---".
       01 STR-DETAILS-HDR              PIC X(50) VALUE "--- Job Details ---".
       01 STR-APPS-HDR                 PIC X(50) VALUE "--- Your Job Applications ---".

       PROCEDURE DIVISION.
           PERFORM MAIN.
           STOP RUN.

       MAIN.
           OPEN INPUT  InputFile
           OPEN OUTPUT OutputFile

           PERFORM LOAD-USERS
           PERFORM LOAD-PROFILES
           PERFORM LOAD-CONNECTIONS
           PERFORM LOAD-ACTIVE-CONNS
           PERFORM LOAD-JOBS
           PERFORM LOAD-APPLICATIONS

           PERFORM UNTIL EOF-Input
               PERFORM MAIN-MENU
           END-PERFORM

           PERFORM SAVE-USERS
           PERFORM SAVE-PROFILES
           PERFORM SAVE-CONNECTIONS
           PERFORM SAVE-ACTIVE-CONNS
           PERFORM SAVE-JOBS
           PERFORM SAVE-APPLICATIONS

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

       LOAD-CONNECTIONS.
           MOVE "00" TO WS-Connections-Status
           OPEN INPUT ConnectionsFile
           IF WS-Connections-Status = "35"
               OPEN OUTPUT ConnectionsFile
               CLOSE ConnectionsFile
               OPEN INPUT ConnectionsFile
           END-IF
           MOVE 0 TO WS-Number-Connections
           MOVE "N" TO WS-EOF-Flag
           PERFORM UNTIL WS-Number-Connections = 20 OR EOF
               READ ConnectionsFile INTO ConnectionRecord
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-Number-Connections
                       MOVE CR-From-Username TO CN-From-Username(WS-Number-Connections)
                       MOVE CR-To-Username   TO CN-To-Username(WS-Number-Connections)
                       MOVE CR-Status        TO CN-Status(WS-Number-Connections)
               END-READ
           END-PERFORM
           CLOSE ConnectionsFile
           MOVE "N" TO WS-EOF-Flag.

       SAVE-CONNECTIONS.
           OPEN OUTPUT ConnectionsFile
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Number-Connections
               MOVE CN-From-Username(COUNTER) TO CR-From-Username
               MOVE CN-To-Username(COUNTER)   TO CR-To-Username
               MOVE CN-Status(COUNTER)        TO CR-Status
               WRITE ConnectionRecord
           END-PERFORM
           CLOSE ConnectionsFile.


       LOAD-ACTIVE-CONNS.
           MOVE "00" TO WS-ActiveConns-Status
           OPEN INPUT ActiveConnsFile
           IF WS-ActiveConns-Status = "35"
               OPEN OUTPUT ActiveConnsFile
               CLOSE ActiveConnsFile
               OPEN INPUT ActiveConnsFile
           END-IF

           MOVE 0 TO WS-Number-Active-Conns
           MOVE "N" TO WS-EOF-Flag
           PERFORM UNTIL WS-Number-Active-Conns = 20 OR EOF
               READ ActiveConnsFile INTO ActiveConnRecord
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-Number-Active-Conns
                       MOVE ACR-User1 TO AC-User1(WS-Number-Active-Conns)
                       MOVE ACR-User2 TO AC-User2(WS-Number-Active-Conns)
               END-READ
           END-PERFORM
           CLOSE ActiveConnsFile
           MOVE "N" TO WS-EOF-Flag.

       SAVE-ACTIVE-CONNS.
           OPEN OUTPUT ActiveConnsFile
           PERFORM VARYING COUNTER FROM 1 BY 1
                   UNTIL COUNTER > WS-Number-Active-Conns
               MOVE AC-User1(COUNTER) TO ACR-User1
               MOVE AC-User2(COUNTER) TO ACR-User2
               WRITE ActiveConnRecord
           END-PERFORM
           CLOSE ActiveConnsFile.

       LOAD-JOBS.
           MOVE "00" TO WS-Jobs-Status
           OPEN INPUT JobsFile
           IF WS-Jobs-Status = "35"
               *> File missing — create it empty, then reopen for input
               OPEN OUTPUT JobsFile
               CLOSE JobsFile
               OPEN INPUT JobsFile
           END-IF

           MOVE 0 TO WS-Number-Jobs
           MOVE "N" TO WS-EOF-Flag
           PERFORM UNTIL WS-Number-Jobs = 3 OR EOF
               READ JobsFile INTO JobRecord
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-Number-Jobs
                       MOVE JR-ID        TO JB-ID(WS-Number-Jobs)
                       MOVE JR-Title     TO JB-Title(WS-Number-Jobs)
                       MOVE JR-Desc      TO JB-Desc(WS-Number-Jobs)
                       MOVE JR-Emp-Name  TO JB-Emp-Name(WS-Number-Jobs)
                       MOVE JR-Location  TO JB-Location(WS-Number-Jobs)
                       MOVE JR-Salary    TO JB-Salary(WS-Number-Jobs)
                       *> Make sure each Id is unique
                       IF JR-ID > WS-Max-Job-ID
                       MOVE JR-ID TO WS-Max-Job-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JobsFile
           MOVE "N" TO WS-EOF-Flag.

       SAVE-JOBS.
           OPEN OUTPUT JobsFile
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > WS-Number-Jobs
               MOVE JB-ID(COUNTER)         TO JR-ID
               MOVE JB-Title(COUNTER)      TO JR-Title
               MOVE JB-Desc(COUNTER)       TO JR-Desc
               MOVE JB-Emp-Name(COUNTER)   TO JR-Emp-Name
               MOVE JB-Location(COUNTER)   TO JR-Location
               MOVE JB-Salary(COUNTER)     TO JR-Salary
               WRITE JobRecord
           END-PERFORM
           CLOSE JobsFile.


           MAIN-MENU.
               PERFORM UNTIL EOF-Input
                       OR InputRecord = "Create New Account"
                       OR InputRecord = "Log In"
                   *> Display menu header and options
                   MOVE "Welcome to InCollege!" TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "1. Log In" TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "2. Create New Account" TO WS-Line
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
                       PERFORM SAVE-CONNECTIONS
                       PERFORM SAVE-ACTIVE-CONNS
                       PERFORM SAVE-JOBS
                       PERFORM SAVE-APPLICATIONS
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
                           MOVE "You have successfully logged in." TO WS-Line
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
               MOVE "1. View My Profile" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "2. Search for User" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "3. Learn a New Skill" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "4. View My Pending Connection Requests" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "5. View My Network" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "6. Find someone you know" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "7. Search for a job" TO WS-Line
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
                       PERFORM JOBS-MENU
                   WHEN "Find someone you know"
                       PERFORM FIND-SOMEONE-YOU-KNOW
                   WHEN "Search for User"
                       PERFORM FIND-SOMEONE-YOU-KNOW
                   WHEN "Learn a new skill"
                       PERFORM LEARN-SKILL-MENU
                   WHEN "View My Pending Connection Requests"
                       PERFORM MANAGE-PENDING-REQUESTS
                   WHEN "View My Network"
                       PERFORM VIEW-MY-NETWORK
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-Line
                       PERFORM OUTPUT-LINE
               END-EVALUATE
           END-PERFORM.

       MANAGE-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE "N" TO WS-Has-Pending
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Connections
               IF CN-To-Username(COUNTER) = WS-Current-Username AND
                  CN-Status(COUNTER) = "PENDING"
                   SET Has-Pending TO TRUE
                   PERFORM PROCESS-SINGLE-REQUEST
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM

           IF NOT Has-Pending
               MOVE "You have no pending connection requests at this time." TO WS-Line
               PERFORM OUTPUT-LINE
           ELSE
               PERFORM CLEANUP-HANDLED-REQUESTS
               PERFORM SAVE-CONNECTIONS
               PERFORM SAVE-ACTIVE-CONNS
           END-IF

           MOVE "-----------------------------------" TO WS-Line
           PERFORM OUTPUT-LINE.

       PROCESS-SINGLE-REQUEST.
           MOVE SPACES TO WS-Line
           STRING "Request from: " DELIMITED BY SIZE
                  FUNCTION TRIM(CN-From-Username(COUNTER) TRAILING)
                     DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE "1. Accept" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "2. Reject" TO WS-Line
           PERFORM OUTPUT-LINE
           STRING "Enter your choice for " DELIMITED BY SIZE
                  FUNCTION TRIM(CN-From-Username(COUNTER) TRAILING)
                     DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           PERFORM READ-INPUT
           EVALUATE InputRecord
               WHEN "Accept"
                   ADD 1 TO WS-Number-Active-Conns
                   MOVE CN-From-Username(COUNTER)
                       TO AC-User1(WS-Number-Active-Conns)
                   MOVE CN-To-Username(COUNTER)
                       TO AC-User2(WS-Number-Active-Conns)

                   MOVE "ACCEPTED" TO CN-Status(COUNTER)
                   MOVE SPACES TO WS-Line
                   STRING "Connection request from " DELIMITED BY SIZE
                          FUNCTION TRIM(CN-From-Username(COUNTER))
                          " accepted!" DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

               WHEN "Reject"
                   MOVE "REJECTED" TO CN-Status(COUNTER)
                   MOVE SPACES TO WS-Line
                   STRING "Connection request from " DELIMITED BY SIZE
                          FUNCTION TRIM(CN-From-Username(COUNTER))
                          " rejected." DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE

               WHEN OTHER
                   MOVE "Invalid choice. Request ignored." TO WS-Line
                   PERFORM OUTPUT-LINE
           END-EVALUATE.

       CLEANUP-HANDLED-REQUESTS.
           MOVE 0 TO WS-Temp-Number-Connections
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Connections
               IF CN-Status(COUNTER) = "PENDING"
                   ADD 1 TO WS-Temp-Number-Connections
                   MOVE WS-Connection(COUNTER) TO
                       WS-Temp-Connection(WS-Temp-Number-Connections)
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM

           MOVE WS-Temp-Number-Connections TO WS-Number-Connections
           MOVE WS-Temp-Connection-Table TO WS-Connection-Table.

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

           MOVE "--- Found User Profile ---" TO WS-Line
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

           IF PF-Exp-Count(WS-Display-Index) > 0
               MOVE "Experience:" TO WS-Line
               PERFORM OUTPUT-LINE
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

           IF PF-Edu-Count(WS-Display-Index) > 0
               MOVE "Education:" TO WS-Line
               PERFORM OUTPUT-LINE
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

           MOVE "-------------------------" TO WS-Line
           PERFORM OUTPUT-LINE.

       VIEW-MY-PROFILE.
           PERFORM FIND-PROFILE-INDEX
           IF WS-Found-Index = 0
               MOVE "No profile found. Use 'Create/Edit My Profile' first." TO WS-Line
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF
           MOVE "--- User Profile ---" TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-FirstName(WS-Found-Index) TRAILING) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-LastName(WS-Found-Index) TRAILING)  DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "University: " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-University(WS-Found-Index) TRAILING) DELIMITED BY SIZE
             INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Major: " DELIMITED BY SIZE
                  FUNCTION TRIM(PF-Major(WS-Found-Index) TRAILING) DELIMITED BY SIZE
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
               PERFORM VIEW-PROFILE-BY-INDEX
               PERFORM SHOW-CONNECTION-OPTIONS
           ELSE
               MOVE "No one by that name could be found." TO WS-Line
               PERFORM OUTPUT-LINE
           END-IF.

       SHOW-CONNECTION-OPTIONS.
           MOVE "1. Send Connection Request" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "2. Back to Main Menu" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "Enter your choice:" TO WS-Line
           PERFORM OUTPUT-LINE

           PERFORM READ-INPUT

           EVALUATE InputRecord
               WHEN "Send Connection Request"
                   PERFORM SEND-CONNECTION-REQUEST
               WHEN "Back to Main Menu"
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice." TO WS-Line
                   PERFORM OUTPUT-LINE
           END-EVALUATE.

       SEND-CONNECTION-REQUEST.
           *> Validate request: cannot send to self, already connected, or pending request exists
           MOVE "Y" TO WS-Connection-Valid
           MOVE "N" TO WS-Has-Pending

           *> Check if trying to connect with self
           IF PF-Username(WS-Display-Index) = WS-Current-Username
               MOVE "You cannot send a connection request to yourself." TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "N" TO WS-Connection-Valid
               EXIT PARAGRAPH
           END-IF

           PERFORM CHECK-ALREADY-CONNECTED
           IF NOT Connection-Valid
               EXIT PARAGRAPH
           END-IF


           *> Check for existing connection or pending request
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Connections

               *> Check if the other user has already sent me a pending request
               IF CN-From-Username(COUNTER) = PF-Username(WS-Display-Index)
                  AND CN-To-Username(COUNTER) = WS-Current-Username
                  AND CN-Status(COUNTER) = "PENDING"
                   MOVE SPACES TO WS-Line
                   STRING "This user has already sent you a connection request." DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE
                   MOVE "N" TO WS-Connection-Valid
                   EXIT PERFORM
               END-IF

               *> Check if I've already sent a pending request to them
               IF CN-From-Username(COUNTER) = WS-Current-Username
                  AND CN-To-Username(COUNTER) = PF-Username(WS-Display-Index)
                  AND CN-Status(COUNTER) = "PENDING"
                   MOVE "You have already sent a connection request to this user." TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "N" TO WS-Connection-Valid
                   EXIT PERFORM
               END-IF

               ADD 1 TO COUNTER
           END-PERFORM

           *> If valid, add the connection request
           IF Connection-Valid
               IF WS-Number-Connections < 20
                   ADD 1 TO WS-Number-Connections
                   MOVE WS-Current-Username TO CN-From-Username(WS-Number-Connections)
                   MOVE PF-Username(WS-Display-Index) TO CN-To-Username(WS-Number-Connections)
                   MOVE "PENDING" TO CN-Status(WS-Number-Connections)
                   MOVE SPACES TO WS-Line
                   STRING "Connection request sent to " DELIMITED BY SIZE
                          FUNCTION TRIM(PF-FirstName(WS-Display-Index) TRAILING) DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          FUNCTION TRIM(PF-LastName(WS-Display-Index) TRAILING) DELIMITED BY SIZE
                          "." DELIMITED BY SIZE
                     INTO WS-Line
                   END-STRING
                   PERFORM OUTPUT-LINE
               ELSE
                   MOVE "Connection storage limit reached." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-IF.

       CHECK-ALREADY-CONNECTED.
          MOVE 1 TO COUNTER
          PERFORM UNTIL COUNTER > WS-Number-Active-Conns
              IF (AC-User1(COUNTER) = WS-Current-Username AND
                  AC-User2(COUNTER) = PF-Username(WS-Display-Index))
               OR (AC-User2(COUNTER) = WS-Current-Username AND
                  AC-User1(COUNTER) = PF-Username(WS-Display-Index))
                  MOVE "You are already connected with this user." TO WS-Line
                  PERFORM OUTPUT-LINE
                  MOVE "N" TO WS-Connection-Valid
                  EXIT PERFORM
              END-IF
              ADD 1 TO COUNTER
          END-PERFORM.

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

       VIEW-MY-NETWORK.
           MOVE "--- Your Network ---" TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE "N" TO WS-Has-Pending *> Re-using flag for "has connections"

           PERFORM VARYING WS-Found-Index FROM 1 BY 1
               UNTIL WS-Found-Index > WS-Number-Active-Conns

               *> Check if the current user is in the FIRST column (AC-User1)
               IF AC-User1(WS-Found-Index) = WS-Current-Username
                   MOVE AC-User2(WS-Found-Index) TO WS-Temp-Username
                   SET Has-Pending TO TRUE
                   PERFORM DISPLAY-CONNECTION-DETAILS
               END-IF

               *> Check if the current user is in the SECOND column (AC-User2)
               IF AC-User2(WS-Found-Index) = WS-Current-Username
                   MOVE AC-User1(WS-Found-Index) TO WS-Temp-Username
                   SET Has-Pending TO TRUE
                   PERFORM DISPLAY-CONNECTION-DETAILS
               END-IF
           END-PERFORM

           IF NOT Has-Pending
               MOVE "You have no connections in your network." TO WS-Line
               PERFORM OUTPUT-LINE
           END-IF

           MOVE "--------------------" TO WS-Line
           PERFORM OUTPUT-LINE.

       DISPLAY-CONNECTION-DETAILS.
           MOVE 0 TO WS-Display-Index
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Profiles
               IF PF-Username(COUNTER) = WS-Temp-Username
                   MOVE COUNTER TO WS-Display-Index
                   EXIT PERFORM
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM

           IF WS-Display-Index > 0
               MOVE SPACES TO WS-Line
               STRING "Connected with: " DELIMITED BY SIZE
                 FUNCTION TRIM(PF-FirstName(WS-Display-Index))
                 " " DELIMITED BY SIZE
                 FUNCTION TRIM(PF-LastName(WS-Display-Index))
                 " (University: " DELIMITED BY SIZE
                 FUNCTION TRIM(PF-University(WS-Display-Index))
                 ", Major: " DELIMITED BY SIZE
                 FUNCTION TRIM(PF-Major(WS-Display-Index))
                 ")" DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE

           END-IF.

       JOBS-MENU.
           PERFORM UNTIL EOF-Input
               MOVE "--- Job Search/Internship Menu ---" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "1. Post a Job/Internship" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "2. Browse Jobs/Internships" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "3. View My Applications" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "4. Back to Main Menu" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Enter your choice:" TO WS-Line
               PERFORM OUTPUT-LINE

               PERFORM READ-INPUT

               EVALUATE InputRecord
                   WHEN "1. Post a Job/Internship"
                       PERFORM POST-JOB
                   WHEN "2. Browse Jobs/Internships"
                       PERFORM BROWSE-JOBS
                   WHEN "3. View My Applications"
                       PERFORM VIEW-MY-APPLICATIONS
                   WHEN "4. Back to Main Menu"
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-Line
                       PERFORM OUTPUT-LINE
           END-PERFORM.

       POST-JOB.
           MOVE "--- Post a New Job/Internship ---" TO WS-Line
           PERFORM OUTPUT-LINE


           *> -------- Job Title (X(30)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Job Title:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT

               ADD 1 TO WS-Number-Jobs
               MOVE WS-Number-Jobs TO WS-Found-Index

               ADD 1 TO WS-Max-Job-ID
               MOVE WS-Max-Job-ID TO JB-ID(WS-Found-Index)

               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Job Title is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO JB-Title(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:30) TO JB-Title(WS-Found-Index)

           *> -------- Description (X(200) [REQUIRED]; input is 100 chars max) --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Description (max 200 chars):" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Description is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO JB-Desc(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:100) TO JB-Desc(WS-Found-Index)

           *> -------- Employer Name (X(30)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Employer Name:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Employer Name is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO JB-Emp-Name(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:30) TO JB-Emp-Name(WS-Found-Index)

           *> -------- Location (X(30)) [REQUIRED] --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Location:" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Location is required. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO JB-Location(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:30) TO JB-Location(WS-Found-Index)

           *> -------- Location (X(30)) --------
           MOVE SPACES TO WS-INPUT-TRIM
           PERFORM UNTIL WS-INPUT-TRIM NOT = SPACES
               MOVE "Enter Salary (optional, enter 'NONE' to skip):" TO WS-Line
               PERFORM OUTPUT-LINE
               PERFORM READ-INPUT
               MOVE InputRecord TO WS-INPUT-TRIM
               MOVE FUNCTION TRIM(WS-INPUT-TRIM TRAILING) TO WS-INPUT-TRIM
               IF WS-INPUT-TRIM = SPACES
                   MOVE "Salary cannot be left blank, enter 'NONE' to skip. Please try again." TO WS-Line
                   PERFORM OUTPUT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO JB-Salary(WS-Found-Index)
           MOVE WS-INPUT-TRIM(1:30) TO JB-Salary(WS-Found-Index)
           MOVE WS-Current-Username TO JB-Poster-User(WS-Number-Jobs)
           MOVE "Job posted successfully!" TO WS-Line
           PERFORM OUTPUT-LINE.


       LOAD-APPLICATIONS.
           IF Apps-Loaded
               EXIT PARAGRAPH
           END-IF
           MOVE "00" TO WS-Applications-Status
           OPEN INPUT ApplicationsFile
           IF WS-Applications-Status = "35"
               *> File missing — create empty and reopen
               OPEN OUTPUT ApplicationsFile
               CLOSE ApplicationsFile
               OPEN INPUT ApplicationsFile
           END-IF
           MOVE 0 TO WS-Number-Applications
           MOVE "N" TO WS-EOF-Flag
           PERFORM UNTIL EOF
               READ ApplicationsFile INTO ApplicationRecord
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-Number-Applications
                       MOVE AR-Username TO AP-Username(WS-Number-Applications)
                       MOVE AR-JobID    TO AP-JobID(WS-Number-Applications)
               END-READ
           END-PERFORM
           CLOSE ApplicationsFile
           MOVE "Y" TO WS-Applications-Loaded
           .

       *> Persist all applications back to file
       SAVE-APPLICATIONS.
           OPEN OUTPUT ApplicationsFile
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Applications
               MOVE AP-Username(COUNTER) TO AR-Username
               MOVE AP-JobID(COUNTER)    TO AR-JobID
               WRITE ApplicationRecord
               ADD 1 TO COUNTER
           END-PERFORM
           CLOSE ApplicationsFile
           .

       *> Utility: check if current user already applied to job id in WS-Selected-Job-ID
       ALREADY-APPLIED.
           MOVE "N" TO WS-App-Applied-Flag
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Applications
               IF AP-Username(COUNTER) = WS-Current-Username
                  AND AP-JobID(COUNTER) = WS-Selected-Job-ID
                   MOVE "Y" TO WS-App-Applied-Flag
                   EXIT PERFORM
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM
           .

       *> Browse all jobs and allow viewing details/applying
       BROWSE-JOBS.
           PERFORM LOAD-APPLICATIONS

           PERFORM UNTIL EOF-Input
               MOVE STR-BROWSE-HDR TO WS-Line
               PERFORM OUTPUT-LINE

               IF WS-Number-Jobs = 0
                   MOVE "No job postings are currently available." TO WS-Line
                   PERFORM OUTPUT-LINE
                   MOVE "-----------------------------" TO WS-Line
                   PERFORM OUTPUT-LINE
                   EXIT PERFORM
               END-IF

               *> List jobs with 1-based numbering
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Jobs
               MOVE COUNTER            TO WS-Idx-3dig
               MOVE JB-ID(COUNTER)     TO WS-JobID-3dig

               MOVE SPACES TO WS-Line
               STRING
                   FUNCTION TRIM(WS-Idx-3dig)          DELIMITED BY SIZE
                   ". "                                DELIMITED BY SIZE
                   FUNCTION TRIM(JB-Title(COUNTER))    DELIMITED BY SIZE
                   " at "                              DELIMITED BY SIZE
                   FUNCTION TRIM(JB-Emp-Name(COUNTER)) DELIMITED BY SIZE
                   " ("                                DELIMITED BY SIZE
                   FUNCTION TRIM(JB-Location(COUNTER)) DELIMITED BY SIZE
                   ") [ID: "                           DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JobID-3dig)        DELIMITED BY SIZE
                   "]"                                 DELIMITED BY SIZE
                 INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE

               ADD 1 TO COUNTER
           END-PERFORM

               MOVE "-----------------------------" TO WS-Line
               PERFORM OUTPUT-LINE
               MOVE "Enter job number to view details, or 0 to go back:" TO WS-Line
               PERFORM OUTPUT-LINE

               PERFORM READ-INPUT

               *> Interpret the number typed in InputRecord
               MOVE 0 TO WS-Tmp-Num
               MOVE FUNCTION NUMVAL (InputRecord) TO WS-Tmp-Num

               IF WS-Tmp-Num = 0
                   EXIT PERFORM
               ELSE
                   IF WS-Tmp-Num >= 1 AND WS-Tmp-Num <= WS-Number-Jobs
                       MOVE WS-Tmp-Num TO WS-Selected-Job-Index
                       PERFORM DISPLAY-JOB-DETAILS
                   ELSE
                       MOVE "Invalid selection." TO WS-Line
                       PERFORM OUTPUT-LINE
                   END-IF
               END-IF
           END-PERFORM
           .

       *> Show details for selected job and allow Apply/Back
       DISPLAY-JOB-DETAILS.
           MOVE STR-DETAILS-HDR TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE JB-ID(WS-Selected-Job-Index) TO WS-Selected-Job-ID

           MOVE SPACES TO WS-Line
           STRING "Title: " FUNCTION TRIM(JB-Title(WS-Selected-Job-Index))
             DELIMITED BY SIZE INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Description: " FUNCTION TRIM(JB-Desc(WS-Selected-Job-Index))
             DELIMITED BY SIZE INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Employer: " FUNCTION TRIM(JB-Emp-Name(WS-Selected-Job-Index))
             DELIMITED BY SIZE INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Location: " FUNCTION TRIM(JB-Location(WS-Selected-Job-Index))
             DELIMITED BY SIZE INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING "Salary: " FUNCTION TRIM(JB-Salary(WS-Selected-Job-Index))
             DELIMITED BY SIZE INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE "-------------------" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "Apply for this Job" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "Back to Job List" TO WS-Line
           PERFORM OUTPUT-LINE
           MOVE "Enter your choice:" TO WS-Line
           PERFORM OUTPUT-LINE

           PERFORM READ-INPUT

           EVALUATE TRUE
               WHEN InputRecord = "Apply for this Job"
                   PERFORM APPLY-TO-JOB
               WHEN InputRecord = "Back to Job List"
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice. Returning to list." TO WS-Line
                   PERFORM OUTPUT-LINE
           END-EVALUATE
           .


       *> Add an application record for current user -> selected job
       APPLY-TO-JOB.
           PERFORM LOAD-APPLICATIONS
           IF JB-Poster-User(WS-Selected-Job-Index) = WS-Current-Username
              MOVE "You cannot apply to a job you posted." TO WS-Line
              PERFORM OUTPUT-LINE
              EXIT PARAGRAPH
           END-IF
           *> avoid duplicates
           PERFORM ALREADY-APPLIED
           IF App-Applied
               MOVE SPACES TO WS-Line
               STRING "You have already applied for "
                      FUNCTION TRIM(JB-Title(WS-Selected-Job-Index))
                      " at "
                      FUNCTION TRIM(JB-Emp-Name(WS-Selected-Job-Index))
                 DELIMITED BY SIZE INTO WS-Line
               END-STRING
               PERFORM OUTPUT-LINE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-Number-Applications
           MOVE WS-Current-Username            TO AP-Username(WS-Number-Applications)
           MOVE JB-ID(WS-Selected-Job-Index)   TO AP-JobID(WS-Number-Applications)

           *> Confirmation message (spec format)
           STRING
               "Your application for "
               FUNCTION TRIM(JB-Title(WS-Selected-Job-Index))
               " at "
               FUNCTION TRIM(JB-Emp-Name(WS-Selected-Job-Index))
               " has been submitted."
               DELIMITED BY SIZE
               INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE
           .

       *> Report: list applications for the logged-in user
       VIEW-MY-APPLICATIONS.
           PERFORM LOAD-APPLICATIONS

           MOVE STR-APPS-HDR TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING
               "Application Summary for "
               FUNCTION TRIM(WS-Current-Username)
               DELIMITED BY SIZE
               INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE "------------------------------" TO WS-Line
           PERFORM OUTPUT-LINE

           *> Count + print details by joining on jobs table via JobID

           MOVE 0 TO WS-My-App-Count

           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > WS-Number-Applications
               IF AP-Username(COUNTER) = WS-Current-Username
                   ADD 1 TO WS-My-App-Count
                   *> Find job row with that ID
                   MOVE 1 TO WS-Tmp-Num
                   PERFORM UNTIL WS-Tmp-Num > WS-Number-Jobs
                       IF JB-ID(WS-Tmp-Num) = AP-JobID(COUNTER)
                           MOVE SPACES TO WS-Line
                           STRING "Job Title: "
                                  FUNCTION TRIM(JB-Title(WS-Tmp-Num))
                             DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE

                           MOVE SPACES TO WS-Line
                           STRING "Employer: "
                                  FUNCTION TRIM(JB-Emp-Name(WS-Tmp-Num))
                             DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE

                           MOVE SPACES TO WS-Line
                           STRING "Location: "
                                  FUNCTION TRIM(JB-Location(WS-Tmp-Num))
                             DELIMITED BY SIZE
                             INTO WS-Line
                           END-STRING
                           PERFORM OUTPUT-LINE

                           MOVE "---" TO WS-Line
                           PERFORM OUTPUT-LINE
                           EXIT PERFORM
                       END-IF
                       ADD 1 TO WS-Tmp-Num
                   END-PERFORM
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM

           MOVE "------------------------------" TO WS-Line
           PERFORM OUTPUT-LINE

           MOVE SPACES TO WS-Line
           STRING
               "Total Applications: " DELIMITED BY SIZE
               WS-My-App-Count        DELIMITED BY SIZE
               INTO WS-Line
           END-STRING
           PERFORM OUTPUT-LINE

           MOVE "------------------------------" TO WS-Line
           PERFORM OUTPUT-LINE
           .
