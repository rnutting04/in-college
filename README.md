# In-College

A COBOL-based program designed to simulate and manage an "In-College" system. It allows users to create accounts, log in, and perform various operations. The project can be run either directly through the COBOL compiler or using a containerized VS Code development environment.

---

## Features

- User account creation and login
- Persistent account storage using text files
- Menu-driven interface for navigating different options
- Example input/output files included for testing

---

## Repository Structure (Tentative)

```
in-college/
├── .devcontainer/       # VS Code devcontainer configuration (optional)
├── .vscode/             # VS Code workspace settings
├── src/                 # COBOL source code
│   └── InCollege.cob    # Main COBOL program
├── InCollege-Input.txt  # Example input file
├── InCollege-Output.txt # Example output file
├── InCollege-Users.txt  # Stores created accounts
└── README.md            # Project documentation
```

---

## Prerequisites

### Option 1 — Run Without VS Code (Local Execution)

- **COBOL Compiler**: Install [GnuCOBOL](https://sourceforge.net/projects/gnucobol/) or another COBOL compiler.
- **Docker** (optional): Only needed if you want to use containerized execution.

**Install GnuCOBOL (Linux / WSL example):**

```bash
sudo apt update
sudo apt install open-cobol
```

---

### Option 2 — Use DevContainer in VS Code (Recommended)

- [Visual Studio Code](https://code.visualstudio.com/)
- [Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) extension
- [Docker](https://www.docker.com/) installed and running

---

## How to Run

### **1. Running Locally (Without VS Code)**

1. Navigate to the `src` folder:
   ```bash
   cd src
   ```

2. Compile the COBOL program:
   ```bash
   cobc -x InCollege.cob -o InCollege
   ```

3. Run the compiled program:
   ```bash
   ./InCollege
   ```

4. Modify `InCollege-Input.txt` to test specific inputs if desired. Outputs will be written to `InCollege-Output.txt`.

---

### **2. Running in VS Code DevContainer**

This repository includes `.devcontainer` configuration to make development easier.

1. Open the project in **VS Code**.
2. When prompted, **Reopen in Container**.
   Alternatively, use:
   ```
   Ctrl + Shift + P → Remote-Containers: Reopen in Container
   ```
3. VS Code will automatically:
   - Build the Docker container
   - Install GnuCOBOL inside the container
   - Open an environment ready for development
4. Open a terminal in VS Code and run:
   ```bash
   cobc -x src/InCollege.cob -o InCollege
   ./InCollege
   ```

---

## Input / Output Files

- **InCollege-Input.txt** → Provides predefined run of the program
- **InCollege-Test.txt** → Provides predefined batches for testing
- **InCollege-Output.txt** → Program outputs for testing are written here
- **Sample-Output-WeekX.txt** → Programs out for predefined run of program is written here
- **InCollege-Users.txt** → Stores account data persistently

---

## Troubleshooting

- **Permission Denied**
  Ensure your compiled binary is executable:
  ```bash
  chmod +x InCollege
  ```

- **Docker Not Running**
  Start Docker Desktop or your Docker daemon before using the VS Code devcontainer.


