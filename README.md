### **SimpleC Compiler in F#**
#### *A lexer, parser, semantic analyzer, and type checker for SimpleC programs.*

---

## **Table of Contents**
- [Description](#description)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Modules](#modules)
- [Technologies Used](#technologies-used)
- [Future Improvements](#future-improvements)
- [Contributing](#contributing)
- [License](#license)

---

## **Description**
The SimpleC Compiler is an F# project that implements a **lexer**, **parser**, **semantic analyzer**, and **type checker** for the SimpleC programming language. The lexer converts input SimpleC source code into a list of tokens, the parser validates the syntax, the analyzer collects variable names and types, and the type checker ensures type compatibility in assignments and expressions. The program determines whether a given SimpleC program is valid by verifying it against formal syntax and type rules.

---

## **Features**
- **Lexical Analysis**: Tokenizes SimpleC source code.
- **Syntax Checking**: Parses tokenized input and verifies adherence to SimpleC's grammar.
- **Semantic Analysis**: Collects variable declarations and ensures uniqueness.
- **Type Checking**: Verifies type compatibility in assignments and expressions.
- **Error Handling**: Provides detailed syntax, semantic, and type error messages.
- **Support for Basic Statements**: Recognizes assignments, variable declarations, conditional statements, and I/O operations.

---

## **Installation**
### **Prerequisites**
Ensure you have the following installed:
- **.NET SDK** (for F# development)
- **F# Compiler (fsharpc)**

### **Clone the Repository**
```bash
git clone https://github.com/yourusername/simplec-compiler.git
cd simplec-compiler
```

### **Compile the Code**
```bash
fsharpc -o SimpleC.exe lexer.fs parser.fs analyzer.fs checker.fs main.fs
```

---

## **Usage**
Run the compiled program with:
```bash
dotnet run <filename>.simplec
```
The program will analyze the provided SimpleC source file and output **"Success!"** if the program is valid or an error message otherwise.

---

## **Modules**
| Module | Description |
|--------|------------|
| `lexer.fs` | Converts input SimpleC code into a list of tokens. |
| `parser.fs` | Parses the tokenized input and validates syntax. |
| `analyzer.fs` | Performs semantic analysis by collecting variable names and checking uniqueness. |
| `checker.fs` | Ensures type compatibility in expressions and assignments. |
| `main.fs` | Handles file input and integrates the lexer, parser, analyzer, and checker. |

---

## **Technologies Used**
- **F#**: Functional programming language.
- **.NET SDK**: Compilation and execution environment.

---

## **Future Improvements**
- Extend syntax support to **loops and functions**.
- Implement **code generation** to produce executable SimpleC programs.
- Improve error handling with more detailed debugging output.
- Optimize performance for large SimpleC programs.

---

## **Contributing**
Contributions are welcome! If you’d like to improve this project:
1. Fork the repository.
2. Create a feature branch (`git checkout -b feature-name`).
3. Commit your changes (`git commit -m "Add feature"`).
4. Push to the branch (`git push origin feature-name`).
5. Open a Pull Request.

---

## **License**
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

