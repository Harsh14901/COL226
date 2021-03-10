# Boolean Expression Lexer and Parser

This is a lexer and parser for the grammar of boolean expressions.

## Requirements

Before building the executable please ensure that the system has the following dependencies installed. If not then run the following command in a terminal

```bash

$ sudo apt install mlton mlton-tools

```

## Usage

- Extract the zip file and run `make` inside the directory where the extracted files are located.
- This will build the executable `a2` inside the current directory
- Now run `./a2 <filename>` where filename is the file that contains the content to be parsed for boolean expressions. This will produce the output of lexer and parser


## Output

1. The first line of the output is the lexer output that lists all the terminals followed by their respective occurence in the input file.
2. The second line is the parser output which lists the post order traversal of the parse tree generated. The terminals are represented by their representation as in lexer output and the terminals are represented by the production rule applied to reduce to the non-terminal.

## Errors
- On encountering an invalid token the program generates a lex error which is shown as `Unknown Token:<line>:<col>:<character>`
- The parser generates a similar error on encountering a parse error which is shown as `Syntax Error:<line>:<col>:<TOKEN>`