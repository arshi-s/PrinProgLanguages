/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *********************************************
 */

/* -------------------------------------------------

            CFG for tinyL LANGUAGE

     <program> ::= <stmt_list> !
	<stmt list> ::= <stmt> <morestmts>
	<morestmts> ::= ; <stmt list> | ε
	<stmt> 	::= <assign> | <read> | <print>
	<assign> ::= <variable> = <expr>
	<read> 	::= ? <variable>
	<print> ::= % <variable>
	<expr> ::= 	+ <expr> <expr> |
				− <expr> <expr> |
				∗ <expr> <expr> |
				& <expr> <expr> |
				| <expr> <expr> |
				<variable> |
				<digit>
	<variable> 	::= a | b | c | d | e | f
	<digit> 	::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

     NOTE: tokens are exactly a single character long

     Example expressions:

			a=+2+25;%a!
			a=|2&3|25;%a!


 ---------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "Instr.h"
#include "InstrUtils.h"
#include "Utils.h"

#define MAX_BUFFER_SIZE 500
#define EMPTY_FIELD 0xFFFFF
#define token *buffer

/* GLOBALS */
static char *buffer = NULL;	/* read buffer */
static int regnum = 1;		/* for next free virtual register number */
static FILE *outfile = NULL;	/* output of code generation */

/* Utilities */
static void CodeGen(OpCode opcode, int field1, int field2, int field3);
static inline void next_token();
static inline int next_register();
static inline int is_digit(char c);
static inline int to_digit(char c);
static inline int is_identifier(char c);
static char *read_input(FILE * f);

/* Routines for recursive descending parser LL(1) */
static void program();
static void stmtlist();
static void morestmts();
static void stmt();
static void assign();
static void read();
static void print();
static int expr();
static int variable();
static int digit();

/*************************************************************************/
/* Definitions for recursive descending parser LL(1)                     */
/*************************************************************************/
static int digit() 
{
	int reg;
	if (!is_digit(token)) { //check if token is valid (needs to be # 0-9)
		ERROR("Expected digit\n");
		exit(EXIT_FAILURE);
	}
	reg = next_register(); //advance index to next reg
	CodeGen(LOADI, reg, to_digit(token), EMPTY_FIELD); //loading immediate # from token (converted token to digit) into reg
	next_token(); //advance token
	//printf("Current token: %c\n", token);
	return reg; //return target register
}

static int variable()
{
	int reg;
	if (!is_identifier(token)) { //check if token is valid (needs to be char a-f)
		ERROR("Expected variable\n");
		exit(EXIT_FAILURE);
	}
	reg = next_register(); 
	CodeGen(LOAD, reg, token, EMPTY_FIELD); //load value of var token into reg (use LOADI for digits and LOAD for vars)
	next_token(); //advance token
	//printf("Current token: %c\n", token);
	return reg; //return target reg
}

static int expr()
{
	//printf("Inside Expr, ");
	int reg, left_reg, right_reg;

	switch (token) {
	//first can be any operation
		case '+':
			next_token(); //not for code generation - just gets next lookahead
			//printf("Current token: %c\n", token);
			left_reg = expr(); //saves left side expr - returns reg # & prints out code
			right_reg = expr(); //saves right side expr - returns reg # & prints out code
			reg = next_register(); //creates target reg to save evaluation of expr
			CodeGen(ADD, reg, left_reg, right_reg); //add left+right to save in reg
			return reg;
		case '-':
			next_token(); //not for code generation - just gets next lookahead
			//printf("Current token: %c\n", token);
			left_reg = expr(); //saves left side expr - returns reg # & prints out code
			right_reg = expr(); //saves right side expr - returns reg # & prints out code
			reg = next_register(); //creates target reg to save evaluation of expr
			CodeGen(SUB, reg, left_reg, right_reg); //subtracts left+right to save in reg
			return reg;
		case '*':
			next_token(); //not for code generation - just gets next lookahead
			//printf("Current token: %c\n", token);
			left_reg = expr(); //saves left side expr - returns reg # & prints out code
			right_reg = expr(); //saves right side expr - returns reg # & prints out code
			reg = next_register(); //creates target reg to save evaluation of expr
			CodeGen(MUL, reg, left_reg, right_reg); //multiples left+right to save in reg
			return reg;
		case '&':
			next_token(); //not for code generation - just gets next lookahead
			//printf("Current token: %c\n", token);
			left_reg = expr(); //saves left side expr - returns reg # & prints out code
			right_reg = expr(); //saves right side expr - returns reg # & prints out code
			reg = next_register(); //creates target reg to save evaluation of expr
			CodeGen(AND, reg, left_reg, right_reg); //bitwise & left+right to save in reg
			return reg;
		case '|':
			next_token(); //not for code generation - just gets next lookahead
			//printf("Current token: %c\n", token);
			left_reg = expr(); //saves left side expr - returns reg # & prints out code
			right_reg = expr(); //saves right side expr - returns reg # & prints out code
			reg = next_register(); //creates target reg to save evaluation of expr
			CodeGen(OR, reg, left_reg, right_reg); //bitwise || left+right to save in reg
			return reg;
	//can be any variable/letter
		case 'a': 
		case 'b':
		case 'c':
		case 'd':
		case 'e':
		case 'f':
			return variable();
	//can be any digit
		case '0': 
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			return digit();
	default:
		ERROR("Symbol %c unknown\n", token);
		exit(EXIT_FAILURE);
	}
}

static void assign() //evaluate expr and store result into memory
{
	//printf("Inside Assign, ");
	char var; 
	if (is_identifier(token)){ //check if token is valid
		var = token; //if token is valid assign it to var
		//printf("Previous token: %c\n", token);
		next_token(); //go to next token (should be =)
		//printf("Current token: %c\n", token);
		if (token == '='){ //next token should be =
			//printf("Previous token: %c\n", token);
			next_token(); //consume =
			//printf("Current token: %c\n", token);
			int result = expr(); //store result of call to expr in var result
			CodeGen(STORE, var, result, EMPTY_FIELD);
		}
	}
	else{
		ERROR("Assign error. %c unknown\n", token);
		exit(EXIT_FAILURE);
	}
}

static void read()
{
	//printf("Inside Read, ");
	if (token != '?') { //first check if token correct (needs to start with ?)
		ERROR("Read error. Current symbol is %c\n", token);
		exit(EXIT_FAILURE);
	}
	next_token(); //consume ? symbol
	//printf("Current token: %c\n", token);
	CodeGen(READ, token, EMPTY_FIELD, EMPTY_FIELD); //store value from reg into memory (var token)
}

static void print()
{
	//printf("Inside Print, "); 
	if (token != '%') { //first check if token correct (needs to start with %)
		ERROR("Print error. Current symbol is %c\n", token);
		exit(EXIT_FAILURE);
	}
	next_token(); //consume % symbol
	if (is_identifier(token) == 1){ //check that the token following % is a variable (returns 1 if is identifier)
		CodeGen(WRITE, token, EMPTY_FIELD, EMPTY_FIELD);
		next_token(); //consume variable
	} else {
		ERROR("Print Error. Current symbol is %c\n", token);
	}
}

static void stmt()
{ 
	//printf("Inside Stmt, ");
	switch (token){ //based on respective (first) token call a certain method
		case 'a': //first of assign is any var, so if you see that call assign
		case 'b':
		case 'c':
		case 'd':
		case 'e':
		case 'f':
			assign(); 
			break;  
		case '?': //first of read is ?, so if see that symbol call read
			read(); 
			next_token(); //move to next token which should be the variable that will hold what's going to be read
			break; 
		case '%': //first of print is %, so if see that symbol call read
			print(); 
			break; 
	}  
	//dont need to call nexttoken() here bc should be called in whatever method you're calling
	//this method is just delegating where to go based on the first token
}

static void morestmts()
{
	//printf("Inside morestmt, ");
	//printf("Current token in morestmts(): %c\n", token);
	if (token == ';'){ //if the first token is ; consume the token then call stmtlist
			next_token(); //consume ; token
			//printf("Current token: %c\n", token);
        	stmtlist(); //parse more stmts recursively
			//morestmts(); 
	}
}

static void stmtlist()
{
	//printf("Inside stmtlist, ");
	//printf("Current token in stmt(): %c\n", token);
	stmt();
	morestmts();

}

static void program()
{
	//printf("Inside program, ");
	//printf("Current token: %c\n", token);
	stmtlist(); 
	if (token != '!') { //every valid tinyL program ends with !
		ERROR("Program error. Current symbol is %c\n", token);
		exit(EXIT_FAILURE);
	}
}

/*************************************************************************/
/* Utility definitions                                                   */
/*************************************************************************/
static void CodeGen(OpCode opcode, int field1, int field2, int field3)
{
	Instruction instr;

	if (!outfile) {
		ERROR("File error\n");
		exit(EXIT_FAILURE);
	}
	instr.opcode = opcode;
	instr.field1 = field1;
	instr.field2 = field2;
	instr.field3 = field3;
	PrintInstruction(outfile, &instr);
}

static inline void next_token()
{
	if (*buffer == '\0') {
		ERROR("End of program input\n");
		exit(EXIT_FAILURE);
	}
	printf("%c ", *buffer);
	if (*buffer == ';')
		printf("\n");
	buffer++;
	if (*buffer == '\0') {
		ERROR("End of program input\n");
		exit(EXIT_FAILURE);
	}
	if (*buffer == '.')
		printf(".\n");
}

static inline int next_register()
{
	return regnum++;
}

static inline int is_digit(char c)
{
	if (c >= '0' && c <= '9')
		return 1;
	return 0;
}

static inline int to_digit(char c)
{
	if (is_digit(c))
		return c - '0';
	WARNING("Non-digit passed to %s, returning zero\n", __func__);
	return 0;
}

static inline int is_identifier(char c)
{
	if (c >= 'a' && c <= 'f') //range used to end at e
		return 1;
	return 0;
}

static char *read_input(FILE * f)
{
	size_t size, i;
	char *b;
	int c;

	for (b = NULL, size = 0, i = 0;;) {
		if (i >= size) {
			size = (size == 0) ? MAX_BUFFER_SIZE : size * 2;
			b = (char *)realloc(b, size * sizeof(char));
			if (!b) {
				ERROR("Realloc failed\n");
				exit(EXIT_FAILURE);
			}
		}
		c = fgetc(f);
		if (EOF == c) {
			b[i] = '\0';
			break;
		}
		if (isspace(c))
			continue;
		b[i] = c;
		i++;
	}
	return b;
}

/*************************************************************************/
/* Main function                                                         */
/*************************************************************************/

int main(int argc, char *argv[])
{
	const char *outfilename = "tinyL.out";
	char *input;
	FILE *infile;

	printf("------------------------------------------------\n");
	printf("CS314 compiler for tinyL\n");
	printf("------------------------------------------------\n");
	if (argc != 2) {
		ERROR("Use of command:\n  compile <tinyL file>\n");
		exit(EXIT_FAILURE);
	}
	infile = fopen(argv[1], "r");
	if (!infile) {
		ERROR("Cannot open input file \"%s\"\n", argv[1]);
		exit(EXIT_FAILURE);
	}
	outfile = fopen(outfilename, "w");
	if (!outfile) {
		ERROR("Cannot open output file \"%s\"\n", outfilename);
		exit(EXIT_FAILURE);
	}
	input = read_input(infile);
	buffer = input;
	program();
	printf("\nCode written to file \"%s\".\n\n", outfilename);
	free(input);
	fclose(infile);
	fclose(outfile);
	return EXIT_SUCCESS;
}
