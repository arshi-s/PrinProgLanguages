#/*
# *********************************************
# *  314 Principles of Programming Languages  *
# *********************************************
# */

CCFLAGS = #-ggdb -Wall

all: compile run

compile: Compiler.c InstrUtils.c InstrUtils.h Utils.c Utils.h
	gcc $(CCFLAGS) Compiler.c InstrUtils.c Utils.c -o compile

run: Interpreter.c InstrUtils.c InstrUtils.h Utils.c Utils.h
	gcc $(CCFLAGS) Interpreter.c InstrUtils.c Utils.c Utils.h -o run

# this will reformat your code according to the linux guidelines.
# be careful when using this command!
pretty: Compiler.c InstrUtils.c InstrUtils.h Utils.c Utils.h
	indent -linux Compiler.c
	indent -linux Instr.h
	indent -linux InstrUtils.c InstrUtils.h
	indent -linux Utils.c Utils.h
	indent -linux Interpreter.c

clean:
	rm -rf compile run tinyL.out

