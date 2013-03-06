#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#define STACK_SIZE 128

/* *************************************************************** */
/* *** Data structures                                         *** */
/* *************************************************************** */

struct Stack;

typedef enum {
	typeInt,
	typeStr,
    typeWord,
	typeBool,
    typeChar,
    typeComm,
	typeNil,
	typeFun
} Type;

typedef struct Defn {
	char *name;
	struct Stack *def;
	struct Stack *dict;
} Defn;

typedef union {
	int n;
	char *str;
	Defn *word;
	struct Stack *quot;
} Data;


/* Stack data type */

typedef struct Cell {
	Type type;
	Data val;
	struct Cell *next;
} Cell;

typedef struct Stack {
	Cell *top;
	Cell *end; /* we do heavy appending, so a tail-pointer is more than a nice-to-have! */
} Stack;


/* *************************************************************** */
/* *** Stack manipulation                                      *** */
/* *************************************************************** */

void push(Stack *s, Cell *word) {
	word->next = s->top;
	if ( s->top == NULL ) /* set the tail pointer */
		s->end = word;
	s->top = word;
}

Cell *pop(Stack *s) {
	Cell *c = s->top;
	if (c != NULL)
		s->top = c->next;
	return c;
}


/* *************************************************************** */
/* *** VM & memory management                                  *** */
/* *************************************************************** */


typedef struct VM {
	Stack s;
	Stack dict;
} VM;

Stack *allocStack() {
	Stack *s;
	if (! (s = malloc(sizeof(Stack))) )
		err(1, "Couldn't allocate space for a new stack");
	s->top = NULL;
	s->end = NULL;
	return s;
}

Cell *allocCell() {
	Cell *c;
	if (! (c = malloc(sizeof(Cell))) )
		err(1, "Couldn't allocate space for Cell");
	return c;
}

void freeCell(Cell *c) {
	free(c);
}

/*Defn *lookup(Stack *dict, char *name) {
	Cell *c = dict->top;
	while (c != NULL) {
		if (c->type == typeFun && ( strcmp(name, c->val.word->def) == 0) )
			return c;
		c = c->next;
	}
	return NULL;
}*/


/* primitives */

void primAdd(Stack *s) {

}


/* Pretty printer */

void showCell(Cell *c) {
	if (c->type == typeInt)
		printf("%d\n", c->val.n);
	else if (c->type == typeBool)
		printf("%s\n", c->val.n == 0 ? "False" : "True");
    else if (c->type == typeStr)
        printf("\"%s\"\n", c->val.str);
	else if (c->type == typeChar)
		printf(".%c\n", c->val.n);
	else /* word */
		printf("%s\n", c->val.str);
}

void showStack(Stack *s) {
	Cell *c = s->top;
	while (c != NULL) {
		showCell(c);
		c = c->next;
	}
}


/* Parser */

typedef int (*pPredicate)(int);

int pred_not_eol(int ch) {
	return (! (ch == '\n' || ch == '\r' || ch == EOF));
}
int pred_not_whitespace(int ch) {
	return (ch > 32);
}
int pred_double_quoted_string(int ch) {
	static int esc = 0;

	if (esc) {
		esc = 0;
		return 1;
	}
	if (ch == '\\')
		esc = 1;
	else if (ch == EOF)
		err(1, "End of file reached before closing quote found");

	return (ch != '"');
}

char *consume_while_true(FILE *fp, pPredicate predicate ) {
	char *buf;
	int ch, len = -1;
	int pos = ftell(fp);

	do {
		ch = fgetc(fp);
		len++;
	} while ( (*predicate)(ch) );
	fflush(stdout);

	fseek(fp, pos, SEEK_SET); /* reset fp to the initial position */

	if ( !(buf = malloc(len + 1)) )
		err(1, "Could not allocate space for a string buffer");

    if (fread(buf, 1, len, fp) != len)
        err(1, "Unexpectedly read the wrong number of characters!");

	buf[len] = '\0'; /* make sure the last byte is null to terminate the string */

	return buf;
}

char *consume_comment(FILE *fp) {
	return consume_while_true(fp, &pred_not_eol);
}
Cell *consume_word(FILE *fp) {
    Cell *c = allocCell();
    char *buf, *remainder;
    long pos = ftell(fp);
    int i;

	buf = consume_while_true(fp, &pred_not_whitespace);

    i = strtol(buf, &remainder, 0);

	if (*remainder == '\0') {
		c->type = typeInt;
		c->val.n = i;
        free(buf);
	} else if (strcmp(buf, "True") == 0) {
		c->type = typeBool;
		c->val.n = -1;
        free(buf);
	} else if (strcmp(buf, "False") == 0) {
		c->type = typeBool;
		c->val.n = 0;
        free(buf);
    } else if (strcmp(buf, "--") == 0) {
        c->type = typeComm;
        c->val.str = consume_comment(fp);
	} else {
		c->type = typeWord;
		c->val.str = buf;
	}
    return c;
}
Cell *consume_char(FILE *fp) {
	/* TODO: add basic support for multi-byte UTF-8 chars */
    Cell *c = allocCell();
    int ch = fgetc(fp);
    if (ch == EOF)
        err(1, "Parse error: expected char literal, but found end of file.");
    c->type = typeChar;
    c->val.n = ch;
    return c;
}
Cell *consume_string(FILE *fp, char delim) {
	/* TODO: conversion to linked list */
	/* TODO: unescaping of escaped characters */
    Cell *c = allocCell();
    char *buf;

	buf = consume_while_true(fp, &pred_double_quoted_string);
	fgetc(fp); /* discard closing quote */
	c->type = typeStr;
	c->val.str = buf;

	return c;
}

Stack *parse(FILE *fp) {
    int c;
    char cur;
    Cell *cell;
    Stack *st = allocStack();

    while ( (c = fgetc(fp)) != EOF ) {
        cur = (char) c;
        if (cur <= 32) /* we have a whitespace character */
            continue;

        switch (cur) {
            case '"':
            /*case '`':*/
                cell = consume_string(fp, cur);
                break;
            case '.':
                cell = consume_char(fp);
/*            case '(':
            case '[':
                cell = consume_quotation(fp);
                break;*/
            default:
                ungetc(c, fp);
                cell = consume_word(fp);
        }
        push(st, cell);
    }
    return st;
}


/* main */

int main(int argc, char *argv[]) {
	char word[1024];
	FILE *fp;
	int i;
	Stack *st;
	Cell *c;

	fp = fopen("test.fn", "r");
	if (!fp)
		err(1, "Couldn't open test.fn");

    st = parse(fp);

	showStack(st);

	return 0;
}
