#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#define STACK_SIZE 128

/* *************************************************************** */
/* *** Data structures                                         *** */
/* *************************************************************** */

struct Stack;
struct Dict;
struct VM;

typedef enum {
	typeInt,
	typeStr,
    typeWord,
	typeBool,
    typeChar,
    typeComm,
	typeNil,
	typeQuot,
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


typedef void (*fFunc)(struct VM*);

typedef struct Dict {
	struct Dict *dict;
	fFunc *fun;
	char *name;
} Dict;


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

typedef struct VM {
	Stack *stack;
	Dict *dict;
	FILE *src;
} VM;



/* Dictionary data type */

/* *************************************************************** */
/* *** Stack manipulation                                      *** */
/* *************************************************************** */

void push(VM *vm, Cell *word) {
	Stack *s = vm->stack;
	word->next = s->top;
	if ( s->top == NULL ) /* set the tail pointer */
		s->end = word;
	s->top = word;
}

Cell *pop(VM *vm) {
	Stack *s = vm->stack;
	Cell *c = s->top;
	if (c != NULL)
		s->top = c->next;
	return c;
}

Data peek(VM *vm) {
	Stack *s = vm->stack;
	return s->top->val;
}

void poke(VM *vm, Data d) {
	Stack *s = vm->stack;
	s->top->val = d;
}


/* *************************************************************** */
/* *** VM & memory management                                  *** */
/* *************************************************************** */


Stack *allocStack(VM *vm) {
	Stack *s;
	if (! (s = malloc(sizeof(Stack))) )
		err(1, "Couldn't allocate space for a new stack");
	s->top = NULL;
	s->end = NULL;
	return s;
}

Cell *allocCell(VM *vm) {
	Stack *s = vm->stack;
	Cell *c;
	if (! (c = malloc(sizeof(Cell))) )
		err(1, "Couldn't allocate space for Cell");
	return c;
}

void freeCell(Cell *c) {
	free(c);
}

Data popFree(VM *vm) {
	Cell *c = pop(vm);
	Data d = c->val;
	freeCell(c);
	return d;
}
/*Defn *lookup(VM *vm, char *name) {
	Cell *c = dict->top;
	while (c != NULL) {
		if (c->type == typeFun && ( strcmp(name, c->val.word->def) == 0) )
			return c;
		c = c->next;
	}
	return NULL;
}*/


/* primitives */

void primAdd(VM *vm) {
	int n = popFree(vm).n;
	Data d;
	d.n = peek(vm).n + n;
	poke(vm, d);
}
void primSub(VM *vm) {
	Stack *s = vm->stack;
	int n = popFree(vm).n;
	Data d;
	d.n = peek(vm).n - n;
	poke(vm, d);
}
void primMul(VM *vm) {
	Stack *s = vm->stack;
	int n = popFree(vm).n;
	Data d;
	d.n = peek(vm).n * n;
	poke(vm, d);
}
void primDiv(VM *vm) {
	Stack *s = vm->stack;
	int n = popFree(vm).n;
	Data d;
	d.n = peek(vm).n / n;
	poke(vm, d);
}

void primPrepend(VM *vm) {
	Stack *s = vm->stack;

}

void primAppend(VM *vm) {
	/* append the quotation on the top of the stack to the quotation that is second-to-top */
	Stack *s = vm->stack;
	Cell *tq = pop(vm);


}


/* Pretty printer */

void showStack(Stack *s, int depth); /* pre-declare so we can use it recursively */
void showCell(Cell *c, int depth) {
	int i = 0;
	for (i=0; i<depth; i++)
		printf("\t");

	if (c->type == typeInt)
		printf("%d\n", c->val.n);
	else if (c->type == typeBool)
		printf("%s\n", c->val.n == 0 ? "False" : "True");
    else if (c->type == typeStr)
        printf("\"%s\"\n", c->val.str);
	else if (c->type == typeChar)
		printf(".%c\n", c->val.n);
	else if (c->type == typeQuot) {
		printf("[\n");
		showStack(c->val.quot, depth+1);
		for (i=0; i<depth; i++)
			printf("\t");
		printf("]\n");
	} else /* word */
		printf("%s\n", c->val.str);
}

void showStack(Stack *s, int depth) {
	Cell *c = s->top;
	while (c != NULL) {
		showCell(c, depth);
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

char *consume_while_true(VM *vm, pPredicate predicate ) {
	FILE *fp = vm->src;
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

char *consume_comment(VM *vm) {
	return consume_while_true(vm, &pred_not_eol);
}
Cell *consume_word(VM *vm) {
	FILE *fp = vm->src;
	Cell *c = allocCell(vm);
    char *buf, *remainder;
    long pos = ftell(fp);
    int i;

	buf = consume_while_true(vm, &pred_not_whitespace);

    i = strtol(buf, &remainder, 0);

	if (*remainder == '\0') {
		c->type = typeInt;
		c->val.n = i;
        free(buf);
	} else if (strcmp(buf, "True") == 0) {
		c->type = typeBool;
		c->val.n = 1;
        free(buf);
	} else if (strcmp(buf, "False") == 0) {
		c->type = typeBool;
		c->val.n = 0;
        free(buf);
    } else if (strcmp(buf, "--") == 0) {
        c->type = typeComm;
        c->val.str = consume_comment(vm);
	} else {
		c->type = typeWord;
		c->val.str = buf;
	}
    return c;
}
Cell *consume_char(VM *vm) {
	/* TODO: add basic support for multi-byte UTF-8 chars */
	FILE *fp = vm->src;
    Cell *c = allocCell(vm);
    int ch = fgetc(fp);
    if (ch == EOF)
        err(1, "Parse error: expected char literal, but found end of file.");
    c->type = typeChar;
    c->val.n = ch;
    return c;
}
Cell *consume_string(VM *vm, char delim) {
	/* TODO: conversion to linked list */
	/* TODO: unescaping of escaped characters */
	FILE *fp = vm->src;
    Cell *c = allocCell(vm);
    char *buf;

	buf = consume_while_true(vm, &pred_double_quoted_string);
	fgetc(fp); /* discard closing quote */
	c->type = typeStr;
	c->val.str = buf;

	return c;
}
void parse(VM *vm, char terminator); /* pre-declare so we can parse recursively */
Cell *consume_quotation(VM *vm, char terminator) {
	Cell *c = allocCell(vm);
	parse(vm, terminator);
	c->type = typeQuot;
	c->val.quot = vm->stack;
	return c;
}

void parse(VM *vm, char terminator) {
    int c;
    char cur, term=']';
	FILE *fp = vm->src;
    Cell *cell;
    Stack *st = allocStack(vm);

	if (vm->stack == NULL)
		vm->stack = st;

    while ( (c = fgetc(fp)) != terminator ) {
        cur = (char) c;
        if (cur <= 32) /* we have a whitespace character */
            continue;

        switch (cur) {
            case '"':
            /*case '`':*/
                cell = consume_string(vm, cur);
                break;
            case '.':
                cell = consume_char(vm);
				break;
            case '(':
				term = ')';  /* this falls-through... */
            case '[':
                cell = consume_quotation(vm, term);
				term = ']'; /* reset to default for next iteration */
                break;
            default:
                ungetc(c, fp);
                cell = consume_word(vm);
        }
        push(vm, cell);
    }
}


/* main */

int main(int argc, char *argv[]) {
	char word[1024];
	FILE *fp;
	VM vm;
	int i;
	Cell *c;

	vm.stack = NULL;
	vm.dict = NULL;

	fp = fopen("test.fn", "r");
	if (!fp)
		err(1, "Couldn't open test.fn");
	vm.src = fp;

    parse(&vm, EOF);

	showStack(vm.stack, 0);

	return 0;
}
