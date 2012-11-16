#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STACK_SIZE 128

typedef enum {
	typeInt,
	typeStr
} Type;

typedef union {
	int n;
	char *str;
} Data;

typedef struct {
	Type type;
	Data val;
} Cell;

typedef struct {
	int top;
	Cell *data[STACK_SIZE];
} Stack;

void push(Stack *s, Cell *word) {
	if ( s->top == STACK_SIZE )
		perror("Out of stack space");
		
	s->data[s->top++] = word;
}

Cell *pop(Stack *s) {
	return s->data[--s->top];
}

Cell *parseVal(char *word) {
	char *buf;
	int i;
	char *remainder;
	Cell *c;
	
	if (! (c = malloc( sizeof(Cell) )) )
		perror("Couldn't allocate space for Cell");

	i = strtol(word, &remainder, 0);

	if (word != '\0' && *remainder == '\0') {
		c->type = typeInt;
		c->val.n = i;
	} else {
		if (! (buf = malloc(strlen(word) + 1)) )
			perror("Couldn't allocate space for word");
		strcpy(buf, word);
		c->type = typeStr;
		c->val.str = buf;
	}
	return c;
}


int main(int argc, char *argv[]) {
	char word[1024];
	FILE *fp;
	int i;
	Stack st;
	Cell *c;

	st.top = 0;

	fp = fopen("test.fn", "r");
	if (!fp)
		perror("Couldn't open test.fn");

	while ( fscanf(fp, "%s", word) != EOF ) {
		push(&st, parseVal(word));
	}

	while (st.top > 0) {
		c = pop(&st);
		if (c->type == typeInt)
			printf("%d\n", c->val.n);
		else
			printf("'%s'\n", c->val.str);
	}

	return 0;
}
