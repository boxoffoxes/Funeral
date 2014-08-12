CC=clang
TARGET=funeral

$(TARGET) : funeral.c
	$(CC) -O2 -o $@ $<

$(TARGET)-gcc : funeral.c
	gcc -O2 -o $@ $<

clean:
	rm -f funeral funeral-gcc
