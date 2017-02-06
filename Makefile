CFLAGS= 
LDFLAGS=
CC=fpc


all: compile

compile: Unpaws.pas
	$(CC) $(CFLAGS) Unpaws.pas 

clean:
	rm -f *.o *.ppu Unpaws

