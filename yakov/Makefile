# $Id$

CPPFLAGS=   # -O2
CFLAGS=-Wall -g
LDFLAGS=

clean:
	$(RM) *.o meepmeep

all: meepmeep

MEEPOBJ = meepmeep.o yakov_b32.o
meepmeep: $(MEEPOBJ)
	$(CC) $(CFLAGS) -o meepmeep $(MEEPOBJ) $(LDFLAGS)

go: clean all
	./meepmeep
