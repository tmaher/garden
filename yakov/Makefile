# $Id$

CPPFLAGS=-O2
CFLAGS=-Wall
LDFLAGS=-lresolv

all: meepmeep

meepmeep: meepmeep.o
	$(CC) $(CFLAGS) -o meepmeep $(<) $(LDFLAGS)

clean:
	$(RM) *.o meepmeep
