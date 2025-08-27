CC = gcc
CFLAGS = -Wall -g

all: ccitem cclist ccadd ccdel ccmatch ccyear ccedit ccmenu

ccitem: ccitem.c cc.h
	$(CC) $(CFLAGS) -o ccitem ccitem.c

cclist: cclist.c cc.h
	$(CC) $(CFLAGS) -o cclist cclist.c

ccadd: ccadd.c cc.h
	$(CC) $(CFLAGS) -o ccadd ccadd.c

ccdel: ccdel.c cc.h
	$(CC) $(CFLAGS) -o ccdel ccdel.c

ccmatch: ccmatch.c cc.h
	$(CC) $(CFLAGS) -o ccmatch ccmatch.c

ccyear: ccyear.c cc.h
	$(CC) $(CFLAGS) -o ccyear ccyear.c

ccedit: ccedit.c cc.h
	$(CC) $(CFLAGS) -o ccedit ccedit.c

ccmenu: ccmenu.c
	$(CC) $(CFLAGS) -o ccmenu ccmenu.c

clean:
	rm -f ccitem cclist ccadd ccdel ccmatch ccyear ccedit ccmenu
