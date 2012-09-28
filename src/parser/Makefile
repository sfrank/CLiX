CFLAGS=-O2 -Iexpat/xmltok -Iexpat/xmlparse -DXML_NS -DXML_DTD

all: elements

elements: elements.o expat/xmltok/xmlrole.o expat/xmltok/xmltok.o expat/xmlparse/hashtable.o expat/xmlparse/xmlparse.o

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	make -C expat clean
	-rm -f elements elements.o
