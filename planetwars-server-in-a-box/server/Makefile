all: server expire

server: server.o board.o
	g++ -g -o server server.o board.o

server.o: server.cpp board.h
	g++ -c -g -o server.o server.cpp

board.o: board.cpp board.h
	g++ -c -DDEBUG -g -o board.o board.cpp

expire: expire.cpp
	g++ -o expire expire.cpp

clean:
	rm -f *.o server expire

release:
	rm -f server.tar.gz
	tar zcvf server.tar.gz server.cpp board.h board.cpp expire.cpp Makefile htdocs rate.sh
	mv server.tar.gz /var/www/
