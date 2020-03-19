all: getplayer getratings getmaps canvas

canvas: canvas.c cgi.o
	gcc -static -o canvas canvas.c cgi.o -lz

getplayer: getplayer.cpp cgi.o html.o
	g++ -static -o getplayer getplayer.cpp cgi.o html.o -lz

getmaps: getmaps.cpp cgi.o
	g++ -static -o getmaps getmaps.cpp cgi.o -lz

getratings: getratings.cpp cgi.o html.o
	g++ -static -o getratings getratings.cpp cgi.o html.o -lz

cgi.o: cgi.c cgi.h
	gcc -static -c -o cgi.o cgi.c

html.o: html.c html.h
	gcc -static -c -o html.o html.c

clean:
	rm -f canvas getplayer getratings getmaps *.o *.core

install:
	cp *.js *.css canvas canvas.html getplayer getratings getmaps /var/www/

