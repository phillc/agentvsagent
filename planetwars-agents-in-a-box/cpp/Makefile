all: MyBot

clean:
	rm *.o MyBot

MyBot:
	g++ MyBot.cc Planetwars.cc -o MyBot

run:
	java -jar ../common/tools/PlayGame.jar ../common/maps/map1.txt 1000 200 log.txt "./MyBot" "java -jar ../common/examples/DualBot.jar" | java -jar ../common/tools/ShowGame.jar
