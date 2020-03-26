CC=g++
RUNCMD = "./MyBot"

all: MyBot

clean:
	rm -rf *.o MyBot

MyBot: MyBot.o PlanetWars.o

run:
	java -jar ../common/tools/PlayGame.jar ../common/maps/map1.txt 1000 200 log.txt ${RUNCMD} "java -jar ../common/examples/DualBot.jar" | java -jar ../common/tools/ShowGame.jar
