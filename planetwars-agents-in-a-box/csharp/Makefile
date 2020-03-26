all: MyBot

clean:
	rm *.exe

MyBot:
	mcs MyBot.cs Fleet.cs Planet.cs PlanetWars.cs -main:MyBot

run:
	java -jar ../common/tools/PlayGame.jar ../common/maps/map1.txt 1000 200 log.txt "mono MyBot.exe" "java -jar ../common/examples/DualBot.jar" | java -jar ../common/tools/ShowGame.jar
