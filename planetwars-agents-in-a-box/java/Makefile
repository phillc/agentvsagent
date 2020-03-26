all: MyBot

clean:
	rm *.class

MyBot:
	javac MyBot.java

run:
	java -jar ../common/tools/PlayGame.jar ../common/maps/map1.txt 1000 200 log.txt "java MyBot" "java -jar ../common/examples/DualBot.jar" | java -jar ../common/tools/ShowGame.jar
