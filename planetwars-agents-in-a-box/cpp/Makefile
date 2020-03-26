all: MyBot

clean:
	rm *.o MyBot

MyBot:
	g++ MyBot.cc Planetwars.cc -o MyBot

run:
	java -jar ../common/tools/PlayGame.jar ../common/maps/map1.txt 1000 200 log.txt "./MyBot" "java -jar ../common/examples/DualBot.jar" | java -jar ../common/tools/ShowGame.jar

connect: check-login
	gcc -o ../common/tools/tcp ../common/tools/tcp.c
	TCPRUN="./MyBot" ../common/tools/tcp 127.0.0.1 8995 $(AGENTNAME) -p $(PASSWORD) ../common/tools/tcpwrap.sh

check-login:
	@if test "$(AGENTNAME)" = "" ; then \
		echo "AGENTNAME must be set"; \
		exit 1; \
	fi
	@if test "$(PASSWORD)" = "" ; then \
		echo "PASSWORD must be set"; \
		exit 1; \
	fi

