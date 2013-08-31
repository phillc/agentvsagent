package main

import (
  "fmt"
	"syscall"
	"git.apache.org/thrift.git/lib/go/thrift"
	"./lib/AgentVsAgent"
)

func runGame() {
}

type passCardFn func() string
type playCardFn func() string

func play(doPassCards passCardFn, doPlayCard playCardFn) {
	host, hostFound := syscall.Getenv("AVA_HOST")
	port, portFound := syscall.Getenv("AVA_PORT")
	if !hostFound { host = "localhost" }
	if !portFound { port = "4001" }
	var addr string = host + ":" + port

	var transportFactory thrift.TTransportFactory
	var protocolFactory thrift.TProtocolFactory
	protocolFactory = thrift.NewTBinaryProtocolFactoryDefault()
	transportFactory = thrift.NewTTransportFactory()
	transportFactory = thrift.NewTFramedTransportFactory(transportFactory)

	var transport thrift.TTransport
	transport, err := thrift.NewTSocket(addr)
	if err != nil {
		fmt.Println("Error opening socket:", err)
		return
	}
	transport = transportFactory.GetTransport(transport)
	defer transport.Close()
	if err := transport.Open(); err != nil {
		fmt.Println("Error opening transport:", err)
		return
	}

	client := AgentVsAgent.NewHeartsClientFactory(transport, protocolFactory)

	request := AgentVsAgent.NewEntryRequest()
	fmt.Println("Entering arena", request)
	response, err := client.EnterArena(request)
	if err != nil {
		fmt.Println("Error", err)
		return
	}
	ticket := response.Ticket
	if ticket != nil {
		fmt.Println("playing")
		game_info, _, _ := client.GetGameInfo(ticket)
		fmt.Println("game info:", game_info)

		runGame()
		// runGame(ticket, client, doPassCards, doPlayCard)
		fmt.Println("Game is over")
		game_result, _, _, _ := client.GetGameResult(ticket)
		fmt.Println("game result:", game_result)
	} else {
		fmt.Println("No ticket")
		return
	}
}
