package main

import (
  "fmt"
	"syscall"
	"git.apache.org/thrift.git/lib/go/thrift"
	"./lib/AgentVsAgent"
	"errors"
)

/*func (ex *AgentVsAgent.GameException) Error() string {*/
/*	return "errr????"*/
/*}*/

type passCardFn func(*Round) []*AgentVsAgent.Card
type playCardFn func(*Trick) *AgentVsAgent.Card

type options struct {
	ticket *AgentVsAgent.Ticket
	client *AgentVsAgent.HeartsClient
	doPassCards *passCardFn
	doPlayCard *playCardFn
}

type Trick struct {
	number int
	round *Round
	leader string
	played []*AgentVsAgent.Card
}

func (trick *Trick) run(opts *options) (err error) {
	trick.log("Starting trick")
	currentTrick, ex, err := opts.client.GetTrick(opts.ticket)
	if err != nil { return err }
	if ex != nil { return errors.New((*ex).String()) }
	trick.leader = string(currentTrick.Leader)
	trick.played = currentTrick.Played

	cardToPlay := (*opts.doPlayCard)(trick)

	trickResult, ex, err := opts.client.PlayCard(opts.ticket, cardToPlay)
	if err != nil { return err }
	if ex != nil { return errors.New((*ex).String()) }

	trick.log("trick: result", trickResult)
	trick.played = trickResult.Played
	return
}

func (trick Trick) log(message ...interface{}) {
	newMessage := append([]interface{}{"T:", trick.number}, message...)
	trick.round.log(newMessage...)
}

type Round struct {
	number int
	tricks []*Trick
	dealt []*AgentVsAgent.Card
	held []*AgentVsAgent.Card
	game *Game
}

func (round *Round) createTrick() Trick {
	trick := Trick{number: len(round.tricks) + 1, round: round}
	round.tricks = append(round.tricks, &trick)
	return trick
}

func (round *Round) run(opts *options) (err error) {
	hand, ex, err := opts.client.GetHand(opts.ticket)
	if err != nil { return err }
	if ex != nil { return errors.New((*ex).String()) }
	round.log("You were dealt:", hand)
	round.dealt = hand
	round.held = hand

	err = round.passCards(opts)
	if err != nil { return err }
	return round.playTrick(opts)
}

func (round *Round) passCards(opts *options) (err error) {
	if round.number % 4 == 0 {
		round.log("Not passing cards")
	} else {
		round.log("About to pass cards")
		cardsToPass := (*opts.doPassCards)(round)

		var newHeld []*AgentVsAgent.Card
		for i := 0; i < len(round.held); i++ {
			toRemove := false
			card := round.held[i]

			for j := 0; j < len(cardsToPass); j++ {
				if cardsToPass[j] == card {
					toRemove = true
				}
			}

			if !toRemove {
				newHeld = append(newHeld, card)
			}
		}

		receivedCards, ex, err := opts.client.PassCards(opts.ticket, cardsToPass)
		if err != nil { return err }
		if ex != nil { return errors.New((*ex).String()) }
		round.log("Received cards:", receivedCards)
		round.held = append(newHeld, receivedCards...)
	}
	return err
}

func (round *Round) playTrick(opts *options) (err error) {
	trick := round.createTrick()
	err = trick.run(opts)
	if err != nil { return err }

	if len(round.tricks) < 13 {
		err = round.playTrick(opts)
	}
	return err
}

func (round Round) log(message ...interface{}) {
	newMessage := append([]interface{}{"R:", round.number}, message...)
	round.game.log(newMessage...)
}

type Game struct {
	rounds []Round
	info *AgentVsAgent.GameInfo
}

func (game *Game) createRound() Round {
	round := Round{number: len(game.rounds) + 1, game: game}
	game.rounds = append(game.rounds, round)
	return round
}

func (game Game) run(opts *options) (err error) {
	game.log("Starting game")

	round := game.createRound()

	err = round.run(opts)
	if err != nil { return err }

	roundResult, ex, err := opts.client.GetRoundResult(opts.ticket)
	if err != nil { return err }
	if ex != nil { return errors.New((*ex).String()) }

	game.log("round result:", roundResult)
	if roundResult.Status == AgentVsAgent.GameStatus_NEXT_ROUND {
		err = game.run(opts)
	}
	// get round result
	//if status not next round
	return err
}

func (game Game) log(message ...interface{}) {
	newMessage := append([]interface{}{"P:", game.info.Position}, message...)
	fmt.Println(newMessage...)
}

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
		gameInfo, _, _ := client.GetGameInfo(ticket)
		fmt.Println("game info:", gameInfo)

		game := Game{info: gameInfo}
		err = game.run(&options{ticket: ticket, client: client, doPassCards: &doPassCards, doPlayCard: &doPlayCard})
		if err != nil {
			fmt.Println("ERROR:", err)
			return
		}
		fmt.Println("Game is over")
		gameResult, _, _ := client.GetGameResult(ticket)
		fmt.Println("game result:", gameResult)
	} else {
		fmt.Println("No ticket")
		return
	}
}
