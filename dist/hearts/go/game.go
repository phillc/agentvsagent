package main

import (
  "fmt"
	"errors"
	"encoding/json"
	"bufio"
	"os"
)

type Json map[string]interface{}

var stdin = bufio.NewReader(os.Stdin)

func clientRead() Json {
	line, err := stdin.ReadString('\n')
	line = line[:len(line)-1] //remove the delimiter

	res := &Json{}
	response := json.Unmarshal([]byte(line), &res)
	return *res
}

func clientSendAndReceive(whatever string) Json {
	return clientRead()
}

type passCardFn func(Round) []*Card
type playCardFn func(Trick) *Card

type options struct {
	doPassCards *passCardFn
	doPlayCard *playCardFn
}

type Card struct {
	suit string
	rank string
}

type Trick struct {
	number int
	round *Round
	leader string
	played []*Card
}

func (trick *Trick) run(opts *options) (err error) {
	trick.log("Starting trick")
	currentTrick := clientSendAndReceive("readyForTrick")
	trick.leader = string(currentTrick["Leader"])
	trick.played = currentTrick["Played"]

	cardToPlay := (*opts.doPlayCard)(*trick)

	var remainingCards []*Card
	for _, heldCard := range trick.round.held {
		if !(heldCard.Suit == cardToPlay.Suit && heldCard.Rank == cardToPlay.Rank) {
			remainingCards = append(remainingCards, heldCard)
		}
	}
	trick.round.held = remainingCards

	trickResult := clientSendAndReceive("playCard", cardToPlay)

	trick.log("trick: result", trickResult)
	trick.played = trickResult["Played"]
	return
}

func (trick *Trick) log(message ...interface{}) {
	newMessage := append([]interface{}{"T:", trick.number}, message...)
	trick.round.log(newMessage...)
}

type Round struct {
	number int
	tricks []*Trick
	dealt []*Card
	passed []*Card
	received []*Card
	held []*Card
	game *Game
}

func (round *Round) createTrick() *Trick {
	trick := Trick{number: len(round.tricks) + 1, round: round}
	round.tricks = append(round.tricks, &trick)
	return &trick
}

func (round *Round) run(opts *options) (err error) {
	hand := clientSendAndReceive("readyForRound")
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
		cardsToPass := (*opts.doPassCards)(*round)

		var newHeld []*Card
		for _, heldCard := range round.held {
			toRemove := false

			for _, cardToPass := range cardsToPass {
				if cardToPass.Suit == heldCard.Suit && cardToPass.Rank == heldCard.Rank {
					toRemove = true
				}
			}

			if !toRemove {
				newHeld = append(newHeld, heldCard)
			}
		}

		receivedCards, ex, err := opts.client.PassCards(opts.ticket, cardsToPass)
		if err != nil { return err }
		if ex != nil { return errors.New((*ex).String()) }
		round.log("Received cards:", receivedCards)
		round.held = append(newHeld, receivedCards...)
		round.passed = cardsToPass
		round.received = receivedCards
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

func (round *Round) log(message ...interface{}) {
	newMessage := append([]interface{}{"R:", round.number}, message...)
	round.game.log(newMessage...)
}

type Game struct {
	rounds []*Round
	info *string
}

func (game *Game) createRound() *Round {
	round := Round{number: len(game.rounds) + 1, game: game}
	game.rounds = append(game.rounds, &round)
	return &round
}

func (game *Game) run(opts *options) (err error) {
	game.log("Starting game")

	round := game.createRound()

	err = round.run(opts)
	if err != nil { return err }

	roundResult, ex, err := opts.client.GetRoundResult(opts.ticket)
	if err != nil { return err }
	if ex != nil { return errors.New((*ex).String()) }

	game.log("round result:", roundResult)
	if roundResult.Status == "nextRound" {
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
	fmt.Println("playing")
	gameInfo := clientRead()
	fmt.Println("game info:", gameInfo)

	game := Game{info: gameInfo}
	err = game.run(&options{doPassCards: &doPassCards, doPlayCard: &doPlayCard})
	if err != nil {
		fmt.Println("ERROR:", err)
		return
	}
	fmt.Println("Game is over")
	gameResult, _, _ := client.GetGameResult(ticket)
	fmt.Println("game result:", gameResult)
}
