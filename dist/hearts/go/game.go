package main

import (
  "fmt"
	"encoding/json"
	"bufio"
	"os"
)

func log(message ...interface{}) {
	fmt.Fprintln(os.Stderr, message...)
}

var stdin = bufio.NewReader(os.Stdin)

func clientRead() (string, map[string]interface{}) {
	line, _ := stdin.ReadString('\n')
	line = line[:len(line)-1] //remove the delimiter

	res := &map[string]interface{}{}
	json.Unmarshal([]byte(line), res)
	data := (*res)["data"].(map[string]interface{})
	return (*res)["message"].(string), data
}

func clientSendAndReceive(message string, data map[string]interface{}) (string, map[string]interface{}) {
	sending := make(map[string]interface{})
	if data != nil {
		sending = data
	}
	req, _ := json.Marshal(map[string]interface{}{ "message": message, "data": sending})
	fmt.Fprintln(os.Stdout, string(req))
	responseMessage, responseData := clientRead()
	return responseMessage, responseData
}

type passCardFn func(Round) []*Card
type playCardFn func(Trick) *Card

type options struct {
	doPassCards *passCardFn
	doPlayCard *playCardFn
}

type Card struct {
	Suit string `json:"suit"`
	Rank string `json:"rank"`
}

func responseToCards(response interface{}) []*Card {
	/*round.log("lets see:", responseToCard(hand.([]interface{})[0]))*/
	var cards []*Card
	for _, res := range response.([]interface{}) {
		newCard := responseToCard(res)
		cards = append(cards, &newCard)
	}

	return cards
}

func responseToCard(response interface{}) Card {
	card := response.(map[string]interface{})
	return Card{ Suit: card["suit"].(string), Rank: card["rank"].(string)}
}

type Trick struct {
	number int
	round *Round
	leader string
	played []*Card
}

func (trick *Trick) run(opts *options) (err error) {
	trick.log("Starting trick")
	_, currentTrick := clientSendAndReceive("readyForTrick", nil)
	trick.leader = currentTrick["leader"].(string)
	trick.played = responseToCards(currentTrick["played"])

	cardToPlay := (*opts.doPlayCard)(*trick)

	var remainingCards []*Card
	for _, heldCard := range trick.round.held {
		if !(heldCard.Suit == cardToPlay.Suit && heldCard.Rank == cardToPlay.Rank) {
			remainingCards = append(remainingCards, heldCard)
		}
	}
	trick.round.held = remainingCards

	_, trickResult := clientSendAndReceive("playCard", map[string]interface{}{ "card": cardToPlay })

	trick.log("trick: result", trickResult)
	trick.played = responseToCards(trickResult["played"])
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
	_, response := clientSendAndReceive("readyForRound", nil)
	hand := responseToCards(response["cards"])
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

		_, response := clientSendAndReceive("passCards", map[string]interface{}{ "cards": cardsToPass})
		receivedCards := responseToCards(response["cards"])
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
	info map[string]interface{}
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

	_, roundResult := clientSendAndReceive("finishedRound", nil)

	game.log("round result:", roundResult)
	if roundResult["status"] == "nextRound" {
		err = game.run(opts)
	}
	// get round result
	//if status not next round
	return err
}

func (game Game) log(message ...interface{}) {
	newMessage := append([]interface{}{"P:", game.info["position"]}, message...)
	log(newMessage...)
}

func play(doPassCards passCardFn, doPlayCard playCardFn) {
	log("playing")
	_, gameInfo := clientRead()
	log("game info:", gameInfo)

	game := Game{info: gameInfo}
	game.run(&options{doPassCards: &doPassCards, doPlayCard: &doPlayCard})
	log("Game is over")
  _, gameResult := clientSendAndReceive("finishedGame", nil)
	log("game result:", gameResult)
}

const HEARTS = "hearts"
const CLUBS = "clubs"
const SPADES = "spades"
const DIAMONDS = "diamonds"
const TWO = "two"
const THREE = "three"
const FOUR = "four"
const FIVE = "five"
const SIX = "six"
const SEVEN = "seven"
const EIGHT = "eight"
const NINE = "nine"
const TEN = "ten"
const JACK = "jack"
const QUEEN = "queen"
const KING = "king"
const ACE = "ace"

