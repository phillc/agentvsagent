package main

import (
	"./lib/AgentVsAgent"
)

func isLeadingTrick(trick *Trick) bool {
	return len(trick.played) == 0
}

func isHeartsBroken(trick *Trick) bool {
	return true
}

func onlyTwoClubs(cards []*AgentVsAgent.Card) []*AgentVsAgent.Card {
	var matchedCards []*AgentVsAgent.Card
	for i := 0; i < len(cards); i++ {
		if cards[i].Suit == AgentVsAgent.Suit_CLUBS && cards[i].Rank == AgentVsAgent.Rank_TWO {
			matchedCards = append(matchedCards, cards[i])
		}
	}
	return matchedCards
}

func playableCards(trick *Trick) []*AgentVsAgent.Card {
	validCards := trick.round.held

	if trick.number == 1 && isLeadingTrick(trick) {
		validCards = onlyTwoClubs(validCards)
	}
	trick.log("Valid cards:", validCards)
	return validCards
}

func doPassCards(round *Round) []*AgentVsAgent.Card {
	cardsToPass := round.dealt[0:3]
	round.log("Passing cards", cardsToPass)

	return cardsToPass
}

func doPlayCard(trick *Trick) *AgentVsAgent.Card {
	trick.log("Current trick:", trick)
	cardToPlay := playableCards(trick)[0]
	trick.log("Playing card:", cardToPlay)
	return cardToPlay
}

func main() {
	play(doPassCards, doPlayCard)
}
