package main

import (
	"./lib/AgentVsAgent"
)

func isLeadingTrick(trick *Trick) bool {
	return len(trick.played) == 0
}

func isHeartsBroken(trick *Trick) bool {
	broken := false
	for _, roundTrick := range trick.round.tricks {
		cards := roundTrick.played
		for _, card := range cards {
			if card.Suit == AgentVsAgent.Suit_HEARTS {
				broken = true
			}
		}
	}
	return broken
}

func onlyTwoClubs(cards []*AgentVsAgent.Card) []*AgentVsAgent.Card {
	var matchedCards []*AgentVsAgent.Card
	for _, card := range cards {
		if card.Suit == AgentVsAgent.Suit_CLUBS && card.Rank == AgentVsAgent.Rank_TWO {
			matchedCards = append(matchedCards, card)
		}
	}
	return matchedCards
}

func noHearts(cards []*AgentVsAgent.Card) []*AgentVsAgent.Card {
	var matchedCards []*AgentVsAgent.Card
	for _, card := range cards {
		if card.Suit != AgentVsAgent.Suit_HEARTS {
			matchedCards = append(matchedCards, card)
		}
	}
	return matchedCards
}

func noPoints(allCards []*AgentVsAgent.Card) []*AgentVsAgent.Card {
	var matchedCards []*AgentVsAgent.Card
	cards := noHearts(allCards)
	for _, card := range cards {
		if !(card.Suit == AgentVsAgent.Suit_SPADES && card.Rank == AgentVsAgent.Rank_QUEEN) {
			matchedCards = append(matchedCards, card)
		}
	}
	return matchedCards
}

func followSuit(cards []*AgentVsAgent.Card, trick *Trick) []*AgentVsAgent.Card {
	var matchedCards []*AgentVsAgent.Card
	suit := trick.played[0].Suit
	for _, card := range cards {
		if card.Suit == suit {
			matchedCards = append(matchedCards, card)
		}
	}
	if len(matchedCards) == 0 {
		matchedCards = cards
	}
	return matchedCards
}

func playableCards(trick *Trick) []*AgentVsAgent.Card {
	validCards := trick.round.held

	if trick.number == 1 && isLeadingTrick(trick) {
		validCards = onlyTwoClubs(validCards)
	}

	if trick.number == 1 {
		validCards = noPoints(validCards)
	}

	if isLeadingTrick(trick) && !isHeartsBroken(trick) && len(noHearts(trick.round.held)) > 0 {
		validCards = noHearts(validCards)
	}

	if !isLeadingTrick(trick) {
		validCards = followSuit(validCards, trick)
	}

	trick.log("Valid cards:", validCards)
	return validCards
}

func doPassCards(round Round) []*AgentVsAgent.Card {
	cardsToPass := round.dealt[0:3]
	round.log("Passing cards", cardsToPass)

	return cardsToPass
}

func doPlayCard(trick Trick) *AgentVsAgent.Card {
	trick.log("Current trick:", trick)
	cardToPlay := playableCards(&trick)[0]
	trick.log("Playing card:", cardToPlay)
	return cardToPlay
}

func main() {
	play(doPassCards, doPlayCard)
}
