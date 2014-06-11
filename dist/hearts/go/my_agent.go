package main

func isLeadingTrick(trick *Trick) bool {
	return len(trick.played) == 0
}

func isHeartsBroken(trick *Trick) bool {
	broken := false
	for _, roundTrick := range trick.round.tricks {
		cards := roundTrick.played
		for _, card := range cards {
			if card.suit == HEARTS {
				broken = true
			}
		}
	}
	return broken
}

func onlyTwoClubs(cards []*Card) []*Card {
	var matchedCards []*Card
	for _, card := range cards {
		if card.suit == CLUBS && card.Rank == TWO {
			matchedCards = append(matchedCards, card)
		}
	}
	return matchedCards
}

func noHearts(cards []*Card) []*Card {
	var matchedCards []*Card
	for _, card := range cards {
		if card.suit != HEARTS {
			matchedCards = append(matchedCards, card)
		}
	}
	return matchedCards
}

func noPoints(allCards []*Card) []*Card {
	var matchedCards []*Card
	cards := noHearts(allCards)
	for _, card := range cards {
		if !(card.suit == SPADES && card.Rank == QUEEN) {
			matchedCards = append(matchedCards, card)
		}
	}
	return matchedCards
}

func followSuit(cards []*Card, trick *Trick) []*Card {
	var matchedCards []*Card
	suit := trick.played[0].suit
	for _, card := range cards {
		if card.suit == suit {
			matchedCards = append(matchedCards, card)
		}
	}
	if len(matchedCards) == 0 {
		matchedCards = cards
	}
	return matchedCards
}

func playableCards(trick *Trick) []*Card {
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

func doPassCards(round Round) []*Card {
	cardsToPass := round.dealt[0:3]
	round.log("Passing cards", cardsToPass)

	return cardsToPass
}

func doPlayCard(trick Trick) *Card {
	trick.log("Current trick:", trick)
	cardToPlay := playableCards(&trick)[0]
	trick.log("Playing card:", cardToPlay)
	return cardToPlay
}

func main() {
	play(doPassCards, doPlayCard)
}
