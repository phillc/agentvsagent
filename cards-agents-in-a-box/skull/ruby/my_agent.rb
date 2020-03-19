require "./game"

def leading_trick?(trick)
  trick.played.size == 0
end

def hearts_broken?(trick)
  trick.round.tricks.any?{|trick| trick.played.any?{|card| card.suit == Game::Suit::HEARTS}}
end

def only_two_clubs(cards)
  cards.select{|card| card.suit == Game::Suit::CLUBS && card.rank == Game::Rank::TWO }
end

def no_hearts(cards)
  cards.reject{|card| card.suit == Game::Suit::HEARTS}
end

def no_points(cards)
  no_hearts(cards).reject{|card| card.suit == Game::Suit::SPADES && Game::Rank::QUEEN}
end

def follow_suit(cards, trick)
  suit = trick.played[0].suit
  matching = cards.select{|card| card.suit == trick.played[0].suit}
  if matching.size > 0
    matching
  else
    cards
  end
end

def playable_cards(trick)
  valid_cards = trick.round.held.dup

  valid_cards = only_two_clubs(valid_cards) if trick.number == 1 && leading_trick?(trick)
  valid_cards = no_points(valid_cards) if trick.number == 1
  valid_cards = no_hearts(valid_cards) if leading_trick?(trick) && !hearts_broken?(trick) && no_hearts(trick.round.held).size > 0
  valid_cards = follow_suit(valid_cards, trick) if !leading_trick?(trick)

  trick.log "Valid cards: #{valid_cards}"
  valid_cards
end

Game.do_pass_cards do |round|
  cards_to_pass = round.dealt[0..2]
  round.log "Passing cards #{cards_to_pass.inspect}"

  cards_to_pass
end

Game.do_play_card do |trick|
  trick.log "Current trick: #{trick.played.inspect}"
  card_to_play = playable_cards(trick)[0]
  trick.log "Playing card: #{card_to_play}"

  card_to_play
end

Game.play

