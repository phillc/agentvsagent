struct Card {
  1: required string suit,
  2: required string rank
}

service Hearts {
  bool get_game(),
  #list<Card> get_hand(),
  bool play_card(1:Card card)
  #list<Card> get_trick()
}

