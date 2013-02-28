namespace rb AgentVsAgent

enum Suit {
  CLUBS = 21,
  DIAMONDS = 22,
  SPADES = 23,
  HEARTS = 24
}

enum Rank {
  TWO = 2,
  THREE = 3,
  FOUR = 4,
  FIVE = 5,
  SIZE = 6,
  SEVEN = 7,
  EIGHT = 8,
  NINE = 9,
  TEN = 10,
  JACK = 11,
  QUEEN = 12,
  KING = 13,
  ACE = 1
}

struct Card {
  1: required Suit suit,
  2: required Rank rank
}

struct Agent {
  1: required string token
}

service Hearts {
  Agent start_agent(),
  list<Card> get_hand(1: Agent agent),
  #bool play_card(1: Agent, 2:Card card),
  #list<Card> get_trick(1: Agent)
}

