namespace rb AgentVsAgent
namespace java AgentVsAgent

enum Suit {
  CLUBS = 21,
  DIAMONDS = 22,
  SPADES = 23,
  HEARTS = 24
}

enum Rank {
  ACE = 1,
  TWO = 2,
  THREE = 3,
  FOUR = 4,
  FIVE = 5,
  SIX = 6,
  SEVEN = 7,
  EIGHT = 8,
  NINE = 9,
  TEN = 10,
  JACK = 11,
  QUEEN = 12,
  KING = 13
}

enum Position {
  NORTH = 1,
  EAST = 2,
  SOUTH = 3,
  WEST = 4
}

struct Card {
  1: required Suit suit,
  2: required Rank rank
}

struct Ticket {
  1: required string gameId,
  2: required string agentId
}

struct EntryResponse {
  1: optional Ticket ticket,
  2: optional string message
}

struct GameInfo {
  1: required Position position
}

struct Trick {
  1: required Position leader
  2: required list<Card> played
}

typedef i32 Score

enum GameStatus {
  NEXT_ROUND = 1,
  END_GAME = 2
}

struct RoundResult {
  1: required Score north
  2: required Score east
  3: required Score south
  4: required Score west
  5: required GameStatus status
}

struct GameResult {
  1: required Score north
  2: required Score east
  3: required Score south
  4: required Score west
}

service Hearts {
  # These may need a wrapper around return values, to indicate things like
  # game ended (in middle of trick... maybe someone played an invalid move)
  EntryResponse enter_arena(),
  GameInfo get_game_info(1: required Ticket ticket),
  list<Card> get_hand(1: required Ticket ticket),
  list<Card> pass_cards(1: required Ticket ticket, 2: required list<Card> cards),
  Trick get_trick(1: required Ticket ticket),
  Trick play_card(1: required Ticket ticket, 2: required Card card),
  RoundResult get_round_result(1: required Ticket ticket),
  GameResult get_game_result(1: required Ticket ticket)
}

