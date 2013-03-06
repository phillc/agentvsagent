Suit = require '../game/suit'
Rank = require '../game/rank'
Card = require '../game/card'
actions = require '../game/actions'
types = require '../lib/hearts_types'

suitMapping = [
  [Suit.CLUBS, types.Suit.CLUBS]
  [Suit.DIAMONDS, types.Suit.DIAMONDS]
  [Suit.SPADES, types.Suit.SPADES]
  [Suit.HEARTS, types.Suit.HEARTS]
]

rankMapping = [
  [Rank.TWO, types.Rank.TWO]
  [Rank.THREE, types.Rank.THREE]
  [Rank.FOUR, types.Rank.FOUR]
  [Rank.FIVE, types.Rank.FIVE]
  [Rank.SIX, types.Rank.SIX]
  [Rank.SEVEN, types.Rank.SEVEN]
  [Rank.EIGHT, types.Rank.EIGHT]
  [Rank.NINE, types.Rank.NINE]
  [Rank.TEN, types.Rank.TEN]
  [Rank.JACK, types.Rank.JACK]
  [Rank.QUEEN, types.Rank.QUEEN]
  [Rank.KING, types.Rank.KING]
  [Rank.ACE, types.Rank.ACE]
]

mapSuitToThrift = (suit) ->
  for mapping in suitMapping
    return mapping[1] if suit == mapping[0]

mapThriftToSuit = (thriftSuit) ->
  for mapping in suitMapping
    return mapping[0] if thriftSuit == mapping[1]

mapRankToThrift = (rank) ->
  for mapping in rankMapping
    return mapping[1] if rank == mapping[0]

mapThriftToRank = (thriftRank) ->
  for mapping in rankMapping
    return mapping[0] if thriftRank == mapping[1]

mapCardToThrift = (card) ->
  new types.Card(suit: mapSuitToThrift(card.suit), rank: mapRankToThrift(card.rank))

mapThriftToCard = (thriftCard) ->
  new Card(mapThriftToSuit(thriftCard.suit), mapThriftToRank(thriftCard.rank))

mapPositionToThrift = (position) ->
  switch position
    when "north" then types.Position.NORTH
    when "east" then types.Position.EAST
    when "south" then types.Position.SOUTH
    when "west" then types.Position.WEST

mapTrickToThrift = (trick) ->
  cards = trick.played.cards.map mapCardToThrift
  new types.Trick leader: mapPositionToThrift(trick.leader), played: cards

module.exports = class Handler
  constructor: (@arena) ->

  enter_arena: (result) ->
    player = @arena.createPlayer()
    player.waitForGame (gameId) =>
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      #player disconnected before response returned
      #if this were updated in npm, would be fine
      # so either need to wrap in a try, or somehow get that newest version
      result null, response

  get_game_info: (ticket, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    thriftPosition = mapPositionToThrift(game.positionOf(player))
    gameInfo = new types.GameInfo(position: thriftPosition)

    result null, gameInfo

  get_hand: (ticket, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    player.waitForHand (cards) =>
      thriftCards = cards.map mapCardToThrift
      result null, thriftCards

  pass_cards: (ticket, cards, result) ->
    console.log "PASS CARDS", ticket, cards
    #What if they send a blank ticket?
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    mappedCards = cards.map mapThriftToCard
    action = new actions.PassCards(player, mappedCards)
    game.handleAction(action)

    player.waitForPassed (cards) =>
      thriftCards = cards.map mapCardToThrift
      result null, thriftCards

  get_trick: (ticket, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    player.waitForTurn (trick) =>
      result null, mapTrickToThrift(trick)

  play_card: (ticket, card, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    action = new actions.PlayCard(player, mapThriftToCard(card))
    game.handleAction(action)

    player.waitForTrickFinished (trick) =>
      result null, mapTrickToThrift(trick)

