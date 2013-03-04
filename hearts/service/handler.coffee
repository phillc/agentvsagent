Suit = require '../game/suit'
Rank = require '../game/rank'
actions = require '../game/actions'
types = require '../lib/hearts_types'

mapSuitToThrift = (suit) ->
  switch suit
    when Suit.CLUBS then types.Suit.CLUBS
    when Suit.DIAMONDS then types.Suit.DIAMONDS
    when Suit.SPADES then types.Suit.SPADES
    when Suit.HEARTS then types.Suit.HEARTS

mapRankToThrift = (rank) ->
  switch rank
    when Rank.TWO then types.Rank.TWO
    when Rank.THREE then types.Rank.THREE
    when Rank.FOUR then types.Rank.FOUR
    when Rank.FIVE then types.Rank.FIVE
    when Rank.SIX then types.Rank.SIX
    when Rank.SEVEN then types.Rank.SEVEN
    when Rank.EIGHT then types.Rank.EIGHT
    when Rank.NINE then types.Rank.NINE
    when Rank.TEN then types.Rank.TEN
    when Rank.JACK then types.Rank.JACK
    when Rank.QUEEN then types.Rank.QUEEN
    when Rank.KING then types.Rank.KING
    when Rank.ACE then types.Rank.ACE

mapCardToThrift = (card) ->
  new types.Card(suit: mapSuitToThrift(card.suit), rank: mapRankToThrift(card.rank))

module.exports = class Handler
  constructor: (@arena) ->

  enter_arena: (result) ->
    player = @arena.createPlayer()
    player.waitForGame (gameId) =>
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      #player disconnected before response returned
      #if this were updatred in npm, would be fine
      # so either need to wrap in a try, or somehow get that newest version
      result null, response

  get_game_info: (ticket, result) ->
    position = types.Position[Object.keys(types.Position)[0]]
    gameInfo = new types.GameInfo(position: position)

    result null, gameInfo

  get_hand: (ticket, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    player.waitForHand (cards) =>
      thriftCards = cards.map (card) ->
        mapCardToThrift(card)
      result null, thriftCards

  pass_cards: (ticket, cards, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    #CARD NEEDS TO BE MAPPED
    action = new actions.PassCards(player, cards)
    game.handleAction(action)

    player.waitForPassed (cards) =>
      result null, cards



