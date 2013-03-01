types = require '../lib/hearts_types'

module.exports = class Handler
  constructor: (@arena) ->

  enter_arena: (result) ->
    player = @arena.createPlayer()
    player.waitForGame (gameId) =>
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      #player disconnected before response returned
      #if this were updatred in npm, would be fine
      result null, response

  get_game_info: (ticket, result) ->
    position = types.Position[Object.keys(types.Position)[0]]
    gameInfo = new types.GameInfo(position: position)

    result null, gameInfo

  get_hand: (ticket, result) ->
    @arena.getGame(ticket.gameId).getPlayer(ticket.playerId)

    result null, [new types.Card(rank: "3", suit: types.Suit.CLUBS)]
