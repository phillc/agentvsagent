types = require '../lib/hearts_types'

module.exports = class Handler
  constructor: (@arena) ->

  enter_arena: (result) ->
    console.log "get_game called"

    player = @arena.createPlayer()
    player.waitForGame (gameId) =>
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      #player disconnected before response returned
      #if this were updatred in npm, would be fine
      result null, response

  get_hand: (agent, result) ->
    # player = @matchMaker.players[agent.token]
    # player.get_hand result
    console.log "nada"
    result null, [new types.Card(rank: "3", suit: types.Suit.CLUBS)]
