types = require '../lib/hearts_types'

module.exports = class Handler
  constructor: (@arena) ->

  enter_arena: (result) ->
    player = @arena.createPlayer()
    console.log "get_game called"
    result null, new types.Agent(agentId: player.id, gameId: "12345")
  get_hand: (agent, result) ->
    # player = @matchMaker.players[agent.token]
    # player.get_hand result
    console.log "nada"
    result null, [new types.Card(rank: "3", suit: types.Suit.CLUBS)]
