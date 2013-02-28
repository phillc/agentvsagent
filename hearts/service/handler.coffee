types = require '../lib/hearts_types'
Player = require './player'

module.exports = class Handler
  constructor: (@matchMaker) ->

  start_agent: (result) ->
    player = new Player()
    playerId = @matchMaker.addPlayer player
    console.log "get_game called"
    result null, new types.Agent(token: playerId)
  get_hand: (agent, result) ->
    # player = @matchMaker.players[agent.token]
    # player.get_hand result
    console.log "nada"
    result null, [new types.Card(rank: "3", suit: types.Suit.CLUBS)]
