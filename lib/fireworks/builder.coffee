FireworksGame = require './game'

module.exports = class FireworksBuilder
  minAgents: 2
  maxAgents: 5
  agentEvents: []
  events: []

  constructor: (@gameOptions={}) ->

  positions: (number) ->
    FireworksGame.positions().slice(0, number)

  createGame: (positions) ->
    new FireworksGame({})

