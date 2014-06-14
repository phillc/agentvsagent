FireworksGame = require './game'

module.exports = class FireworksBuilder
  minAgents: 2
  maxAgents: 5
  agentEvents: ["ready"]
  events: ["starting", "turn"]

  constructor: (@gameOptions={}) ->

  positions: (number) ->
    FireworksGame.availablePositions().slice(0, number)

  createGame: (positions) ->
    new FireworksGame(positions: positions)

