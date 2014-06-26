FireworksGame = require './game'
Validator = require './validator'

module.exports = class FireworksBuilder
  minAgents: 2
  maxAgents: 5
  agentEvents: ["ready", "move"]
  events: ["starting", "turn"]

  constructor: (@gameOptions={}) ->

  positions: (number) ->
    FireworksGame.availablePositions().slice(0, number)

  createGame: (positions) ->
    new FireworksGame(positions: positions)

  validator: ->
    new Validator()
