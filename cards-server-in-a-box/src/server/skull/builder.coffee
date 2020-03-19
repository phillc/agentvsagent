SkullGame = require './game'
Validator = require './validator'

module.exports = class SkullBuilder
  minAgents: 3
  maxAgents: 5
  agentEvents: ["readyForRound"]
  events: ["roundStarted"]

  constructor: (@gameOptions={}) ->

  positions: ->
    SkullGame.availablePositions()

  createGame: ->
    new SkullGame()

  validator: ->
    new Validator()
