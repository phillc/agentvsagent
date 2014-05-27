FireworksGame = require './game'

module.exports = class FireworksBuilder
  minAgents: 2
  maxAgents: 5
  agentEvents: []
  events: []

  constructor: (@gameOptions={}) ->

  positions: ->
    FireworksGame.positions()

  createGame: ->
    new FireworksGame({})

