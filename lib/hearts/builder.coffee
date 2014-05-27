HeartsGame = require './game'

module.exports = class HeartsBuilder
  minAgents: 4
  maxAgents: 4
  agentEvents: ["readyForRound", "passCards", "readyForTrick", "playCard", "finishedRound", "finishedGame"]
  events: ["roundStarted", "dealt", "received", "turn", "finishedTrick", "roundFinished"]

  constructor: (@gameOptions={}) ->

  positions: ->
    HeartsGame.positions()

  createGame: ->
    new HeartsGame({heartsMaxPoints: @gameOptions.heartsMaxPoints})

