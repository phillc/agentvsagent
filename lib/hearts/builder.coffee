HeartsGame = require './game'

module.exports = class HeartsBuilder
  minPlayers: 4
  maxPlayers: 4
  agentEvents: ["readyForRound", "passCards", "readyForTrick", "playCard", "finishedRound", "finishedGame"]
  events: ["roundStarted", "dealt", "received", "turn", "finishedTrick", "roundFinished"]

  constructor: (@gameOptions={}) ->

  positions: ->
    HeartsGame.positions()

  createGame: ->
    new HeartsGame({heartsMaxPoints: @gameOptions.heartsMaxPoints})

