logger = require './logger'

module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    waiting = @arena.waitingRoom.length
    if waiting >= @arena.numberOfAgents
      logger.verbose "Match maker is creating a game!"
      @arena.createGame @arena.waitingRoom[0..(@arena.numberOfAgents - 1)]
    else
      logger.verbose "Match maker did not find a game. #{waiting} waiting, need #{@arena.numberOfAgents}"

  start: ->
    @arena.on 'agentJoined', =>
      @findMatch()

