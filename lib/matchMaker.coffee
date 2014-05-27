logger = require './logger'

module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    waiting = @arena.waitingRoom.length
    if waiting >= @arena.builder.maxAgents
      @createGame()
    else if waiting >= @arena.builder.minAgents
      @delayCreate = setTimeout =>
        @createGame()
      , 100
    else
      logger.verbose "Match maker did not find a game. #{waiting} waiting, need #{@arena.numberOfAgents}"

  start: ->
    @arena.on 'agentJoined', =>
      @findMatch()

  createGame: ->
    if @delayCreate
      clearTimeout(@delayCreate)

    logger.verbose "Match maker is creating a game!"
    @arena.createGame @arena.waitingRoom[0..(@arena.builder.maxAgents - 1)]
