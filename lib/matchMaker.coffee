logger = require './logger'

module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    logger.verbose "#{@arena.waitingRoom.length} players waiting."
    if @arena.waitingRoom.length >= 4
      logger.verbose "Match maker is creating a game!"
      @arena.createGame @arena.waitingRoom[0..3]

  start: ->
    @arena.on 'newPlayer', =>
      @findMatch()

