logger = require './logger'

module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    if @arena.waitingRoom.length >= @arena.numberOfPlayers
      logger.verbose "Match maker is creating a game!"
      @arena.createGame @arena.waitingRoom[0..(@arena.numberOfPlayers - 1)]

  start: ->
    @arena.on 'newPlayer', =>
      @findMatch()

