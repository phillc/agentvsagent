logger = require './logger'

module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    logger.info "#{@arena.waitingRoom.length} players waiting."
    if @arena.waitingRoom.length >= 4
      logger.info "Match maker is creating a game!"
      @arena.createGame @arena.waitingRoom[0..3]

  start: ->
    @arena.on 'newPlayer', =>
      @findMatch()

