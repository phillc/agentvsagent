{EventEmitter} = require 'events'
logger = require '../logger'

module.exports = class Game
  @positions = -> ["player1", "player2", "player3", "player4", "player5"]

  constructor: ({@positions}) ->
    @emitter = new EventEmitter()

  on: (args...) ->
    @emitter.on(args...)

  start: ->
    logger.info "Starting a game of fireworks"
