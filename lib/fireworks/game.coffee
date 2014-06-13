{EventEmitter} = require 'events'
logger = require '../logger'

module.exports = class Game
  @positions = -> ["player 1", "player 2", "player 3", "player 4", "player 5"]

  constructor: (options={}) ->
    @emitter = new EventEmitter()

  on: (args...) ->
    @emitter.on(args...)

  start: ->
    logger.info "Starting a game of fireworks"
