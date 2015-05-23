logger = require './logger'
module.exports = class AbstractGame
  @buildPositionHandlers = (eventName, builder, props) ->
    for position in @availablePositions()
      do (position) ->
        props["#{eventName}.#{position}"] = (data) -> builder.bind(this)(position, data)

    props

  on: (args...) ->
    @emitter.on(args...)

  emitAll: (message, data) ->
    for position in @positions()
      do (position, message, data) =>
        @emitPosition(position, message, data)

  emitPosition: (position, message, data) ->
    logger.verbose "GAME emitting to #{position} - #{message} with #{data}"
    heard = @emitter.emit([position, message].join("."), data)
    if !heard
      logger.error "no one was listening to #{message}"

  handle: (args...) ->
    @engine.handle(args...)

