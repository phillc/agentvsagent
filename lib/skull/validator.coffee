Joi = require 'joi'

module.exports = class Validator
  constructor: ->
    @schemas =
      readyForRound: Joi.object().keys({})
      passCards:
        Joi.object().keys
          cards: Joi.array().length(3)
      readyForTrick: Joi.object().keys({})
      playCard:
        Joi.object().keys
          card: Joi.object()
      finishedRound: Joi.object().keys({})
      finishedGame: Joi.object().keys({})

  validate: (message, data, callback) ->
    schema = @schemas[message]
    if schema
      schema.validate data, (args...) ->
        callback(args...)
    else
      callback "Unknown message, #{message}"

