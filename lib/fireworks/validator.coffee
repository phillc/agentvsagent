Joi = require 'joi'

module.exports = class Validator
  constructor: ->
    @schemas =
      ready: Joi.object().keys({})
      move:
        Joi.object().keys({
          hint: Joi.object()
          discard: Joi.number().integer()
        }).xor("hint", "discard")

  validate: (message, data, callback) ->
    schema = @schemas[message]
    if schema
      schema.validate data, (args...) ->
        callback(args...)
    else
      callback "Unknown message, #{message}"
