uuid = require 'node-uuid'

module.exports = class IdGenerator
  generate: ->
    uuid.v4()

