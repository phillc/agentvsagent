uuid = require 'node-uuid'

module.exports =
  generate: ->
    uuid.v4()

