{EventEmitter} = require 'events'
IdGenerator = require '../idgenerator'

module.exports = class Player extends EventEmitter
  constructor: ->
    @id = IdGenerator.generate()

