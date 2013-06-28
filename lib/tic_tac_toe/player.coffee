IdGenerator = require '../idGenerator'
Queue = require '../queue'

module.exports = class Player
  constructor: ->
    @id = IdGenerator.generate()

    @out = new Queue(['startedGame', 'gameInfo'])

