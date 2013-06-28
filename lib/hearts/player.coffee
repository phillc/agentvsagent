IdGenerator = require '../idGenerator'
Queue = require '../queue'

module.exports = class Player
  constructor: ->
    @id = IdGenerator.generate()

    @out = new Queue(['startedGame', 'dealt', 'passed', 'turn', 'endTrick', 'endRound', 'endGame'])

  raiseError: (error) ->
    @out.messages.splice(0, @out.messages.length)
    @out.messages.push(["error", error])
    @out.emit "newMessage"

