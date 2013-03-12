{EventEmitter} = require 'events'
IdGenerator = require './idgenerator'
Pile = require "./game/Pile"

module.exports = class Player extends EventEmitter
  @events = ['startedGame', 'dealt', 'passed', 'turn', 'endTrick', 'endRound', 'endGame']

  constructor: ->
    @id = IdGenerator.generate()

    # TODO: Make this one queue, so that a game ended suddenly
    # message can be put on the queue for any listener to pick up
    Player.events.forEach (event) =>
      @["#{event}Messages"] = []
      name = event.charAt(0).toUpperCase() + event.slice(1)
      @["send#{name}"] = (args...) ->
        @["#{event}Messages"].push [event, args]
        @emit "message#{name}", event

      @["recv#{name}"] = (callback) ->
        #TODO: What if message is unexpected?
        if @["#{event}Messages"].length > 0
          callback @["#{event}Messages"].shift()[1]...
        else
          @once "message#{name}", ->
            callback @["#{event}Messages"].shift()[1]...


