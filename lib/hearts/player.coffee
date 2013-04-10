{EventEmitter} = require 'events'
IdGenerator = require '../idgenerator'
Pile = require "./engine/Pile"

module.exports = class Player extends EventEmitter
  @events = ['startedGame', 'dealt', 'passed', 'turn', 'endTrick', 'endRound', 'endGame']

  constructor: ->
    @id = IdGenerator.generate()

    @messages = []
    Player.events.forEach (event) =>
      name = event.charAt(0).toUpperCase() + event.slice(1)
      @["send#{name}"] = (args...) ->
        @messages.push [event, args]
        @emit "newMessage", event

      @["recv#{name}"] = (callback) ->
        if @messages.length > 0
          @process event, callback
        else
          @once "newMessage", =>
            @process event, callback

  process: (event, callback) ->
    if @messages[0][0] == event
      callback null, @messages.shift()[1]...
    else
      callback "outOfSequence", null

