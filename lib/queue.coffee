{EventEmitter} = require 'events'

module.exports = class Queue extends EventEmitter
  constructor: (@events) ->
    @messages = []
    @events.forEach (event) =>
      name = event.charAt(0).toUpperCase() + event.slice(1)
      @["send#{name}"] = (args...) ->
        @messages.push [event, args]
        @emit "newMessage"

      @["recv#{name}"] = (callback) ->
        if @messages.length > 0
          @process event, callback
        else
          @once "newMessage", =>
            @process event, callback

  process: (event, callback) ->
    if @messages[0][0] == "error"
      callback @messages[0][1], null
    else if @messages[0][0] == event
      callback null, @messages.shift()[1]...
    else
      callback {type: "outOfSequence", message: "Method call out of sequence"}, null

