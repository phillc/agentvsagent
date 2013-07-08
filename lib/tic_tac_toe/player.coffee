machina = require('machina')()
Q = require 'q'

IdGenerator = require '../idGenerator'
logger = require '../logger'

PlayerState = machina.Fsm.extend
  initialState: "waitingForClient"
  states:
    waitingForServer:
      send: (message, data) ->
        @request.resolve(message: message, data: data || {})

      forward: (args..., request) ->
        request.reject(new Error("unexpectedMessage"))

    waitingForClient:
      forward: (args..., @request)->
        @transition "waitingForServer"

module.exports = class Player
  constructor: ->
    @id = IdGenerator.generate()
    @state = new PlayerState()
    @state.on "transition", (details) ->
      # console.log "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"

  forward: (args...) ->
    request = Q.defer()
    @state.handle "forward", args..., request
    request.promise

  send: (args...) ->
    @state.handle "send", args...

