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

      forward: (message, data, request) ->
        request.reject(new Error("unexpectedMessage"))

    waitingForClient:
      forward: (message, data, @request)->
        @emit "message", message, data
        @transition "waitingForServer"

module.exports = class Player
  constructor: ->
    @id = IdGenerator.generate()
    @state = new PlayerState()
    @on "transition", (details) ->
      # console.log "player changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "player changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"

  forward: (message, data) ->
    request = Q.defer()
    @state.handle "forward", message, data, request
    request.promise

  send: (message, data) ->
    @state.handle "send", message, data

  on: (event, callback) ->
    @state.on event, callback
