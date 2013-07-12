machina = require('machina')()
Q = require 'q'

IdGenerator = require '../idGenerator'
logger = require '../logger'

PlayerState = machina.Fsm.extend
  initialState: "waitingForClient"
  "*": ->
    console.log "=( =( unexpected message =( =( =(", arguments
  states:
    waitingForServer:
      send: (message, data) ->
        @request.resolve(message: message, data: data || {})
        @transition "waitingForClient"

      forward: (message, data, request) ->
        request.reject(new Error("unexpectedMessage"))

    waitingForClient:
      forward: (message, data, @request)->
        console.log("@state", @state)
        @transition "waitingForServer"
        @emit "message", message, data
        console.log("@state2", @state)

module.exports = class Player
  constructor: ->
    @id = IdGenerator.generate()
    @state = new PlayerState()
    @on "transition", (details) ->
      # console.log "player changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "player changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"

  forward: (message, data) ->
    logger.verbose "forwarding #{message}, #{data}"
    request = Q.defer()
    @state.handle "forward", message, data, request
    request.promise

  send: (message, data) ->
    logger.verbose "sending", message, data
    @state.handle "send", message, data

  on: (event, callback) ->
    @state.on event, callback
