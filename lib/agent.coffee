machina = require('machina')()
logger = require '../lib/logger'
{EventEmitter} = require 'events'

ConnectionState = machina.Fsm.extend
  initialize: (options) ->
    @agent = options.agent
    @timeout = options.timeout
  "*": ->
    logger.error "Agent received unexpected message (#{@state})", arguments

  initialState: "waitingForClient"
  states:
    waitingForServer:
      send: (message, data) ->
        if message == "end" || message == "error"
          @transition("finished")
        else
          @transition("waitingForClient")

        if message == "error"
          @agent.out.emit('failure', message: message, data: data || {})
        else
          @agent.out.emit('success', message: message, data: data || {})

      forward: (message, data) ->
        @agent.out.emit('failure', message: "unexpectedMessage")

    waitingForClient:
      _onEnter: ->
        if @timeout
          @timer = setTimeout =>
            logger.error "timeout!"
            @transition("timedOut")
          , @timeout

      forward: (message, data) ->
        @transition "waitingForServer"
        heard = @agent.in.emit message, data
        if !heard
          logger.error "no one was listening to #{message}"
          #TODO: Error out (if they send a login request in the middle of a game)

      _onExit: ->
        clearTimeout(@timer)

    timedOut:
      _onEnter: ->
        logger.error "agent timeout"
        @agent.in.emit "timeout"

      forward: (message, data) ->
        logger.verbose "Discarded message #{message}", data

      send: (message, data) ->
        @transition("finished")
        @agent.out.emit('failure', message: message, data: data || {})

    finished: {}

module.exports = class Agent
  constructor: (options={}) ->
    @in = new EventEmitter()
    @out = new EventEmitter()

    @connectionState = new ConnectionState(agent: this, timeout: options.timeout)

    @connectionState.on "transition", (details) ->
      message = "AGENT changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      # console.log message
      logger.verbose message

  forward: (message, data) ->
    logger.verbose "<<<<<forwarding", message, " - ", data
    @connectionState.handle "forward", message, data

  send: (message, data) ->
    logger.verbose ">>>>>>>sending", message, " - ", data
    @connectionState.handle "send", message, data

  # end: (message, data)

