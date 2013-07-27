IdGenerator = require '../lib/idGenerator'
{EventEmitter} = require 'events'
Agent = require '../lib/agent'

module.exports = class AbstractHandler extends EventEmitter
  constructor: (@options={}) ->
    @ticketToAgent = {}

  _createAgent: ->
    id = IdGenerator.generate()
    agent = new Agent(timeout: @options.agentTimeout)
    @ticketToAgent[id] = agent
    @emit 'connect', agent
    id

  _getAgent: (agentId) ->
    @ticketToAgent[agentId]

