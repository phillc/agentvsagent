IdGenerator = require '../lib/idGenerator'
{EventEmitter} = require 'events'
Agent = require '../lib/agent'

module.exports = class AbstractHandler extends EventEmitter
  constructor: ->
    @ticketToAgent = {}

  _createAgent: ->
    id = IdGenerator.generate()
    agent = new Agent()
    @ticketToAgent[id] = agent
    @emit 'connect', agent
    id

  _getAgent: (agentId) ->
    @ticketToAgent[agentId]

