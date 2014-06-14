logger = require './logger'

module.exports = class Entrance
  constructor: (@arenas) ->

  addAgent: (agent) ->
    agent.in.once 'requestingEntry', (data) =>
      logger.info "Agent has identified as #{data.name}"
      agent.send("entryGranted", message: "How ya doing?")
      if arena = @arenas[data.game]
        agent.in.once 'requestingGame', (data) =>
          console.log "game requested", data
          arena.addAgent(agent)
      else
        agent.send("error", foo: "What?")


