module.exports = class Entrance
  constructor: (@arenas) ->

  addAgent: (agent) ->
    agent.in.once 'enter', (data) =>
      agent.send("hiya", foo: "How ya doing?")
      if arena = @arenas[data.game]
        agent.in.once 'ready', (data) =>
          console.log "readied", data
          arena.addAgent(agent)
      else
        agent.send("error", foo: "What?")


