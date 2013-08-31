{EventEmitter} = require 'events'
logger = require './logger'
und = require 'underscore'
Agent = require './agent'

# class User
#   constructor: ->
#
#   name: ->
#     "anonymous"

module.exports = class Arena extends EventEmitter
  constructor: (@builder, @handlers)->
    @waitingRoom = []
    @runningMatches = []
    @lingerTime = 5000
    @numberOfAgents = @builder.positions().length

    for handler in handlers
      handler.on 'connect', (agent) =>
        agent.on 'join', =>
          # We would login here-ish...
          # accept username/password parameters perhaps
          # user = @getUser()
          # connection.send("joined", user)
          # or agent.user = user

          agent.send("joined")

        agent.on 'ready', =>
          @addAgent agent

  addAgent: (agent) ->
    @waitingRoom.push agent
    logger.info "#{@waitingRoom.length} agents waiting and #{Object.keys(@runningMatches).length} matches playing."
    @emit 'agentJoined'

  removeAgent: (agent) ->
    @waitingRoom.splice(@waitingRoom.indexOf(agent), 1)

  createGame: (agents) ->
    for agent in agents
      @removeAgent(agent)

    gameClass = @builder.Game
    game = @builder.createGame()
    availablePositions = @builder.positions()
    for agent in und.shuffle(agents)
      position = availablePositions.shift()
      do (game, gameClass, position, agent) ->
        logger.info "wiring up #{agent} to #{position}"

        #TODO: clearly, this needs a cleanup
        # Tic tac toe
        # agent.on "move", (args...) ->
        #   game.handle ["move", position].join("."), args...
        # game.on [position, "turn"].join("."), (data) ->
        #   agent.send("turn", data)

        # Hearts
        defaultAgentEvents = ["timeout"]
        defaultGameEvents = ["end", "error"]

        for agentEvent in defaultAgentEvents.concat(gameClass.AGENT_EVENTS)
          do (agent, agentEvent, position) ->
            agent.on agentEvent, (args...) ->
              game.handle [agentEvent, position].join("."), args...
        for gameEvent in defaultGameEvents.concat(gameClass.EVENTS)
          do (game, gameEvent, position) ->
            game.on [position, gameEvent].join("."), (data) ->
              agent.send(gameEvent, data)


    # player.on '*', game.handle player, thing

    @runningMatches.push game
    # Move time to game, and just emit terminated or something
    # game.on 'gameEnded', =>
    #   setTimeout =>
    #     @removeGame(game)
    #   , @lingerTime

    game.start()
    game

  # getGame: (gameId) ->
  #   @runningMatches[gameId]

  # removeGame: (game) ->
  #   @runningMatches.splice(@runningMatches.indexOf(game), 1)
