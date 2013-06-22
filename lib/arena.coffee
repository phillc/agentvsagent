{EventEmitter} = require 'events'
logger = require './logger'

module.exports = class Arena extends EventEmitter
  constructor: (@factory)->
    @waitingRoom = []
    @runningMatches = {}
    @lingerTime = 5000
    @numberOfPlayers = @factory.numberOfPlayers

  createPlayer: ->
    player = @factory.createPlayer()
    @waitingRoom.push player
    logger.info "#{@waitingRoom.length} players waiting and #{Object.keys(@runningMatches).length.length} matches playing."
    @emit 'newPlayer'
    player

  removePlayer: (player) ->
    @waitingRoom.splice(@waitingRoom.indexOf(player), 1)

  createGame: (players) ->
    for player in players
      @removePlayer(player)

    game = @factory.createGame(players...)

    @runningMatches[game.id] = game
    game.on 'gameEnded', =>
      setTimeout =>
        @removeGame(game.id)
      , @lingerTime

    game.start()
    game

  getGame: (gameId) ->
    @runningMatches[gameId]

  removeGame: (gameId) ->
    delete @runningMatches[gameId]
