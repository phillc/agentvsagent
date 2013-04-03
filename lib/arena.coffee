{EventEmitter} = require 'events'

module.exports = class Arena extends EventEmitter
  constructor: (@factory)->
    @waitingRoom = []
    @runningMatches = {}

  createPlayer: ->
    player = @factory.createPlayer()
    @waitingRoom.push player
    @emit 'newPlayer'
    player

  removePlayer: (player) ->
    @waitingRoom.splice(@waitingRoom.indexOf(player), 1)

  createGame: (players) ->
    for player in players
      @removePlayer(player)

    game = @factory.createGame(players...)

    @runningMatches[game.id] = game
    game.start()
    game

  getGame: (gameId) ->
    @runningMatches[gameId]

