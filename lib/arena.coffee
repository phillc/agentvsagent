{EventEmitter} = require 'events'
HeartsPlayer = require './hearts/player'
Game = require './hearts/engine/game'

module.exports = class Arena extends EventEmitter
  constructor: ->
    @waitingRoom = []
    @runningMatches = {}

  createPlayer: ->
    player = new HeartsPlayer()
    @waitingRoom.push player
    @emit 'newPlayer'
    player

  removePlayer: (player) ->
    @waitingRoom.splice(@waitingRoom.indexOf(player), 1)

  createGame: (players) ->
    for player in players
      @removePlayer(player)

    game = new Game(players...)

    @runningMatches[game.id] = game
    game.start()
    game

  getGame: (gameId) ->
    @runningMatches[gameId]

