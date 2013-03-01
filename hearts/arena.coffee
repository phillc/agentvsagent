{EventEmitter} = require 'events'
Player = require './player'
Game = require './game'

module.exports = class Arena extends EventEmitter
  constructor: ->
    @waitingRoom = []
    @runningMatches = {}

  createPlayer: ->
    player = new Player()
    @waitingRoom.push player
    @emit 'newPlayer'
    player

  removePlayer: (player) ->
    @waitingRoom.splice(@waitingRoom.indexOf(player), 1)

  createMatch: (players) ->
    for player in players
      @removePlayer(player)

    game = new Game(players...)
    @runningMatches[game.id] = game
    game.start() #untested
    game.id


