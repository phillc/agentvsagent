{EventEmitter} = require 'events'
Player = require './player'
Game = require './game'

module.exports = class Arena extends EventEmitter
  constructor: (@idGenerator) ->
    @waitingRoom = []
    @matches = {}

  createPlayer: ->
    player = new Player(@idGenerator.generate())
    @waitingRoom.push player
    @emit 'newPlayer'
    player

  removePlayer: (player) ->
    @waitingRoom.splice(@waitingRoom.indexOf(player), 1)

  createMatch: (players) ->
    id = @idGenerator.generate()
    for player in players
      @removePlayer(player)

    game = new Game(players...)
    @matches[id] = game
    id


