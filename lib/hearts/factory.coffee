Player = require './player'
Game = require './game'

module.exports = class HeartsFactory
  constructor: (@gameOptions) ->
    @numberOfPlayers = 4

  createPlayer: ->
    new Player()

  createGame: (players...) ->
    new Game(players..., @gameOptions)
