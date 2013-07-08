Player = require './player'
Game = require './game'

# I hate how this conflicts with test factory naming. Rename to builder?
module.exports = class HeartsFactory
  constructor: (@gameOptions) ->
    @numberOfPlayers = 4

  createPlayer: ->
    new Player()

  createGame: (players...) ->
    new Game(players..., @gameOptions)
