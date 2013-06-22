Player = require './player'
Game = require './engine/game'

module.exports = class TicTacToeFactory
  constructor: (@gameOptions) ->
    @numberOfPlayers = 2

  createPlayer: ->
    new Player()

  createGame: (players...) ->
    new Game(players..., @gameOptions)
