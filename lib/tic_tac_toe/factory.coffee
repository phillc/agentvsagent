Game = require './engine/game'

module.exports = class TicTacToeFactory
  constructor: (@gameOptions) ->
  createPlayer: ->
    # new Player()
    {}

  createGame: (players...) ->
    new Game(players..., @gameOptions)
