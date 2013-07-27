Game = require './game'

module.exports = class TicTacToeBuilder
  constructor: (@gameOptions) ->

  positions: ->
    ["X", "O"]

  createGame: (positions) ->
    new Game(positions, @gameOptions)

  events: ->
    []
