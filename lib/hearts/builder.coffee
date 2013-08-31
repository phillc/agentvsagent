Game = require './game'

#TODO: Remove me

module.exports = class Heartsbuilder
  constructor: (@gameOptions) ->
    @numberOfPlayers = Game.positions().length
    @Game = Game

  positions: ->
    Game.positions()

  createGame: (positions) ->
    new Game(positions, {})

