Player = require './player'
Game = require './game'

module.exports = class Heartsbuilder
  constructor: (@gameOptions) ->
    @numberOfPlayers = 4

  positions: ->
    ["north", "south", "east", "west"]

  createGame: (positions) ->
    new Game(positions, @gameOptions)
