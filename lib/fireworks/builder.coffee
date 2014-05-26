Game = require './game'

#TODO: Remove me

module.exports = class FireworksBuilder
  constructor: (@gameOptions={}) ->
    @numberOfPlayers = Game.positions().length
    @Game = Game

  positions: ->
    @Game.positions()

  createGame: ->
    new @Game({})

