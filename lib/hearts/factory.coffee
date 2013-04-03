Player = require './player'
Game = require './engine/game'

module.exports = class HeartsFactory
  constructor: (@gameOptions) ->
  createPlayer: ->
    new Player()

  createGame: (players...) ->
    new Game(players..., @gameOptions)
