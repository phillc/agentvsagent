{EventEmitter} = require 'events'
IdGenerator = require './idgenerator'
Pile = require "./game/Pile"

module.exports = class Player extends EventEmitter
  constructor: ->
    @id = IdGenerator.generate()

    @once 'started', (gameId) =>
      @_waitForGame = gameId

    @once 'dealt', (hand) =>
      @_waitForHand = hand

  #thinking this belongs elsewhere
  waitForGame: (callback) ->
    if @_waitForGame
      callback @_waitForGame
    else
      @removeAllListeners 'started'
      @once 'started', callback

  waitForHand: (callback) ->
    console.log "wait for hand"
    console.log "@_waitForHand", @_waitForHand
    if @_waitForHand
      callback @_waitForHand
    else
      @removeAllListeners 'dealt'
      @once 'dealt', callback
