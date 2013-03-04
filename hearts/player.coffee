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

    @once 'passed', (cards) =>
      @_waitForPassed = cards

  waitForGame: (callback) ->
    if @_waitForGame
      callback @_waitForGame
    else
      @removeAllListeners 'started'
      @once 'started', callback

  waitForHand: (callback) ->
    if @_waitForHand
      callback @_waitForHand
    else
      @removeAllListeners 'dealt'
      @once 'dealt', callback

  waitForPassed: (callback) ->
    if @_waitForPassed
      callback @_waitForPassed
    else
      @removeAllListeners 'passed'
      @once 'passed', callback
