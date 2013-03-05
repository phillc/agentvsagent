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

    @once 'turn', (trick) =>
      @_waitForTurn = trick

    @once 'endTrick', (trick) =>
      @_waitForTrickFinished = trick

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

  waitForTurn: (callback) ->
    if @_waitForTurn
      callback @_waitForTurn
    else
      @removeAllListeners 'turn'
      @once 'turn', callback

  waitForTrickFinished: (callback) ->
    if @_waitForTrickFinished
      callback @_waitForTrickFinished
    else
      @removeAllListeners 'endTrick'
      @once 'endTrick', callback
