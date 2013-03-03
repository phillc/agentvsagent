und = require 'underscore'
Pile = require './pile'
Round = require './round'

class State
  constructor: (@game) ->

  handleAction: (action) ->
    false

exports.StartingGame = class StartingGame extends State
  run: ->
    console.log "Starting game with players:", @players
    positions = ["north", "east", "west", "south"]

    for player in und.shuffle(@game.players)
      @game[positions.shift()] = player

      player.emit 'start', @game.id

    @game.stack.push("endingGame")
    @game.stack.push("startingRound")
    @game.nextState()

exports.StartingRound = class StartingRound extends State
  run: ->
    @game.currentRound = new Round()
    @game.stack.push("startingTrick")
    @game.stack.push("passing")
    @game.stack.push("dealing")
    @game.nextState()

exports.Dealing = class Dealing extends State
  run: ->
    deck = Pile.createDeck()

    # @game.positions.leftOf(@game.currentDealer)
    # players = @game.positions.fromLeftOf(@game.currentDealer)
    deck.moveCardsTo(13, @game.currentRound.north.dealt)
    deck.moveCardsTo(13, @game.currentRound.east.dealt)
    deck.moveCardsTo(13, @game.currentRound.south.dealt)
    deck.moveCardsTo(13, @game.currentRound.west.dealt)
    @game.nextState()

exports.Passing = class Passing extends State
  run: ->
    @game.nextState()

  handleAction: (action) ->
    recordAction

    if allPlayersPassed
      broadcast
      proceed()

exports.PlayingTrick = class PlayingTrick extends State
  run: ->
    new Hand(start: North)
    new WaitForCard(north, hand)

  handleAction: (action) ->
    new WaitForCard(east, hand)
    new WaitForCard(south, hand)
    new WaitForCard(west, hand)
    new EndTrick
    proceed()
# 
# class WaitForCard
#   player
# 
#   if action not by player
#     raise
#   else
#     gameState.next
# 
# class EndTrick
#   tallyScore
#   if any over 100
#     gameState.push new EndGame
#   else
#     gameState.push new StartTrick
#   gameState.next
# 
# 
# gameState.add new GameStart()
