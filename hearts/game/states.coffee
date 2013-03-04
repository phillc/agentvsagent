und = require 'underscore'
Pile = require './pile'
Round = require './round'

class State
  constructor: (@game) ->

  run: ->

  handleAction: (action) ->
    false

exports.StartingGame = class StartingGame extends State
  run: ->
    console.log "Starting game with players:", @players
    positions = ["northPlayer", "eastPlayer", "westPlayer", "southPlayer"]

    for player in und.shuffle(@game.players)
      @game[positions.shift()] = player

      player.emit 'started', @game.id

    @game.stack.push("endingGame")
    @game.stack.push("startingRound")
    @game.nextState()

exports.StartingRound = class StartingRound extends State
  run: ->
    @game.currentRound = new Round()
    @game.stack.push("startingTrick")
    @game.stack.push("passingRight")
    @game.stack.push("dealing")
    @game.nextState()

exports.Dealing = class Dealing extends State
  run: ->
    deck = Pile.createDeck()

    # @game.positions.leftOf(@game.currentDealer)
    # players = @game.positions.fromLeftOf(@game.currentDealer)
    deck.moveCardsTo(13, @game.currentRound.north.dealt)
    @game.northPlayer.emit 'dealt', @game.currentRound.north.dealt.cards

    deck.moveCardsTo(13, @game.currentRound.east.dealt)
    @game.eastPlayer.emit 'dealt', @game.currentRound.east.dealt.cards

    deck.moveCardsTo(13, @game.currentRound.south.dealt)
    @game.southPlayer.emit 'dealt', @game.currentRound.south.dealt.cards

    deck.moveCardsTo(13, @game.currentRound.west.dealt)
    @game.westPlayer.emit 'dealt', @game.currentRound.west.dealt.cards

    @game.nextState()

exports.Passing = class Passing extends State
  # @directions =
  #   left: null
  #   right: null
  #   across: null

  constructor: (game, direction) ->
    # @strategy = Passing.directions[direction]
    super(game)

  handleAction: (action) ->
    action.execute(@game)

    if @game.currentRound.allHavePassed()
      @game.nextState()

exports.StartingTrick = class StartingTrick extends State
  run: ->
    @game.currentRound.tricks.push({})
    @game.stack.push("waitingForCardFromNorth")
    @game.stack.push("waitingForCardFromWest")
    @game.stack.push("waitingForCardFromSouth")
    @game.stack.push("waitingForCardFromEast")

exports.WaitingForCard = class WaitingForCard extends State
  constructor: (game, @position) ->
    super(game)

  handleAction: (action) ->
    action.execute(@game)
    @game.nextState()

# exports.PlayingTrick = class PlayingTrick extends State
#   run: ->
#     new Hand(start: North)
#     new WaitForCard(north, hand)
# 
#   handleAction: (action) ->
#     new WaitForCard(east, hand)
#     new WaitForCard(south, hand)
#     new WaitForCard(west, hand)
#     new EndTrick
#     proceed()
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
