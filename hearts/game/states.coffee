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
    console.log "Starting game with players:", @game.players.map (p) -> p.id
    positions = ["north", "east", "west", "south"]

    for player in und.shuffle(@game.players)
      @game.positions[positions.shift()] = player

      player.emit 'started', @game.id

    @game.stack.push("endingGame")
    @game.stack.push("startingRound")
    @game.nextState()

exports.StartingRound = class StartingRound extends State
  run: ->
    @game.rounds.push(new Round())

    for _ in [1..13]
      @game.stack.push("startingTrick")
    @game.stack.push("passingRight")
    @game.stack.push("dealing")
    @game.nextState()

exports.Dealing = class Dealing extends State
  run: ->
    deck = Pile.createShuffledDeck()

    # TODO: copyAllCardsTo held
    deck.moveCardsTo(13, @game.currentRound().north.dealt)
    @game.positions.north.emit 'dealt', @game.currentRound().north.dealt.cards

    deck.moveCardsTo(13, @game.currentRound().east.dealt)
    @game.positions.east.emit 'dealt', @game.currentRound().east.dealt.cards

    deck.moveCardsTo(13, @game.currentRound().south.dealt)
    @game.positions.south.emit 'dealt', @game.currentRound().south.dealt.cards

    deck.moveCardsTo(13, @game.currentRound().west.dealt)
    @game.positions.west.emit 'dealt', @game.currentRound().west.dealt.cards

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

    if @game.currentRound().allHavePassed()
      # add to passed for the seat
      # Maybe have a holding one as well, that can be drained and validated against

      #TODO: copy into passed
      # and copyAllTo held
      @game.positions.north.emit 'passed', @game.currentRound().east.passed.cards
      @game.positions.east.emit 'passed', @game.currentRound().south.passed.cards
      @game.positions.south.emit 'passed', @game.currentRound().west.passed.cards
      @game.positions.west.emit 'passed', @game.currentRound().north.passed.cards
      @game.nextState()

exports.StartingTrick = class StartingTrick extends State
  run: ->
    @game.currentRound().newTrick()
    @game.stack.push("endingTrick")
    @game.stack.push("waitingForCardFromNorth")
    @game.stack.push("waitingForCardFromWest")
    @game.stack.push("waitingForCardFromSouth")
    @game.stack.push("waitingForCardFromEast")

    @game.nextState()

exports.WaitingForCard = class WaitingForCard extends State
  constructor: (game, @position) ->
    super(game)

  run: ->
    @game.positions[@position].emit 'turn', @game.currentRound().currentTrick()

  handleAction: (action) ->
    # TODO: Remove from held
    action.execute(@game)
    @game.nextState()

exports.EndingTrick = class EndingTrick extends State
  run: ->
    for player in @game.players
      player.emit 'endTrick', @game.currentRound().currentTrick()
    @game.nextState()


# class EndRound
#   tallyScore
#   if any over 100
#     gameState.push new EndGame
#   else
#     gameState.push new StartTrick
#   gameState.next
# 
# 
# gameState.add new GameStart()
