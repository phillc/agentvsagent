und = require 'underscore'
logger = require '../logger'
Pile = require './pile'
Round = require './round'

class State
  constructor: (@game) ->

  run: ->

  handleAction: (action) ->
    false

exports.StartingGame = class StartingGame extends State
  run: ->
    logger.info "Starting game with players:", @game.players.map (p) -> p.id
    positions = ["north", "east", "west", "south"]

    for player in und.shuffle(@game.players)
      @game.positions[positions.shift()] = player

      player.emit 'started', @game.id

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
    logger.info "Dealing"
    @deal()
    @game.nextState()

  deal: ->
    deck = Pile.createShuffledDeck()

    positions = ["north", "east", "south", "west"]

    for position in positions
      seat = @game.currentRound()[position]
      deck.moveCardsTo(13, seat.dealt)
      seat.dealt.copyAllCardsTo(seat.held)
      @game.positions[position].emit 'dealt', seat.dealt.cards

exports.Passing = class Passing extends State
  constructor: (game, @direction) ->
    super(game)

  run: ->
    logger.info "Passing", @direction

  handleAction: (action) ->
    logger.info "Handling passing action"
    action.execute(@game)

    if @game.currentRound().allHavePassed()
      @exchange()
      @game.nextState()

  exchange: ->
    passing =
      right: [["north", "west"], ["east", "north"], ["south", "east"], ["west", "south"]]
      left: [["north", "east"], ["east", "south"], ["south", "west"], ["west", "north"]]
      across: [["north", "south"], ["east", "west"], ["south", "north"], ["west", "east"]]

    strategy = passing[@direction]

    game = @game
    do (strategy, game) ->
      for pair in strategy
        do ->
          round = game.currentRound()
          fromPosition = pair[0]
          toPosition = pair[1]
          fromSeat = round[fromPosition]
          toSeat = round[toPosition]
          passedCards = fromSeat.passed.cards

          for card in passedCards
            fromSeat.held.moveCardTo(card, toSeat.held)

          game.positions[toPosition].emit 'passed', passedCards

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

exports.EndingRound = class EndingRound extends State
  run: ->
    if @game.maxPenaltyReached()
      @game.stack.push("endingGame")
      for player in @game.players
        player.emit 'endRound', @game.currentRound().scores(), 'endGame'
    else
      @game.stack.push("startingRound")
      for player in @game.players
        player.emit 'endRound', @game.currentRound().scores(), 'nextRound'

    # Should this pause and wait for all bots
    # to check in before moving to the next round?
    @game.nextState()

