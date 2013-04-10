und = require 'underscore'
logger = require '../../logger'
Pile = require './pile'
Round = require './round'

class State
  constructor: (@game) ->

  run: ->

  handleAction: (action, callback) ->
    callback("actionOutOfSequence", null)

exports.StartingGame = class StartingGame extends State
  run: ->
    logger.info "Starting game with players:", @game.players.map (p) -> p.id
    positions = ["north", "east", "west", "south"]

    for player in und.shuffle(@game.players)
      @game.positions[positions.shift()] = player

      player.sendStartedGame @game.id

    @game.stack.push("startingRound")
    @game.nextState()

exports.StartingRound = class StartingRound extends State
  run: ->
    @game.rounds.push(new Round())

    @game.stack.push("endingRound")
    for _ in [1..13]
      @game.stack.push("startingTrick")

    passing = ["passingLeft", "passingRight", "passingAcross"][(@game.rounds.length - 1) % 4]
    if passing
      @game.stack.push(passing)
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
      @game.positions[position].sendDealt seat.dealt.cards

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

          game.positions[toPosition].sendPassed passedCards

exports.StartingTrick = class StartingTrick extends State
  run: ->
    @game.currentRound().newTrick()
    @game.stack.push("endingTrick")
    for position in @game.currentRound().currentTrick().positionsFromLeader().reverse()
      @game.stack.push("waitingForCardFrom" + position.charAt(0).toUpperCase() + position.slice(1))
    @game.nextState()

exports.WaitingForCard = class WaitingForCard extends State
  constructor: (game, @position) ->
    super(game)

  run: ->
    logger.info "Waiting for card from", @position
    @game.positions[@position].sendTurn @game.currentRound().currentTrick()

  handleAction: (action) ->
    logger.info "Handling action while waiting for card from", @position
    # TODO: Remove from held
    action.execute(@game)
    @game.nextState()

exports.EndingTrick = class EndingTrick extends State
  run: ->
    logger.info "Trick ended"
    for player in @game.players
      player.sendEndTrick @game.currentRound().currentTrick()
    @game.nextState()

exports.EndingRound = class EndingRound extends State
  run: ->
    logger.info "round ended", @game.currentRound().scores()
    if @game.maxPenaltyReached()
      @game.stack.push("endingGame")
      for player in @game.players
        player.sendEndRound @game.currentRound().scores(), 'endGame'
    else
      @game.stack.push("startingRound")
      for player in @game.players
        player.sendEndRound @game.currentRound().scores(), 'nextRound'

    # Should this pause and wait for all bots
    # to check in before moving to the next round?
    @game.nextState()

exports.EndingGame = class EndingGame extends State
  run: ->
    # TODO: cleanup from arena
    logger.info "game ended", @game.scores()
    for player in @game.players
      player.sendEndGame @game.scores()


