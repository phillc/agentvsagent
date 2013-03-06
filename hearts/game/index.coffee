IdGenerator = require '../idgenerator'
states = require './states'

module.exports = class Game
  constructor: (player1, player2, player3, player4) ->
    @id = IdGenerator.generate()
    @players = [player1, player2, player3, player4]

    # STATES
    @stack = []
    @states =
      startingGame: new states.StartingGame(this)
      startingRound: new states.StartingRound(this)
      startingTrick: new states.StartingTrick(this)
      dealing: new states.Dealing(this)
      passingRight: new states.Passing(this, "right")
      waitingForCardFromNorth: new states.WaitingForCard(this, "north")
      waitingForCardFromEast: new states.WaitingForCard(this, "east")
      waitingForCardFromSouth: new states.WaitingForCard(this, "south")
      waitingForCardFromWest: new states.WaitingForCard(this, "west")
      endingTrick: new states.EndingTrick(this)
      endingGame: {run: ->}#new states.EndGame(this)

    # DATA
    @positions = {}
    @rounds = []

  currentRound: ->
    @rounds[@rounds.length - 1]

  positionOf: (player) ->
    if @positions.north == player
      "north"
    else if @positions.east == player
      "east"
    else if @positions.south == player
      "south"
    else if @positions.west == player
      "west"

  getPlayer: (playerId) ->
    for player in @players
      return player if player.id == playerId

  nextState: ->
    console.log "nextState:: stack",  @stack
    @currentState = @states[@stack.pop()]
    @currentState.run()

  start: ->
    @stack.push("startingGame")
    @nextState()

  handleAction: (action) ->
    @currentState.handleAction(action)

