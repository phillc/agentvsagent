# Pile = require "./pile"
IdGenerator = require '../idgenerator'
states = require './states'
# Engine = 

# class GameManager
# class Engine
# class GameState
# class StartingGame
# class Deal
# class WaitingForPlayer extends GameState
#   constructor: (@position) ->
# class EndTurn
# 

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
      endingGame: {run: ->}#new states.EndGame(this)

    # DATA
    @northPlayer = null
    @eastPlayer = null
    @southPlayer = null
    @westPlayer = null
    @rounds = []
    @currentRound = null

  positionOf: (player) ->
    if @northPlayer == player
      "north"
    else if @eastPlayer == player
      "east"
    else if @southPlayer == player
      "south"
    else if @westPlayer == player
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


  # transition: (state) ->
  #   @states[state].run

  handleAction: (action) ->
    @currentState.handleAction(action)




    # stateObjects


    # @play()

  # play: ->
  #   state =# new GameState
  #     positions:
  #       north: @players[0]
  #       east: @players[1]
  #       south: @players[2]
  #       west: @players[3]





    # loop until player reaches 100 (term: rounds, sequence)
      # createDeck
      # shuffle
      # deal
      # pass
      # loop 13 (term: tricks)
        # 4x (term: rotation)
          # get moves
        # get winner




  # passCards: (stategy) ->



