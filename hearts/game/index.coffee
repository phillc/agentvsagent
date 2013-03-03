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
      startingTrick: {run: ->}
      dealing: {run: ->}
      passingRight: new states.Passing(this, "right")
      endingGame: {run: ->}#new states.EndGame(this)

    # DATA
    @north = null
    @east = null
    @south = null
    @west = null
    @rounds = []
    @currentRound = null

  getPlayer: (playerId) ->
    for player in @players
      return player if player.id == playerId

  nextState: ->
    console.log "stack",  @stack
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



