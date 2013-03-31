class GameState
  constructor: (@$scope) ->
    attributes = ["currentState", "ticket", "position", "roundNumber", "trick", "roundResult"]
    for attribute in attributes
      do (attribute) =>
        @["set#{attribute.charAt(0).toUpperCase() + attribute.slice(1)}"] = (value) =>
          @[attribute] = value
          @_apply()

    @setCurrentState "unstarted"

  _apply: ->
    #It's things like this, angular...
    phase = @$scope.$root.$$phase
    unless phase == '$apply' || phase == '$digest'
      @$scope.$apply()

  setHand: (hand) ->
    @hand = hand.sort (a, b) ->
      if a.suit != b.suit
        a.suit - b.suit
      else
        a.rank - b.rank
    @_apply()

  removeCardsFromHand: (cards...) ->
    for card in cards
      @hand.splice(@hand.indexOf(card), 1)
    @_apply()

  addCardsToHand: (cards...) ->
    @setHand @hand.concat(cards)
    @_apply()

class Game
  constructor: (@state) ->
    transport = new Thrift.Transport("/game/hearts/service.json")
    protocol  = new Thrift.Protocol(transport)
    @client    = new AgentVsAgent.HeartsClient(protocol)

  start: ->
    @state.setCurrentState("waitingForGame")
    @client.enter_arena (response) =>
      @state.setTicket(response.ticket)
      if @state.ticket
        @client.get_game_info @state.ticket, (gameInfo) =>
          @state.setCurrentState("started")
          @state.setPosition(gameInfo.position)
          @state.setRoundNumber(gameInfo.roundNumber)
          @playRound()

  playRound: ->
    @state.setRoundNumber(@state.roundNumber + 1)
    @client.get_hand @state.ticket, (hand) =>
      @state.setHand(hand)
      @state.setCurrentState("passing")

  passCards: (cardsToPass) ->
    @state.removeCardsFromHand cardsToPass...
    @client.pass_cards @state.ticket, cardsToPass, (receivedCards) =>
      @state.addCardsToHand receivedCards...
      @state.setCurrentState("playing")
      @playTrick()

  playTrick: ->
    @client.get_trick @state.ticket, (trick) =>
      @state.setTrick(trick)

  playCard: (cardToPlay) ->
    @state.removeCardsFromHand cardToPlay
    @client.play_card @state.ticket, cardToPlay, (trickResult) =>
      if @state.hand.length == 0
        @client.get_round_result @state.ticket, (roundResult) =>
          @state.setRoundResult roundResult
          if roundResult.status != AgentVsAgent.GameStatus.NEXT_ROUND
            @client.get_game_result @state.ticket, (gameResult) ->
              @state.setGameResult gameResult
              @state.setCurrentState("finished")
          else
            @playRound()
      else
        @playTrick()

angular.module('AgentVsAgent.services', [])
  .factory 'GameService', ["$rootScope", ($rootScope) ->
    createGame: ->
      new Game(new GameState($rootScope))
  ]
