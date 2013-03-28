class GameState
  constructor: (@$scope) ->
    for attribute in ["currentState", "ticket", "position", "roundNumber", "hand", "trick"]
      @["set#{attribute.charAt(0).toUpperCase() + attribute.slice(1)}"] = (value) =>
        @[attribute] = value
        @$scope.$apply()
    # @setCurrentState "unstarted"
    @currentState = "unstarted"

  removeCardsFromHand: (cards...) ->
    for card in cards
      @hand.splice(@hand.indexOf(card), 1)
    @$scope.$apply()

  addCardsToHand: (cards...) ->
    @setHand @hand.concat(cards)
    @$scope.$apply()

class Game
  constructor: (@state) ->
    transport = new Thrift.Transport("/game/hearts/service.json")
    protocol  = new Thrift.Protocol(transport)
    @client    = new AgentVsAgent.HeartsClient(protocol)

  start: ->
    @state.setCurrentState("started")
    @client.enter_arena (response) =>
      @state.setTicket(response.ticket)
      if @state.ticket
        @client.get_game_info @state.ticket, (gameInfo) =>
          @state.setPosition(gameInfo.position)
          @state.setRoundNumber(gameInfo.roundNumber)
          @playRound()

  playRound: ->
    @state.setRoundNumber(@state.roundNumber + 1)
    @client.get_hand @state.ticket, (hand) =>
      @setHand(hand)

  passCards: (cardsToPass) ->
    @state.removeCardsFromHand cardsToPass
    @client.pass_cards @state.ticket, cardsToPass, (receivedCards) =>
      @state.addCardsToHand receivedCards...
      @playTrick()

  playTrick: ->
    @client.get_trick @state.ticket, (trick) =>
      @state.setTrick(trick)

  playCard: (cardToPlay) ->
    @state.removeCardsFromHand cardToPlay
    @client.play_card @state.ticket, cardToPlay, (trickResult) =>
      if @hand.length == 0
        @client.get_round_result @state.ticket, (roundResult) =>
          @$rootScope.$apply ($scope) =>
            $scope.roundResult = roundResult
          if roundResult.status != AgentVsAgent.GameStatus.NEXT_ROUND
            @client.get_game_result @state.ticket, (gameResult) ->
              @$rootScope.$apply ($scope) =>
                $scope.gameResult = gameResult
          else
            @playRound()
      else
        @playTrick()

angular.module('AgentVsAgent.services', [])
  .factory 'GameService', ["$rootScope", ($rootScope) ->
    createGame: ->
      new Game(new GameState($rootScope))
  ]
