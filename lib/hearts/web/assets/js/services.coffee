class Client
  constructor: (@$rootScope) ->
    @transport = new Thrift.Transport("/game/hearts/service.json")
    @protocol  = new Thrift.Protocol(@transport)
    @client    = new AgentVsAgent.HeartsClient(@protocol)

  startGame: ->
    console.log @
    @client.enter_arena (response) =>
      @$rootScope.$apply ($scope) ->
        $scope.ticket = response.ticket
      @ticket = response.ticket
      if response.ticket
        @client.get_game_info @ticket, (gameInfo) =>
          @$rootScope.$apply ($scope) ->
            $scope.position = gameInfo.position
            $scope.roundNumber = 0
          @position = gameInfo.position
          @roundNumber = 0
          @playRound()

  playRound: ->
    @roundNumber += 1
    @client.get_hand @ticket, (hand) =>
      @hand = hand
      @$rootScope.$apply ($scope) ->
        $scope.hand = hand

  passCards: (cardsToPass) ->
    for card in cardsToPass
      @hand.splice(@hand.indexOf(card), 1)
    @client.pass_cards @ticket, cardsToPass, (receivedCards) =>
      @hand = @hand.concat(receivedCards)
      @$rootScope.$apply ($scope) =>
        $scope.hand = @hand
      @playTrick()


  playTrick: ->
    @client.get_trick @ticket, (trick) =>
      @$rootScope.$apply ($scope) =>
        $scope.trick = trick

  playCard: (cardToPlay) ->
    @hand.splice(@hand.indexOf(cardToPlay), 1)
    @client.play_card @ticket, cardToPlay, (trickResult) =>
      if @hand.length == 0
        @client.get_round_result @ticket, (roundResult) =>
          @$rootScope.$apply ($scope) =>
            $scope.roundResult = roundResult
          if roundResult.status != AgentVsAgent.GameStatus.NEXT_ROUND
            @client.get_game_result @ticket, (gameResult) ->
              @$rootScope.$apply ($scope) =>
                $scope.gameResult = gameResult
          else
            @playRound()
      else
        @playTrick()

angular.module('AgentVsAgent.services', [])
  .factory 'GameService', ["$rootScope", (args...) -> createClient: -> new Client(args...)]
