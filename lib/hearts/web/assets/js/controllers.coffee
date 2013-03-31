window.HeartsCtrl = HeartsCtrl = ($scope, GameService) ->
  game = GameService.createGame()
  $scope.state = game.state
  $scope.Suits = AgentVsAgent.Suit
  console.log("scope:", $scope)

  $scope.startGame = ->
    game.start()
  $scope.pass = ->
    game.passCards(card for card in $scope.state.hand when card.checked)
  $scope.play = ->
    game.playCard((card for card in $scope.state.hand when card.checked)[0])

HeartsCtrl.$inject = ['$scope', 'GameService']
