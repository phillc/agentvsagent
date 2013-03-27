window.HeartsCtrl = HeartsCtrl = ($scope, GameService) ->
  console.log("IN CNTRL")
  client = GameService.createClient()
  $scope.startGame = -> client.startGame()

  $scope.pass = ->
    client.passCards(card for card in $scope.hand when card.checked)
  $scope.play = ->
    client.playCard((card for card in $scope.hand when card.checked)[0])

HeartsCtrl.$inject = ['$scope', 'GameService']
