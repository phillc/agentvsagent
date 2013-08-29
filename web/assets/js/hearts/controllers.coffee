window.HeartsCtrl = HeartsCtrl = ($scope, GameService) ->
  game = GameService.createGame()
  $scope.state = game.state
  $scope.Suits = AgentVsAgent.Suit
  console.log("scope:", $scope)

  $scope.startGame = ->
    game.start()
  $scope.pass = ->
    game.passCards()
  $scope.play = ->
    game.playCard()

  positions = [
    "north"
    "east"
    "south"
    "west"
  ]

  $scope.Positions = AgentVsAgent.Position

  $scope.positionsBefore = (trick) ->
    positions.slice(0, positions.indexOf(trick.leader))
  $scope.positionsNotPlayed = (trick) ->
    positions.slice(trick.played.length)
  $scope.positionsAfter = (trick) ->
    positions.slice(positions.indexOf(trick.leader) + 1)

HeartsCtrl.$inject = ['$scope', 'GameService']
