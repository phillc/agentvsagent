window.HeartsCtrl = HeartsCtrl = ($scope, GameService) ->
  console.log "HMM", $scope, GameService
  $scope.client = GameService.createClient()
    # { suit: "clubs", rank: "A", value: "c-AC" }
    # { suit: "clubs", rank: "K", value: "c-AK" }


  # $scope.foo = {a: "FOOOO"}
  # setTimeout (->
  #   $scope.foo.a = "BARRRR"
  #   $scope.$apply()
  #   console.log("GOOO")
  # ), 3000

HeartsCtrl.$inject = ['$scope', 'GameService']
