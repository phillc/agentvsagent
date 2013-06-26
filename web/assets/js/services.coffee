class GameState
  constructor: (@$scope) ->
    @rounds = []
    @hand = []
    attributes = ["currentState", "ticket", "position"]
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
      @$scope.$apply(@_applyOnce)

  _applyOnce: ->
    $('html,body').animate({ scrollTop: document.height }, 'slow')

  removeCardsFromHand: (cards...) ->
    for card in cards
      @hand.splice(@hand.indexOf(card), 1)
    @_apply()

  sort: (cards) ->
    cards.sort (a, b) ->
      if a.suit != b.suit
        a.suit - b.suit
      else
        a.rank - b.rank

  passCards: (cardsToPass...) ->
    @removeCardsFromHand cardsToPass...
    @currentRound().passed = cardsToPass

  receiveCards: (receivedCards...) ->
    @currentRound().received = receivedCards
    @addCardsToHand receivedCards...

  addCardsToHand: (cards...) ->
    @hand = @sort(@hand.concat(cards))
    @_apply()

  newRound: (dealt) ->
    @hand = @sort(dealt)
    @rounds.push(new Round(@hand.slice()))
    @_apply()

  currentRound: ->
    @rounds[@rounds.length - 1]

  newTrick: (trick) ->
    @currentRound().newTrick(trick)
    @_apply()

  finishTrick: (trick) ->
    @currentRound().finishTrick(trick)
    @_apply()

  selectedCards: ->
    @hand.filter (card) -> card.checked

class Round
  constructor: (dealt) ->
    @dealt = dealt || []
    @passed = []
    @received = []
    @tricks = []

  currentTrick: ->
    @tricks[@tricks.length - 1]

  newTrick: (trick) ->
    @tricks.push(trick)

  finishTrick: (trick) ->
    @tricks.pop()
    @tricks.push(trick)

  setResult: (result) ->
    @result = result

class Game
  constructor: (@state) ->
    transport = new Thrift.Transport("/game/hearts/service.json")
    protocol  = new Thrift.Protocol(transport)
    @client    = new AgentVsAgent.HeartsClient(protocol)

  start: ->
    @state.setCurrentState("waitingForGame")
    @client.enter_arena new AgentVsAgent.EntryRequest(), (response) =>
      @state.setTicket(response.ticket)
      if @state.ticket
        @client.get_game_info @state.ticket, (gameInfo) =>
          @state.setCurrentState("started")
          @state.setPosition(gameInfo.position)
          @playRound()

  playRound: ->
    @client.get_hand @state.ticket, (hand) =>
      @state.setCurrentState("passing")
      @state.newRound(hand)
      console.log "NEW ROUND", @state.rounds

  passCards: ->
    cardsToPass = @state.selectedCards()
    @state.passCards(cardsToPass...)
    @state.setCurrentState("waiting")
    @client.pass_cards @state.ticket, cardsToPass, (receivedCards) =>
      @state.receiveCards(receivedCards...)
      @playTrick()

  playTrick: ->
    @client.get_trick @state.ticket, (trick) =>
      @state.newTrick(trick)
      @state.setCurrentState("playing")

  playCard: ->
    cardToPlay = @state.selectedCards()[0]
    @state.setCurrentState("waiting")
    @state.removeCardsFromHand cardToPlay
    @client.play_card @state.ticket, cardToPlay, (trick) =>
      @state.finishTrick(trick)
      if @state.hand.length == 0
        @client.get_round_result @state.ticket, (roundResult) =>
          @state.currentRound().setResult roundResult
          if roundResult.status != AgentVsAgent.GameStatus.NEXT_ROUND
            @client.get_game_result @state.ticket, (gameResult) ->
              # undefined...
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
