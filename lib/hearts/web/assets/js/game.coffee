transport = new Thrift.Transport("/game/hearts/service.json")
protocol  = new Thrift.Protocol(transport)
client    = new AgentVsAgent.HeartsClient(protocol)

class SampleAgent
  constructor: (@game) ->

  run: ->
    console.log "Entering arena"
    @game.enter_arena (response) =>
      @ticket = response.ticket
      if @ticket
        @play()

  play: ->
    console.log "playing", @ticket

    @game.get_game_info @ticket, (gameInfo) =>
      console.log "game info:", gameInfo
      @gameInfo = gameInfo
      @roundNumber = 0
      @playRound()

  playRound: ->
    @roundNumber += 1
    @game.get_hand @ticket, (hand) =>
      console.log "hand:", hand
      @hand = hand

      if @roundNumber % 4 != 0
        cardsToPass = hand.splice(0, 3)
        @game.pass_cards @ticket, cardsToPass, (receivedCards) =>
          @hand = @hand.concat(receivedCards)
          @playTrick(0)
      else
        @playTrick(0)

  playTrick: (trickNumber) ->
    console.log "[#{@gameInfo.position}, round #{@roundNumber}, trick #{trickNumber}, playing trick"

    @game.get_trick @ticket, (trick) =>
      console.log "Leading the trick #{@gameInfo.position}, #{trick}" if @gameInfo.position == trick.leader
      console.log "current trick:", trick

      if trickNumber == 0 && twoClubs = (card for card in @hand when card.suit == AgentVsAgent.Suit.CLUBS && card.rank == AgentVsAgent.Rank.TWO)[0]
        console.log "playing two of clubs"
        cardToPlay = twoClubs
      else if trick.played[0] && matchingSuit = (card for card in @hand when card.suit == trick.played[0].suit)[0]
        console.log "playing matching suit"
        cardToPlay = matchingSuit
      else
        console.log "playing off suit"
        cardToPlay = @hand[0]

      @hand.splice(@hand.indexOf(cardToPlay), 1)
      console.log "[#{@gameInfo.position}] playing card:", cardToPlay
      @game.play_card @ticket, cardToPlay, (trickResult) =>
        console.log "trick: result", trickResult

        if trickNumber >= 12
          @game.get_round_result @ticket, (roundResult) =>
            console.log "round result:", roundResult
            if roundResult.status != AgentVsAgent.GameStatus.NEXT_ROUND
              @game.get_game_result @ticket, (gameResult) ->
                console.log "game result:", gameResult
            else
              @playRound()

        else
          @playTrick trickNumber + 1


window.agent = new SampleAgent(client)
# agent.run()

