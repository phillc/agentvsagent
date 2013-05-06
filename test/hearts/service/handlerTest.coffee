Card = require '../../../lib/hearts/engine/card'
Suit = require '../../../lib/hearts/engine/suit'
Rank = require '../../../lib/hearts/engine/rank'
Handler = require '../../../lib/hearts/service/handler'
mapper = require '../../../lib/hearts/service/mapper'
types = require '../../../lib/hearts/service/types/hearts_types'
Hearts = require '../../../lib/hearts/service/types/hearts'
Factory = require '../../factory'
should = require("should")

describe "Handler", ->
  beforeEach ->
    @arena = Factory.createArena()
    @arena.createPlayer()
    @arena.createPlayer()
    @arena.createPlayer()
    @handler = new Handler(@arena)
    @game = Factory.createGame(arena: @arena)

    @nextStateCalls = 0
    @game.nextState = =>
      @nextStateCalls++

  it "implements everything declared in the service", ->
    methods = Object.keys(Hearts.Client.prototype).filter (method) ->
      method[0..3] != "send" && method[0..3] != "recv"

    for method in methods
      @handler.should.have.property method
      @handler[method].length.should.equal(Hearts.Client.prototype[method].length)

    Object.keys(Handler.prototype).filter((name) -> name[0] != "_").length.should.equal(methods.length)

  describe "#enter_arena", ->
    it "returns when there is a game to be played", (done) ->
      @handler.enter_arena new types.EntryRequest(), (err, response) ->
        should.not.exist(err)
        response.ticket.agentId.should.be.a("string")
        response.ticket.gameId.should.be.a("string")
        done()

      @arena.createGame(@arena.waitingRoom[0..3])

  describe "#get_game_info", ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: @game.positions.west.id, gameId: @game.id)

    it "returns game info", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        should.not.exist(err)
        gameInfo.position.should.equal(types.Position.WEST)
        done()

  describe "#get_hand", ->
    beforeEach ->
      @game.positions.west.messages.splice(0, 1)
      @player = @game.positions.west
      @ticket = new types.Ticket(agentId: @player.id, gameId: @game.id)

    it "returns the players hand", (done) ->
      @handler.get_hand @ticket, (err, hand) ->
        should.not.exist(err)
        hand.length.should.equal(13)
        done()

    it "maps the cards to thrift types", (done) ->
      @player.recvDealt = (callback) ->
        ranks = Rank.all()
        cards = [
          new Card(Suit.CLUBS, Rank.TWO)
          new Card(Suit.SPADES, Rank.FOUR)
          new Card(Suit.DIAMONDS, Rank.ACE)
          new Card(Suit.HEARTS, Rank.EIGHT)
          new Card(Suit.SPADES, Rank.QUEEN)
          new Card(Suit.SPADES, Rank.NINE)
          new Card(Suit.HEARTS, Rank.TEN)
          new Card(Suit.DIAMONDS, Rank.KING)
          new Card(Suit.CLUBS, Rank.THREE)
          new Card(Suit.HEARTS, Rank.FIVE)
          new Card(Suit.HEARTS, Rank.JACK)
          new Card(Suit.CLUBS, Rank.SIX)
          new Card(Suit.SPADES, Rank.SEVEN)
        ]
        callback(null, cards)

      @handler.get_hand @ticket, (err, cards) ->
        should.not.exist(err)
        cards[0].suit.should.equal types.Suit.CLUBS
        cards[0].rank.should.equal types.Rank.TWO
        cards[1].suit.should.equal types.Suit.SPADES
        cards[1].rank.should.equal types.Rank.FOUR
        cards[2].suit.should.equal types.Suit.DIAMONDS
        cards[2].rank.should.equal types.Rank.ACE
        cards[3].suit.should.equal types.Suit.HEARTS
        cards[3].rank.should.equal types.Rank.EIGHT
        cards[4].suit.should.equal types.Suit.SPADES
        cards[4].rank.should.equal types.Rank.QUEEN
        cards[5].suit.should.equal types.Suit.SPADES
        cards[5].rank.should.equal types.Rank.NINE
        cards[6].suit.should.equal types.Suit.HEARTS
        cards[6].rank.should.equal types.Rank.TEN
        cards[7].suit.should.equal types.Suit.DIAMONDS
        cards[7].rank.should.equal types.Rank.KING
        cards[8].suit.should.equal types.Suit.CLUBS
        cards[8].rank.should.equal types.Rank.THREE
        cards[9].suit.should.equal types.Suit.HEARTS
        cards[9].rank.should.equal types.Rank.FIVE
        cards[10].suit.should.equal types.Suit.HEARTS
        cards[10].rank.should.equal types.Rank.JACK
        cards[11].suit.should.equal types.Suit.CLUBS
        cards[11].rank.should.equal types.Rank.SIX
        cards[12].suit.should.equal types.Suit.SPADES
        cards[12].rank.should.equal types.Rank.SEVEN
        done()

    it "passes through errors", (done) ->
      @player.messages.unshift ["foo"]
      @handler.get_hand @ticket, (err, card) ->
        err.message.should.equal("Method call out of sequence")
        should.not.exist(card)
        done()

  describe "#pass_cards", ->
    beforeEach ->
      # @game.stack.push("passingLeft")
      # @game.nextState()
      @game.positions.north.messages.splice(0, 10)
      @game.positions.east.messages.splice(0, 10)
      @game.positions.south.messages.splice(0, 10)
      @game.positions.west.messages.splice(0, 10)
      @northTicket = new types.Ticket(agentId: @game.positions.north.id, gameId: @game.id)
      @eastTicket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)
      @southTicket = new types.Ticket(agentId: @game.positions.south.id, gameId: @game.id)
      @westTicket = new types.Ticket(agentId: @game.positions.west.id, gameId: @game.id)

      @game.currentRound().north.held.cards.splice(0, 13)
      @game.currentRound().north.held.addCard(new Card(Suit.HEARTS, Rank.THREE))
      @game.currentRound().north.held.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @game.currentRound().north.held.addCard(new Card(Suit.SPADES, Rank.QUEEN))
      @northPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.THREE)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.TWO)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.QUEEN)
      ]

      @game.currentRound().east.held.cards.splice(0, 13)
      @game.currentRound().east.held.addCard(new Card(Suit.HEARTS, Rank.FOUR))
      @game.currentRound().east.held.addCard(new Card(Suit.CLUBS, Rank.THREE))
      @game.currentRound().east.held.addCard(new Card(Suit.SPADES, Rank.KING))
      @eastPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.FOUR)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.THREE)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.KING)
      ]

      @game.currentRound().south.held.cards.splice(0, 13)
      @game.currentRound().south.held.addCard(new Card(Suit.HEARTS, Rank.FIVE))
      @game.currentRound().south.held.addCard(new Card(Suit.CLUBS, Rank.FOUR))
      @game.currentRound().south.held.addCard(new Card(Suit.SPADES, Rank.ACE))
      @southPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.FIVE)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.FOUR)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.ACE)
      ]

      @game.currentRound().west.held.cards.splice(0, 13)
      @game.currentRound().west.held.addCard(new Card(Suit.HEARTS, Rank.SIX))
      @game.currentRound().west.held.addCard(new Card(Suit.CLUBS, Rank.FIVE))
      @game.currentRound().west.held.addCard(new Card(Suit.SPADES, Rank.TWO))
      @westPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.SIX)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.FIVE)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.TWO)
      ]

    it "passes the card", ->
      @handler.pass_cards @westTicket, @westPassed, ->
      @game.currentRound().west.passed.cards.should.have.length(3)
      @game.currentRound().west.passed.cards[0].suit.should.equal(Suit.HEARTS)
      @game.currentRound().west.passed.cards[0].rank.should.equal(Rank.SIX)

    it "returns the cards passed to the west player", (done) ->
      @handler.pass_cards @northTicket, @northPassed, ->
      @handler.pass_cards @eastTicket, @eastPassed, ->
      @handler.pass_cards @southTicket, @southPassed, ->
      @handler.pass_cards @westTicket, @westPassed, (err, cards) =>
        should.not.exist(err)
        cards[0].suit.should.equal(types.Suit.HEARTS)
        cards[0].rank.should.equal(types.Rank.FIVE)
        cards[1].suit.should.equal(types.Suit.CLUBS)
        cards[1].rank.should.equal(types.Rank.FOUR)
        cards[2].suit.should.equal(types.Suit.SPADES)
        cards[2].rank.should.equal(types.Rank.ACE)
        done()

    it "passes through errors", (done) ->
      @game.positions.west.messages.unshift ["foo"]
      @handler.pass_cards @westTicket, @westPassed, (err, cards) ->
        err.message.should.equal("Method call out of sequence")
        should.not.exist(cards)
        done()

  describe "#get_trick", ->
    beforeEach ->
      @game.states.startingTrick.run()
      @game.positions.east.messages.splice(0, 10)
      @game.states.waitingForCardFromEast.run()
      @ticket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)

    it "returns the current trick", (done) ->
      @game.currentRound().currentTrick().leader = "north"
      @game.currentRound().currentTrick().played.addCard(new Card(Suit.HEARTS, Rank.NINE))
      @game.currentRound().currentTrick().played.cards.should.have.length(1)

      @handler.get_trick @ticket, (error, trick) =>
        trick.leader.should.equal(types.Position.NORTH)
        trick.played[0].suit.should.equal types.Suit.HEARTS
        trick.played[0].rank.should.equal types.Rank.NINE
        done()

    it "passes through errors", (done) ->
      @game.positions.east.messages.unshift ["foo"]
      @handler.get_trick @ticket, (err, trick) ->
        err.message.should.equal("Method call out of sequence")
        should.not.exist(trick)
        done()

  describe "#play_card", ->
    beforeEach ->
      @game.states.startingTrick.run()
      @game.states.waitingForCardFromEast.run()
      @game.positions.east.messages.splice(0, 10)
      @game.currentState = @game.states.waitingForCardFromEast
      @ticket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)

    it "plays the card", ->
      card = @game.currentRound().east.held.cards[0]
      thriftCard = mapper.cardToThrift(card)
      @handler.play_card @ticket, thriftCard

      @game.currentRound().currentTrick().played.cards[0].isEqual(card).should.equal(true)

    it "returns the result of the trick", (done) ->
      card = @game.currentRound().east.held.cards[0]
      thriftCard = mapper.cardToThrift(card)
      @game.currentRound().currentTrick().leader = "north"
      @handler.play_card @ticket, thriftCard, (error, trick) =>
        should.not.exist(error)
        trick.leader.should.equal(types.Position.NORTH)
        trick.played[0].suit.should.equal thriftCard.suit
        trick.played[0].rank.should.equal thriftCard.rank
        done()

      @game.states.endingTrick.run()

    it "passes through errors", (done) ->
      card = @game.currentRound().east.held.cards[0]
      thriftCard = mapper.cardToThrift(card)
      @game.positions.east.messages.unshift ["foo"]
      @handler.play_card @ticket, thriftCard, (err, trick) ->
        err.message.should.equal("Method call out of sequence")
        should.not.exist(trick)
        done()

  describe "#get_round_result", ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)
      @game.positions.east.messages.splice(0, 10)

    it "returns the results of the previous round and play the next hand", (done) ->
      @game.rounds.push({ scores: -> { north: 99, east: 0, south: 15, west: 1 }})
      @game.states.endingRound.run()
      @handler.get_round_result @ticket, (error, roundResult) =>
        should.not.exist(error)
        roundResult.north.should.equal(99)
        roundResult.east.should.equal(0)
        roundResult.south.should.equal(15)
        roundResult.west.should.equal(1)
        roundResult.status.should.equal types.GameStatus.NEXT_ROUND
        done()

    it "returns the results of the previous round and finish the game", (done) ->
      @game.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})
      @game.rounds.push({ scores: -> { north: 51, east: 0, south: 15, west: 1 }})
      @game.states.endingRound.run()
      @handler.get_round_result @ticket, (error, roundResult) =>
        should.not.exist(error)
        roundResult.north.should.equal(51)
        roundResult.east.should.equal(0)
        roundResult.south.should.equal(15)
        roundResult.west.should.equal(1)
        roundResult.status.should.equal types.GameStatus.END_GAME
        done()

    it "passes through errors", (done) ->
      @game.positions.east.messages.unshift ["foo"]
      @handler.get_round_result @ticket, (err, roundResult) ->
        err.message.should.equal("Method call out of sequence")
        should.not.exist(roundResult)
        done()

  describe "#get_game_result", ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)
      @game.positions.east.messages.splice(0, 10)

    it "returns the results of the game", (done) ->
      @game.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})
      @game.rounds.push({ scores: -> { north: 51, east: 0, south: 15, west: 1 }})
      @game.states.endingGame.run()
      @handler.get_game_result @ticket, (error, gameResult) =>
        should.not.exist(error)
        gameResult.north.should.equal(101)
        gameResult.east.should.equal(0)
        gameResult.south.should.equal(30)
        gameResult.west.should.equal(2)
        done()

    it "passes through errors", (done) ->
      @game.positions.east.messages.unshift ["foo"]
      @handler.get_game_result @ticket, (err, roundResult) ->
        err.message.should.equal("Method call out of sequence")
        should.not.exist(roundResult)
        done()
