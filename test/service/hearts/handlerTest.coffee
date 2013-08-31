Card = require '../../../lib/hearts/card'
Suit = require '../../../lib/hearts/suit'
Rank = require '../../../lib/hearts/rank'
Trick = require '../../../lib/hearts/trick'
Handler = require '../../../service/hearts/handler'
mapper = require '../../../service/hearts/mapper'
types = require '../../../service/hearts/types/hearts_types'
Hearts = require '../../../service/hearts/types/Hearts'

describe "Handler", ->
  beforeEach ->
    @handler = new Handler()
    agentId = @handler._createAgent()
    @agent = @handler._getAgent(agentId)
    @ticket = new types.Ticket(agentId: agentId)

  it "implements everything declared in the service", ->
    serviceMethods = Object.keys(Hearts.Client.prototype).filter (method) ->
      method[0..3] != "send" && method[0..3] != "recv"

    for method in serviceMethods
      expect(@handler).to.have.property(method)
      expect(@handler[method]).to.have.length(Hearts.Client.prototype[method].length)

    handlerMethods = Object.keys(Handler.prototype).filter((name) -> name[0] != "_" && name != "constructor")

    expect(handlerMethods).to.eql(serviceMethods)

  describe "#enter_arena", ->
    it "returns when joined", (done) ->
      @handler.on 'connect', (connection) ->
        connection.on 'join', ->
          connection.send("joined")

      @handler.enter_arena new types.EntryRequest(), (err, response) =>
        expect(err).to.not.exist
        expect(response.ticket.agentId).to.be.a("string")
        done()

  describe "#get_game_info", ->
    it "returns game info", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        expect(err).to.not.exist
        expect(gameInfo.position).to.equal("west")
        done()

      @agent.send("begin", position: "west")

    it "passes through errors", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        expect(err.message).to.equal("Method call out of sequence")
        expect(gameInfo).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})

  describe "#get_hand", ->
    it "returns the players hand", (done) ->
      @handler.get_hand @ticket, (err, hand) ->
        expect(err).to.not.exist
        expect(hand).to.have.length(13)
        done()

      @agent.send("dealt", cards: Card.all()[1..13])

    it "maps the cards to thrift types", (done) ->
      @handler.get_hand @ticket, (err, cards) ->
        expect(err).to.not.exist
        expect(cards[0].suit).to.equal(types.Suit.CLUBS)
        expect(cards[0].rank).to.equal(types.Rank.TWO)
        expect(cards[1].suit).to.equal(types.Suit.SPADES)
        expect(cards[1].rank).to.equal(types.Rank.FOUR)
        expect(cards[2].suit).to.equal(types.Suit.DIAMONDS)
        expect(cards[2].rank).to.equal(types.Rank.ACE)
        expect(cards[3].suit).to.equal(types.Suit.HEARTS)
        expect(cards[3].rank).to.equal(types.Rank.EIGHT)
        expect(cards[4].suit).to.equal(types.Suit.SPADES)
        expect(cards[4].rank).to.equal(types.Rank.QUEEN)
        expect(cards[5].suit).to.equal(types.Suit.SPADES)
        expect(cards[5].rank).to.equal(types.Rank.NINE)
        expect(cards[6].suit).to.equal(types.Suit.HEARTS)
        expect(cards[6].rank).to.equal(types.Rank.TEN)
        expect(cards[7].suit).to.equal(types.Suit.DIAMONDS)
        expect(cards[7].rank).to.equal(types.Rank.KING)
        expect(cards[8].suit).to.equal(types.Suit.CLUBS)
        expect(cards[8].rank).to.equal(types.Rank.THREE)
        expect(cards[9].suit).to.equal(types.Suit.HEARTS)
        expect(cards[9].rank).to.equal(types.Rank.FIVE)
        expect(cards[10].suit).to.equal(types.Suit.HEARTS)
        expect(cards[10].rank).to.equal(types.Rank.JACK)
        expect(cards[11].suit).to.equal(types.Suit.CLUBS)
        expect(cards[11].rank).to.equal(types.Rank.SIX)
        expect(cards[12].suit).to.equal(types.Suit.SPADES)
        expect(cards[12].rank).to.equal(types.Rank.SEVEN)
        done()

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
      @agent.send("dealt", cards: cards)

    it "passes through errors", (done) ->
      @handler.get_hand @ticket, (err, cards) ->
        expect(err.message).to.equal("Method call out of sequence")
        expect(cards).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})

  describe "#pass_cards", ->
    it "passes the card", (done) ->
      cards = [
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.TWO)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.FOUR)
        new types.Card(suit: types.Suit.DIAMONDS, rank: types.Rank.ACE)
      ]

      @agent.on "passCards", (action) ->
        expect(action.cards[0].suit).to.equal(Suit.CLUBS)
        expect(action.cards[0].rank).to.equal(Rank.TWO)
        expect(action.cards[1].suit).to.equal(Suit.SPADES)
        expect(action.cards[1].rank).to.equal(Rank.FOUR)
        expect(action.cards[2].suit).to.equal(Suit.DIAMONDS)
        expect(action.cards[2].rank).to.equal(Rank.ACE)
        done()

      @handler.pass_cards @ticket, cards, ->

    it "returns the cards passed to the west player", (done) ->
      @handler.pass_cards @ticket, [], (err, cards) =>
        expect(err).to.not.exist
        expect(cards[0].suit).to.equal(types.Suit.HEARTS)
        expect(cards[0].rank).to.equal(types.Rank.FIVE)
        expect(cards[1].suit).to.equal(types.Suit.CLUBS)
        expect(cards[1].rank).to.equal(types.Rank.FOUR)
        expect(cards[2].suit).to.equal(types.Suit.SPADES)
        expect(cards[2].rank).to.equal(types.Rank.ACE)
        done()

      cards = [
        new Card(Suit.HEARTS, Rank.FIVE)
        new Card(Suit.CLUBS, Rank.FOUR)
        new Card(Suit.SPADES, Rank.ACE)
      ]
      @agent.send("received", cards: cards)

    it "passes through errors", (done) ->
      @handler.pass_cards @ticket, [], (err, cards) ->
        expect(err.message).to.equal("Method call out of sequence")
        expect(cards).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})

  describe "#get_trick", ->
    it "returns the current trick", (done) ->
      @handler.get_trick @ticket, (error, trick) =>
        expect(error).to.not.exist
        expect(trick.leader).to.equal("north")
        expect(trick.played[0].suit).to.equal(types.Suit.HEARTS)
        expect(trick.played[0].rank).to.equal(types.Rank.NINE)
        done()

      trick = new Trick("north")
      trick.played.addCard(new Card(Suit.HEARTS, Rank.NINE))
      @agent.send("turn", trick: trick)

    it "passes through errors", (done) ->
      @handler.get_trick @ticket, (err, trick) ->
        expect(err.message).to.equal("Method call out of sequence")
        expect(trick).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})

  describe "#play_card", ->
    beforeEach ->
      @thriftCard = new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.TWO)

    it "plays the card", (done) ->
      @agent.on "playCard", (action) ->
        expect(action.card.suit).to.equal(Suit.CLUBS)
        expect(action.card.rank).to.equal(Rank.TWO)
        done()

      @handler.play_card @ticket, @thriftCard, ->

    it "returns the result of the trick", (done) ->
      @handler.play_card @ticket, @thriftCard, (error, trick) =>
        expect(error).to.not.exist
        expect(trick.leader).to.equal("north")
        expect(trick.played[0].suit).to.equal(types.Suit.HEARTS)
        expect(trick.played[0].rank).to.equal(types.Rank.NINE)
        done()

      trick = new Trick("north")
      trick.played.addCard(new Card(Suit.HEARTS, Rank.NINE))
      @agent.send("trickEnded", trick: trick)

    it "passes through errors", (done) ->
      @handler.play_card @ticket, @thriftCard, (err, trick) ->
        expect(err.message).to.equal("Method call out of sequence")
        expect(trick).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})

  describe "#get_round_result", ->
    it "waits for round result", (done) ->
      @agent.on "finishedRound", ->
        done()

      @handler.get_round_result @ticket, ->

    it "returns the results of the previous round and play the next hand", (done) ->
      @handler.get_round_result @ticket, (error, roundResult) =>
        expect(error).to.not.exist
        expect(roundResult.north).to.equal(99)
        expect(roundResult.east).to.equal(0)
        expect(roundResult.south).to.equal(15)
        expect(roundResult.west).to.equal(1)
        expect(roundResult.status).to.equal(types.GameStatus.NEXT_ROUND)
        done()

      @agent.send("roundEnded", roundScores: { north: 99, east: 0, south: 15, west: 1 }, status: "nextRound")

    it "returns the results of the previous round and finish the game", (done) ->
      @handler.get_round_result @ticket, (error, roundResult) =>
        expect(error).to.not.exist
        expect(roundResult.north).to.equal(51)
        expect(roundResult.east).to.equal(0)
        expect(roundResult.south).to.equal(15)
        expect(roundResult.west).to.equal(1)
        expect(roundResult.status).to.equal(types.GameStatus.END_GAME)
        done()

      @agent.send("roundEnded", roundScores: { north: 51, east: 0, south: 15, west: 1 }, status: "endGame")

    it "passes through errors", (done) ->
      @handler.get_round_result @ticket, (err, roundResult) ->
        expect(err.message).to.equal("Method call out of sequence")
        expect(roundResult).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})

  describe "#get_game_result", ->
    it "waits for round result", (done) ->
      @agent.on "finishedGame", ->
        done()

      @handler.get_game_result @ticket, ->

    it "returns the results of the game", (done) ->
      @handler.get_game_result @ticket, (err, gameResult) =>
        expect(err).to.not.exist
        expect(gameResult.north).to.equal(101)
        expect(gameResult.east).to.equal(0)
        expect(gameResult.south).to.equal(30)
        expect(gameResult.west).to.equal(2)
        done()
      @agent.send("gameEnded", gameScores: { north: 101, east: 0, south: 30, west: 2 })

    it "passes through errors", (done) ->
      @handler.get_game_result @ticket, (err, gameResult) =>
        expect(err.message).to.equal("Method call out of sequence")
        expect(gameResult).to.not.exist
        done()

      @agent.send("error", {type: "outOfSequence", message: "Method call out of sequence"})
