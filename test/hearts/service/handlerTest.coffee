Arena = require '../../../hearts/arena'
Handler = require '../../../hearts/service/handler'
Card = require '../../../hearts/game/card'
Suit = require '../../../hearts/game/suit'
Rank = require '../../../hearts/game/rank'
types = require '../../../hearts/lib/hearts_types'
Hearts = require '../../../hearts/lib/hearts'
Factory = require '../factory'
should = require("should")

describe "Handler", ->
  beforeEach ->
    @arena = new Arena()
    @arena.createPlayer()
    @arena.createPlayer()
    @arena.createPlayer()
    @handler = new Handler(@arena)

  it "implements everything declared in the service", ->
    methods = Object.keys(Hearts.Client.prototype).filter (method) ->
      method[0..3] != "send" && method[0..3] != "recv"

    for method in methods
      @handler.should.have.property method

    Object.keys(Handler.prototype).length.should.equal(methods.length)


  describe "#enter_arena", ->
    it "returns when there is a game to be played", (done) ->
      @handler.enter_arena (err, response) ->
        should.not.exist(err)
        response.ticket.agentId.should.be.a("string")
        response.ticket.gameId.should.be.a("string")
        done()

      @arena.createGame(@arena.waitingRoom[0..3])

  describe "#get_game_info", ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: "123", gameId: "456")

    it "returns game info", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        should.not.exist(err)
        gameInfo.position.should.equal(types.Position.NORTH)
        done()

    it "returns players position"

  describe "#get_hand", ->
    beforeEach ->
      @game = Factory.createGame(arena: @arena)
      @player = @game.players[0]
      @ticket = new types.Ticket(agentId: @player.id, gameId: @game.id)
      @game.states.startingGame.run()
      @game.states.dealing.run()

    it "returns the players hand", (done) ->
      @handler.get_hand @ticket, (err, hand) ->
        should.not.exist(err)
        hand.length.should.equal(13)
        done()

    it "maps the cards to thrift types", (done) ->
      @player.waitForHand = (callback) ->
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
        callback(cards)


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

  describe "#pass_cards", ->
    beforeEach ->
      @game = Factory.createGame(arena: @arena)
      @game.states.startingGame.run()
      @game.states.dealing.run()
      @game.stack.push("passingRight")
      @game.nextState()
      @northTicket = new types.Ticket(agentId: @game.positions.north.id, gameId: @game.id)
      @eastTicket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)
      @southTicket = new types.Ticket(agentId: @game.positions.south.id, gameId: @game.id)
      @westTicket = new types.Ticket(agentId: @game.positions.west.id, gameId: @game.id)
      @northPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.THREE)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.TWO)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.QUEEN)
      ]
      @eastPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.FOUR)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.THREE)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.KING)
      ]
      @southPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.FIVE)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.FOUR)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.ACE)
      ]
      @westPassed = [
        new types.Card(suit: types.Suit.HEARTS, rank: types.Rank.SIX)
        new types.Card(suit: types.Suit.CLUBS, rank: types.Rank.FIVE)
        new types.Card(suit: types.Suit.SPADES, rank: types.Rank.TWO)
      ]

    it "passes the card", ->
      @handler.pass_cards @westTicket, @westPassed, ->
      @game.currentRound.west.passed.cards.should.have.length(3)
      @game.currentRound.west.passed.cards[0].suit.should.equal(Suit.HEARTS)
      @game.currentRound.west.passed.cards[0].rank.should.equal(Rank.SIX)

    it "returns the cards passed to the west player", (done) ->
      @handler.pass_cards @northTicket, @northPassed, ->
      @handler.pass_cards @eastTicket, @eastPassed, ->
      @handler.pass_cards @southTicket, @southPassed, ->
      @handler.pass_cards @westTicket, @westPassed, (err, cards) =>
        cards[0].suit.should.equal(Suit.HEARTS)
        cards[0].rank.should.equal(Rank.THREE)
        cards[1].suit.should.equal(Suit.CLUBS)
        cards[1].rank.should.equal(Rank.TWO)
        cards[2].suit.should.equal(Suit.SPADES)
        cards[2].rank.should.equal(Rank.QUEEN)
        done()

  describe "#get_trick", ->
    beforeEach ->
      @game = Factory.createGame(arena: @arena)
      @game.states.startingGame.run()
      @game.states.dealing.run()
      @game.stack.push("waitingForCardFromEast")
      @game.nextState()
      @ticket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)

    it "returns the current trick", (done) ->
      @handler.get_trick @ticket, (error, trick) =>
        trick.should.equal @game.currentRound.tricks[0]
        done()

  describe "#play_card", ->
    beforeEach ->
      @game = Factory.createGame(arena: @arena)
      @game.states.startingGame.run()
      @game.states.dealing.run()
      @game.stack.push("waitingForCardFromEast")
      @game.nextState()
      @ticket = new types.Ticket(agentId: @game.positions.east.id, gameId: @game.id)

    it "plays the card", ->
      card = new types.Card(suit: types.Suit.DIAMONDS, rank: types.Rank.TWO)
      @handler.play_card @ticket, card

      @game.currentRound.tricks[0].east.suit.should.equal(Suit.DIAMONDS)
      @game.currentRound.tricks[0].east.rank.should.equal(Rank.TWO)


    it "returns the result of trick", (done) ->
      card = new types.Card(suit: types.Suit.DIAMONDS, rank: types.Rank.TWO)
      @handler.play_card @ticket, card, (error, trick) =>
        trick.should.equal @game.currentRound.tricks[0]
        done()

      @game.stack.push("endingTrick")
      @game.nextState()


