thrift = require('thrift')
Hearts = require('./lib/Hearts')
types = require('./lib/hearts_types')

class Trick
  constructor: (@number, @round, @options) ->
    @leader = null
    @played = []

  run: (callback) ->
    @log "Starting trick"
    @options.client.get_trick @options.ticket, (err, trick) =>
      throw err if err
      @leader = trick.leader
      @played = trick.played

      cardToPlay = @options.playCardFn this
      @round.held.splice(@round.held.indexOf(cardToPlay), 1)
      @options.client.play_card @options.ticket, cardToPlay, (err, trickResult) =>
        throw err if err
        @log "trick: result", trickResult
        @played = trickResult.played
        callback()

  log: (args...) ->
    @round.log "T:#{@number}", args...

class Round
  constructor: (@number, @game, @options) ->
    @tricks = []
    @dealt = []
    @held = []

  createTrick: ->
    trickNumber = @tricks.length + 1
    trick = new Trick(trickNumber, this, @options)
    @tricks.push trick
    trick

  run: (callback) ->
    @log "Starting round"
    @options.client.get_hand @options.ticket, (err, hand) =>
      throw err if err
      @log "You were dealt:", hand
      @dealt = hand.slice(0)
      @held = hand.slice(0)

      @passCards =>
        @playTrick callback

  passCards: (callback) ->
    if @number % 4 == 0
      @log "Not passing cards"
      callback()
    else
      @log "About to pass cards"
      cardsToPass = @options.passCardsFn this
      for cardToPass in cardsToPass
        @held.splice(@held.indexOf(cardToPass), 1)

      @options.client.pass_cards @options.ticket, cardsToPass, (err, receivedCards) =>
        throw err if err
        @log "Received cards:", receivedCards
        @held = @held.concat(receivedCards)
        callback()

  playTrick: (callback) ->
    trick = @createTrick()

    trick.run =>
      if @tricks.length >= 13
        callback()
      else
        @playTrick(callback)

  log: (args...) ->
    @game.log "R:#{@number}", args...

class Game
  constructor: (@info, @options) ->
    @rounds = []

  createRound: ->
    roundNumber = @rounds.length + 1
    round = new Round(roundNumber, this, @options)
    @rounds.push round
    round

  run: (callback) ->
    @log "Starting game"

    round = @createRound()

    round.run =>
      @options.client.get_round_result @options.ticket, (err, roundResult) =>
        throw err if err
        @log "round result:", roundResult
        if roundResult.status != types.GameStatus.NEXT_ROUND
          callback()
        else
          @run(callback)

  log: (args...) ->
    console.log "P:#{@info.position}", args...

exports.play = (passCardsFn, playCardFn) ->
  host = process.env.AVA_HOST || '127.0.0.1'
  port = process.env.AVA_PORT || 4001
  connection = thrift.createConnection(host, port)
  client = thrift.createClient(Hearts, connection)

  request = new types.EntryRequest()
  console.log "Entering arena", request
  client.enter_arena request, (err, response) =>
    throw err if err
    ticket = response.ticket
    if ticket
      console.log "playing"
      client.get_game_info ticket, (err, gameInfo) =>
        throw err if err
        console.log "game info:", gameInfo

        game = new Game gameInfo,
          ticket: ticket
          client: client
          passCardsFn: passCardsFn
          playCardFn: playCardFn

        game.run =>
          console.log "Game is over"
          client.get_game_result ticket, (err, gameResult) ->
            throw err if err
            console.log "game result:", gameResult
            connection.end()
    else
      console.log "No ticket"
      connection.end()

exports.Suit = types.Suit
exports.Rank = types.Rank
exports.Card = types.Card

