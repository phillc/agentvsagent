log = (args...) ->
  process.stderr.write(args.toString())

class Client
  listen: ->
    @inputs = []
    buffer = ''

    process.stdin.on 'data', (data) =>
      lines = data.toString().split("\n")
      lines[0] = buffer + lines[0]
      buffer = ''
      if lines[lines.length - 1] != ""
        buffer = lines[lines.length - 1]
        lines.splice(lines.length - 1, 1)

      for line in lines
        if line != ''
          @inputs.push JSON.parse(line.toString())

      if @inputs.length > 0 && @waiting
        waiting = @waiting
        @waiting = null
        waiting @inputs.shift()

  read: (callback) ->
    if @inputs.length > 0
      callback @inputs.shift()
    else
      @waiting = callback

  sendCommand: (command) ->
    process.stdout.write(JSON.stringify(command) + "\n")

  sendAndReceive: (message, data, callback) ->
    @sendCommand {message: message, data: data}
    @read callback

$client = new Client()
$client.listen()

class Trick
  constructor: (@number, @round, @options) ->
    @leader = null
    @played = []

  run: (callback) ->
    @log "Starting trick"
    $client.sendAndReceive "readyForTrick", {}, (response) =>
      throw response if response.message != "turn"
      trick = response.data.trick
      @leader = trick.leader
      @played = trick.played

      cardToPlay = @options.playCardFn this
      @round.held.splice(@round.held.indexOf(cardToPlay), 1)
      $client.sendAndReceive "playCard", {card: cardToPlay}, (response) =>
        throw response if response.message != "finishedTrick"
        trickResult = response.data.trick
        @log "trick: result", trickResult
        @played = trickResult.played
        callback()

  log: (args...) ->
    @round.log "T:#{@number}", args...

class Round
  constructor: (@number, @game, @options) ->
    @tricks = []
    @dealt = []
    @passed = []
    @received = []
    @held = []

  createTrick: ->
    trickNumber = @tricks.length + 1
    trick = new Trick(trickNumber, this, @options)
    @tricks.push trick
    trick

  run: (callback) ->
    @log "Starting round"
    $client.sendAndReceive "readyForRound", {}, (response) =>
      throw response if response.message != "dealt"
      hand = response.data.cards
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
      @passed = @options.passCardsFn this
      for cardToPass in @passed
        @held.splice(@held.indexOf(cardToPass), 1)

      $client.sendAndReceive "passCards", cards: @passed, (response) =>
        throw response if response.message != "received"
        receivedCards = response.data.cards
        @received = receivedCards
        @log "Received cards:", @received
        @held = @held.concat(@received)
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
      $client.sendAndReceive "finishedRound", {}, (response) =>
        throw response if response.message != "roundFinished"
        @log "round result:", response
        if response.data.status != "nextRound"
          callback()
        else
          @run(callback)

  log: (args...) ->
    log "P:#{@info.position}", args...

  @play: (passCardsFn, playCardFn) ->
    log "playing"

    $client.read (response) ->
      throw response if response.message != "roundStarted"
      gameInfo = response.data
      log "game info:", gameInfo

      game = new Game gameInfo,
        passCardsFn: passCardsFn
        playCardFn: playCardFn

      game.run ->
        log "Game is over"
        $client.sendAndReceive "finishedGame", {}, (response) =>
          throw response if response.message != "end"
          log "game result:", response
          process.exit()

  @Suit:
    SPADES: "spades"
    HEARTS: "hearts"
    DIAMONDS: "diamonds"
    CLUBS: "clubs"
  @Rank:
    TWO: "two"
    QUEEN: "queen"

exports.Game = Game
