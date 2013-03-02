Pile = require "./pile"
IdGenerator = require '../idgenerator'
types = require "../lib/hearts_types"

module.exports = class Game
  constructor: (player1, player2, player3, player4) ->
    @id = IdGenerator.generate()
    @players = [player1, player2, player3, player4]

  getPlayer: (playerId) ->
    for player in @players
      return player if player.id == playerId

  start: ->
    console.log "Starting game with players:", @players
    for player in @players
      player.emit 'start', @id
    @play()

  play: ->
    # loop until player reaches 100 (term: rounds, sequence)
      # createDeck
      # shuffle
      # deal
      # pass
      # loop 13 (term: tricks)
        # 4x (term: rotation)
          # get moves
        # get winner



  deal: (deck) ->
    deck.moveCardsTo(13, @players[0].held)
    deck.moveCardsTo(13, @players[1].held)
    deck.moveCardsTo(13, @players[2].held)
    deck.moveCardsTo(13, @players[3].held)

  # passCards: (stategy) ->



