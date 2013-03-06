Pile = require './pile'
class Action
  constructor: (@player) ->

  # isValid: -> true

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  execute: (game) ->
    position = game.positionOf(@player)
    console.log "PASSING CARDS", @player.id, @cards
    (new Pile(@cards)).copyAllCardsTo game.currentRound()[position].passed

exports.PlayCard = class PlayCard extends Action
  constructor: (player, @card) ->
    super(player)

  execute: (game) ->
    console.log "PLAYING CARD", @player.id, @card
    position = game.positionOf(@player)
    game.currentRound().tricks[0][position] = @card
