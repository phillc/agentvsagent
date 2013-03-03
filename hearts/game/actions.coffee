Pile = require './pile'
class Action
  constructor: (@player) ->

  # isValid: -> true

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  execute: (game) ->
    position = game.positionOf(@player)
    (new Pile(@cards)).copyAllCardsTo game.currentRound[position].passed

exports.PlayCard = class PlayCard extends Action
  constructor: (player, @card) ->
    console.log "Constructor", player
    super(player)

  execute: (game) ->
    position = game.positionOf(@player)
    game.currentRound.tricks[0][position] = @card
