Pile = require './pile'
logger = require '../../logger'
class Action
  constructor: (@player) ->

  # isValid: -> true

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  execute: (game) ->
    position = game.positionOf(@player)
    logger.info "PASSING CARDS", @player.id, @cards
    (new Pile(@cards)).copyAllCardsTo game.currentRound()[position].passed

exports.PlayCard = class PlayCard extends Action
  constructor: (player, @card) ->
    super(player)

  execute: (game) ->
    logger.info "PLAYING CARD", @player.id, @card
    position = game.positionOf(@player)
    # TODO: validate/shift it off of current player held
    game.currentRound().currentTrick().played.addCard(@card)
