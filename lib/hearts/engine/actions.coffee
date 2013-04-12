Pile = require './pile'
logger = require '../../logger'
class Action
  constructor: (@player) ->

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  execute: (game, callback) ->
    position = game.positionOf(@player)
    logger.info "PASSING CARDS", @cards

    if @cards.length != 3
      return callback(["invalidMove", "Must pass three cards. You passed #{@cards.length}."])

    (new Pile(@cards)).copyAllCardsTo game.currentRound()[position].passed

    callback()

exports.PlayCard = class PlayCard extends Action
  constructor: (player, @card) ->
    super(player)

  execute: (game, callback) ->
    logger.info "PLAYING CARD", @card
    position = game.positionOf(@player)
    # TODO: validate/shift it off of current player held
    game.currentRound().currentTrick().played.addCard(@card)
    callback()
