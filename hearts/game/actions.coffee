Pile = require './pile'
class Action
  constructor: (@player) ->

  # isValid: -> true

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  run: (game) ->
    position = @game.positionOf(player)
    (new Pile(@cards)).copyAllCardsTo game.currentRound[position].passed

exports.PlayCard = class PlayCard extends Action

