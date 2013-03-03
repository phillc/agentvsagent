class Action
  constructor: (@player) ->

  # isValid: -> true

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  run: (game) ->
    console.log game
    game.currentRound.north.passedCards = @cards

exports.PlayCard = class PlayCard extends Action

