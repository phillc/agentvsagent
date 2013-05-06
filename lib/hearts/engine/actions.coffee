Pile = require './pile'
states = require './states'
logger = require '../../logger'
class Action
  constructor: (@player) ->

exports.PassCards = class PassCards extends Action
  constructor: (player, @cards) ->
    super(player)

  validate: (game) ->
    position = game.positionOf(@player)
    seat = game.currentRound()[position]
    if !(game.currentState instanceof states.Passing)
      return {type: "outOfSequence", message: "Action requested out of sequence."}

    if seat.passed.cards.length > 0
      return {type: "invalidMove", message: "May not pass more than once in a round."}

    uniqueCards = []
    for card in @cards
      if uniqueCards.some((uniqueCard) -> uniqueCard.isEqual(card))
        return {type: "invalidMove", message: "Must pass a card no more than once."}
      else
        uniqueCards.push(card)

    for card in @cards
      if !seat.held.cards.some((heldCard) -> heldCard.isEqual(card))
        return {type: "invalidMove", message: "Must pass cards in your hand."}

    if @cards.length != 3
      return {type: "invalidMove", message: "Must pass three cards. You passed #{@cards.length}."}
    null

  execute: (game) ->
    position = game.positionOf(@player)
    seat = game.currentRound()[position]
    logger.info "PASSING CARDS", @cards

    (new Pile(@cards)).copyAllCardsTo game.currentRound()[position].passed

exports.PlayCard = class PlayCard extends Action
  constructor: (player, @card) ->
    super(player)

  validate: (game) ->
    position = game.positionOf(@player)
    seat = game.currentRound()[position]

    if !(game.currentState instanceof states.WaitingForCard)
      return {type: "outOfSequence", message: "Action requested out of sequence."}

    if position != game.currentState.position
      return {type: "outOfSequence", message: "Card played out of sequence."}

    if !seat.held.cards.some((heldCard) => heldCard.isEqual(@card))
      return {type: "invalidMove", message: "Must play a card in your hand."}

    null

  execute: (game) ->
    logger.info "PLAYING CARD", @card
    position = game.positionOf(@player)
    # TODO: validate/shift it off of current player held
    game.currentRound().currentTrick().played.addCard(@card)
