Pile = require './pile'
Card = require './card'
Rank = require './rank'
Suit = require './suit'
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

    validateState = ->
      if !(game.currentState instanceof states.Passing)
        {type: "outOfSequence", message: "Action requested out of sequence."}

    validatePassingAllowed = ->
      if seat.passed.cards.length > 0
        return {type: "invalidMove", message: "May not pass more than once in a round."}

    validateUniqueCards = =>
      uniqueCards = []
      for card in @cards
        if uniqueCards.some((uniqueCard) -> uniqueCard.isEqual(card))
          return {type: "invalidMove", message: "Must pass a card no more than once."}
        else
          uniqueCards.push(card)

    validateOwnCards = =>
      for card in @cards
        if !seat.held.cards.some((heldCard) -> heldCard.isEqual(card))
          return {type: "invalidMove", message: "Must pass cards in your hand."}

    validateNumberCards = =>
      if @cards.length != 3
        return {type: "invalidMove", message: "Must pass three cards. You passed #{@cards.length}."}

    validateState() ||
      validatePassingAllowed() ||
      validateUniqueCards() ||
      validateOwnCards() ||
      validateNumberCards() ||
      null

  execute: (game) ->
    position = game.positionOf(@player)
    seat = game.currentRound()[position]
    logger.verbose "PASSING CARDS", @cards

    (new Pile(@cards)).copyAllCardsTo game.currentRound()[position].passed

exports.PlayCard = class PlayCard extends Action
  constructor: (player, @card) ->
    super(player)

  validate: (game) ->
    position = game.positionOf(@player)
    round = game.currentRound()
    seat = round[position]
    trick = round.currentTrick()

    validateState = ->
      if !(game.currentState instanceof states.WaitingForCard)
        return {type: "outOfSequence", message: "Action requested out of sequence."}

    validatePlayer = ->
      if position != game.currentState.position
        return {type: "outOfSequence", message: "Card played out of sequence."}

    validateOwnCard = =>
      if !seat.held.cards.some((heldCard) => heldCard.isEqual(@card))
        return {type: "invalidMove", message: "Must play a card in your hand."}

    validateTwoClubs = =>
      if !@card.isEqual(new Card(Suit.CLUBS, Rank.TWO))
        return {type: "invalidMove", message: "Must lead round with two of clubs."}

    validateNoPoints = =>
      if @card.score() > 0 && seat.held.cards.some((heldCard) => heldCard.score() == 0)
        return {type: "invalidMove", message: "Must not play points in the first trick of a round."}

    validateHeartsBroken = =>
      if @card.suit == Suit.HEARTS && !round.isHeartsBroken() && seat.held.allOfSuit(Suit.HEARTS).cards.length != seat.held.cards.length
        return {type: "invalidMove", message: "Must not play a heart until broken."}

    validateFollowingSuit = =>
      if @card.suit != trick.played.cards[0].suit && !seat.held.allOfSuit(trick.played.cards[0].suit).isEmpty()
        return {type: "invalidMove", message: "Must follow suit."}

    validateTrickCard = =>
      if trick.played.isEmpty()
        validateHeartsBroken()
      else
        validateFollowingSuit()

    validateFirstTrickCard = =>
      if trick.played.isEmpty()
        validateTwoClubs()
      else
        validateFollowingSuit() || validateNoPoints()

    validateCard = =>
      if round.tricks.length == 1
        validateFirstTrickCard()
      else
        validateTrickCard()

    validateState() ||
      validatePlayer() ||
      validateOwnCard() ||
      validateCard() ||
      null

  execute: (game) ->
    logger.verbose "PLAYING CARD", @card
    position = game.positionOf(@player)
    round = game.currentRound()

    round[position].held.moveCardTo(@card, round.currentTrick().played)
