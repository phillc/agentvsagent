exports.Discard = class Discard
  @build: (data) ->
    new Discard(data.discard)

  constructor: (@slot) ->

  validate: (game, position) ->
    if game.deck.isEmpty()
      return {type: "invalidMove", message: "Out of cards to draw."}
    else if game.hints >= game.maxHints
      return {type: "invalidMove", message: "Out of hints to receive."}
    else
      return null

  execute: (game, position) ->
    game.deck.moveFirstCardToSlot(@slot, game.seats[position])
    game.hints = game.hints + 1
    #Adds it to the messages stack for each player?
    # or adds to a general move set, interpreted

exports.Hint = class Hint
  @build: (data) ->
    if data.hint.rank
      new RankHint(data.hint.rank)
    else if data.hint.suit
      new SuitHint(data.hint.suit)

  execute: (game, position) ->
    game.hints = game.hints - 1

  validate: (game, position) ->
    if game.hints <= 0
      return {type: "invalidMove", message: "Out of hints."}
    else
      return null

exports.SuitHint = class SuitHint extends Hint
  constructor: (position, suit) ->

  execute: (game, position) ->
    super(game, position)

exports.RankHint = class RankHint extends Hint
  constructor: (position, rank) ->

  execute: (game, position) ->
    super(game, position)
