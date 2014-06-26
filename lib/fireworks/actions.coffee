exports.Discard = class Discard
  @build: (data) ->
    new Discard(data.discard)

  constructor: (@slot) ->

  validate: (game, position) ->
    null

  execute: (game, position) ->
    game.deck.moveFirstCardToSlot(@slot, game.seats[position])
    #Adds it to the messages stack for each player?
    # or adds to a general move set, interpreted

exports.Hint = class Hint
  @build: (data) ->
    if data.hint.rank
      new RankHint(data.hint.rank)
    else if data.hint.suit
      new SuitHint(data.hint.suit)

  validate: (game, position) ->
    null

exports.SuitHint = class SuitHint extends Hint
  constructor: (position, suit) ->

  execute: (game, position) ->

exports.RankHint = class RankHint extends Hint
  constructor: (position, rank) ->

  execute: (game, position) ->
