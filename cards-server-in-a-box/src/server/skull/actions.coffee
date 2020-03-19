logger = require '../logger'

exports.PlayCard = class PlayCard
  @build: (data) ->
    new PlayCard(data.cards.map (jsonCard) -> Card.fromJSON(jsonCard))

  constructor: (@cards) ->

  validate: (game, position) ->
    # if you have no cards, you must challenge

exports.Bid = class Bid
  @build: (data) ->
    new Bid(data.cards.map (jsonCard) -> Card.fromJSON(jsonCard))

  constructor: (@cards) ->

  validate: (game, position) ->
    # can't do this in first round of game
