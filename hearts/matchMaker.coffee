module.exports = class MatchMaker
  constructor: (@arena) ->
    # @arena.on 'newPlayer', @findMatch

  findMatch: ->
    if @arena.waitingRoom.length >= 4
      @arena.createMatch @arena.waitingRoom[0..3]


