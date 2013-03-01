module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    console.log "#{@arena.waitingRoom.length} players waiting."
    if @arena.waitingRoom.length >= 4
      console.log "Match maker is creating a game!"
      @arena.createGame @arena.waitingRoom[0..3]

  start: ->
    @arena.on 'newPlayer', =>
      @findMatch()

