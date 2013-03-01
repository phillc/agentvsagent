module.exports = class MatchMaker
  constructor: (@arena) ->

  findMatch: ->
    console.log "#{@arena.waitingRoom.length} players waiting."
    if @arena.waitingRoom.length >= 4
      console.log "Starting a game!"
      @arena.createMatch @arena.waitingRoom[0..3]

  start: ->
    @arena.on 'newPlayer', =>
      @findMatch()

