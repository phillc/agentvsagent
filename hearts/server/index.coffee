thrift = require 'thrift'
Hearts = require './lib/Hearts'

server = thrift.createServer Hearts,
  get_game: (result) ->
    console.log "get_game called"
    result null, true
  # play_card: (card, result) ->
  #   console.log "play_card called"
  #   result null, false

server.listen(4001)
console.log "Thrift listening on", server.address()
