net = require 'net'


class RandomBot
  constructor: ->
    @client = new net.Socket()

  connect: ->
    console.log "connecting"
    @client.connect(4001)

    @client.on 'connect', =>
      console.log "connected to server"
      @client.write("HEYYYY")

    @client.on 'data', (data) ->
      console.log "received data", data.toString()

    @client.on 'end', ->
      console.log "server disconnected"

console.log "Starting..."
(new RandomBot()).connect()
(new RandomBot()).connect()
(new RandomBot()).connect()
(new RandomBot()).connect()

