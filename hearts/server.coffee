net = require 'net'

openSockets = []
matchmake = ->
  if openSockets.length == 4
    console.log "starting game"
  else
    console.log "only have #{openSockets.length}"

tcpServer = net.createServer()
tcpServer.listen(4001)

tcpServer.on 'connection', (socket) ->
  console.log "Connected"

  socket.on 'end', ->
    console.log "client disconnected"

  socket.on 'close', ->
    console.log "connection closed"
    openSockets.splice(openSockets.indexOf(socket), 1)

  socket.on 'data', (data) ->
    console.log "data received", data.toString()
    socket.write "YOU SAID THIS: #{data}"

  openSockets.push socket
  matchmake()
