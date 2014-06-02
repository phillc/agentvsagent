net = require 'net'
logger = require '../../lib/logger'
Handler = require './handler'

module.exports = class Service
  constructor: (options) ->
    @handler = new Handler()

  handlers: ->
    [@handler]

  createTCPServer: ->
    net.createServer (socket) =>
      socket.write 'Heya!\n'

      processLine = (line) ->
        console.log "line: ", line

      buffer = ''
      socket.on 'data', (data) =>
        console.log "DATA!", data.toString()
        lines = data.toString().split("\n")
        lines[0] = buffer + lines[0]
        buffer = ''
        if lines[lines.length - 1] != ""
          buffer = lines[lines.length - 1]
          lines.splice(lines.length - 1, 1)

        for line in lines
          processLine(line)

      socket.on 'end', ->
        console.log 'disconnected'

