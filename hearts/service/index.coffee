thrift = require 'thrift'
Hearts = require '../lib/hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (arena) ->
    @server = thrift.createServer Hearts, new Handler(arena)

  start: ->
    @server.listen(4001)
    console.log "Service listening on", @server.address()

