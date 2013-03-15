thrift = require 'thrift'
logger = require '../../logger'
Hearts = require './types/hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (arena) ->
    @server = thrift.createServer Hearts, new Handler(arena)

  start: ->
    @server.listen(4001)
    logger.info "Service listening on", @server.address()

