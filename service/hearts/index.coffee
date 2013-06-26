thrift = require 'thrift'
logger = require '../../lib/logger'
Hearts = require './types/Hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->
    @handler = new Handler(@arena)

  createTCPServer: -> thrift.createServer(Hearts, @handler)
  binaryHttpMiddleware: -> thrift.httpMiddleware(Hearts, @handler)
  jsonHttpMiddleware: -> thrift.httpMiddleware(Hearts, @handler, protocol: thrift.TJSONProtocol)

