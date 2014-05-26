thrift = require 'thrift'
logger = require '../../lib/logger'
Fireworks = require './types/Fireworks'
Handler = require './handler'

module.exports = class Service
  constructor: (options) ->
    @tcpHandler = new Handler(agentTimeout: options.agentTimeout)
    @binaryHandler = new Handler(agentTimeout: options.agentTimeout)
    @jsonHandler = new Handler()

  handlers: ->
    [@tcpHandler, @binaryHandler, @jsonHandler]

  createTCPServer: -> thrift.createServer(Fireworks, @tcpHandler, transport: thrift.TFramedTransport)
  binaryHttpMiddleware: -> thrift.httpMiddleware(Fireworks, @binaryHandler, transport: thrift.TFramedTransport)
  jsonHttpMiddleware: -> thrift.httpMiddleware(Fireworks, @jsonHandler, protocol: thrift.TJSONProtocol)

