thrift = require 'thrift'
logger = require '../../lib/logger'
Hearts = require './types/Hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (options) ->
    @tcpHandler = new Handler(agentTimeout: options.agentTimeout)
    @binaryHandler = new Handler(agentTimeout: options.agentTimeout)
    @jsonHandler = new Handler()

  handlers: ->
    [@tcpHandler, @binaryHandler, @jsonHandler]

  createTCPServer: -> thrift.createServer(Hearts, @tcpHandler, transport: thrift.TFramedTransport)
  binaryHttpMiddleware: -> thrift.httpMiddleware(Hearts, @binaryHandler, transport: thrift.TFramedTransport)
  jsonHttpMiddleware: -> thrift.httpMiddleware(Hearts, @jsonHandler, protocol: thrift.TJSONProtocol)

