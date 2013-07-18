thrift = require 'thrift'
logger = require '../../lib/logger'
Hearts = require './types/Hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->
    @tcpHandler = new Handler()
    @binaryHandler = new Handler()
    @jsonHandler = new Handler()

  handlers: ->
    [@tcpHandler, @binaryHandler, @jsonHandler]

  createTCPServer: -> thrift.createServer(Hearts, @tcpHandler)
  binaryHttpMiddleware: -> thrift.httpMiddleware(Hearts, @binaryHandler)
  jsonHttpMiddleware: -> thrift.httpMiddleware(Hearts, @jsonHandler, protocol: thrift.TJSONProtocol)

