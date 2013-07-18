thrift = require 'thrift'
logger = require '../../lib/logger'
TicTacToe = require './types/TicTacToe'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->
    @tcpHandler = new Handler()
    @binaryHandler = new Handler()
    @jsonHandler = new Handler()

  handlers: ->
    [@tcpHandler, @binaryHandler, @jsonHandler]

  createTCPServer: -> thrift.createServer(TicTacToe, @tcpHandler)
  binaryHttpMiddleware: -> thrift.httpMiddleware(TicTacToe, @binaryHandler)
  jsonHttpMiddleware: -> thrift.httpMiddleware(TicTacToe, @jsonHandler, protocol: thrift.TJSONProtocol)

