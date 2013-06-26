thrift = require 'thrift'
logger = require '../../lib/logger'
TicTacToe = require './types/TicTacToe'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->
    @handler = new Handler(@arena)

  createTCPServer: -> thrift.createServer(TicTacToe, @handler)
  binaryHttpMiddleware: -> thrift.httpMiddleware(TicTacToe, @handler)
  jsonHttpMiddleware: -> thrift.httpMiddleware(TicTacToe, @handler, protocol: thrift.TJSONProtocol)

