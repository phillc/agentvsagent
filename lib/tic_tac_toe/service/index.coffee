thrift = require 'thrift'
logger = require '../../logger'
TicTacToe = require './types/TicTacToe'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->

  create: ->
    handler = new Handler(@arena)

    tcpServer: thrift.createServer(TicTacToe, handler)
    binaryHttpMiddleware: thrift.httpMiddleware(TicTacToe, handler)
    jsonHttpMiddleware: thrift.httpMiddleware(TicTacToe, handler, protocol: thrift.TJSONProtocol)

