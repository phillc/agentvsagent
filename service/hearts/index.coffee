thrift = require 'thrift'
logger = require '../../lib/logger'
Hearts = require './types/Hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->

  create: ->
    handler = new Handler(@arena)

    tcpServer: thrift.createServer(Hearts, handler)
    binaryHttpMiddleware: thrift.httpMiddleware(Hearts, handler)
    jsonHttpMiddleware: thrift.httpMiddleware(Hearts, handler, protocol: thrift.TJSONProtocol)

