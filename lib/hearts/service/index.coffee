thrift = require 'thrift'
logger = require '../../logger'
Hearts = require './types/hearts'
Handler = require './handler'

module.exports = class Service
  constructor: (@arena) ->

  create: ->
    handler = new Handler(@arena)

    tcpServer: thrift.createServer(Hearts, handler)
    httpMiddleware: thrift.httpMiddleware(Hearts, handler)

