thrift = require 'thrift'
Hearts = require './lib/hearts'
Handler = require './handler'

exports.createServer = (matchMaker) ->
    thrift.createServer Hearts, new Handler(matchMaker)

