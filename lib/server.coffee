express = require 'express'
winston = require 'winston'
logger = require './logger'

exports.start = (options) ->
  logger.add winston.transports.Console
  app = express()

  app.set 'view engine', 'jade'
  app.configure 'development', ->
    app.use (req, res, next) ->
      res.locals.pretty = true
      next()

  app.use express.logger(format: 'dev')
  app.get '/', (req, res) -> res.send("<a href='/game/hearts/play'>Hearts</a>")

  HeartsService = require './hearts/service'
  MatchMaker = require './matchmaker'
  Arena = require './arena'
  HeartsFactory = require './hearts/factory'

  factory = new HeartsFactory(options)
  arena = new Arena(factory)

  matchMaker = new MatchMaker(arena)
  matchMaker.start()

  {tcpServer, binaryHttpMiddleware, jsonHttpMiddleware} = new HeartsService(arena).create()

  app.use '/game/hearts/service.json', jsonHttpMiddleware
  app.use '/game/hearts/service.thrift', binaryHttpMiddleware
  app.use '/game/hearts', require('connect-assets')(src: __dirname + '/hearts/web/assets', servePath: '/game/hearts')
  app.use '/game/hearts', require('./hearts/web').app()

  console.log "OPTIONS", options
  tcpServer.listen(4001)
  logger.info "TCP Server listening on", tcpServer.address()
  httpServer = app.listen(4000)
  logger.info "HTTP Server listening on", httpServer.address()
