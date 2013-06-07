express = require 'express'
winston = require 'winston'
logger = require './logger'

exports.start = (options) ->
  app = express()

  app.set 'view engine', 'jade'
  app.configure 'development', ->
    app.use (req, res, next) ->
      res.locals.pretty = true
      next()

  loggerOptions = timestamp: true, colorize: true
  if options.debug
    app.use express.logger(format: 'dev')
    loggerOptions.level = 'verbose'
  else
    loggerOptions.level = 'info'
  logger.add winston.transports.Console, loggerOptions

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

  logger.verbose "OPTIONS", options
  tcpServer.listen(4001)
  logger.info "TCP Server listening on", tcpServer.address()
  httpServer = app.listen(4000)
  logger.info "HTTP Server listening on", httpServer.address()
