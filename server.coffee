express = require 'express'
winston = require 'winston'
logger = require './lib/logger'

Arena = require './lib/arena'
MatchMaker = require './lib/matchMaker'

HeartsService = require './service/hearts'
HeartsFactory = require './lib/hearts/factory'

TicTacToeService = require './service/tic_tac_toe'
TicTacToeFactory = require './lib/tic_tac_toe/factory'

mountGame = (name, app, service, factory, tcpPort, options) ->
  factory = new factory(options)
  arena = new Arena(factory)

  matchMaker = new MatchMaker(arena)
  matchMaker.start()

  {tcpServer, binaryHttpMiddleware, jsonHttpMiddleware} = new service(arena).create()

  app.use "/game/#{name}/service.json", jsonHttpMiddleware
  app.use "/game/#{name}/service.thrift", binaryHttpMiddleware
  app.use "/game/#{name}", require("./web/#{name}").app()

  tcpServer.listen(tcpPort)
  logger.info "TCP Server listening on", tcpServer.address()

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

  app.get '/', (req, res) -> res.send("<a href='/game/hearts/play'>Hearts</a><a href='/game/tic_tac_toe/play'>Tic Tac Toe</a>")

  mountGame("hearts", app, HeartsService, HeartsFactory, 4001, options)
  mountGame("tic_tac_toe", app, TicTacToeService, TicTacToeFactory, 4002, options)

  logger.verbose "OPTIONS", options
  httpServer = app.listen(4000)
  logger.info "HTTP Server listening on", httpServer.address()
