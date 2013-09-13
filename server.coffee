express = require 'express'
winston = require 'winston'
logger = require './lib/logger'

Arena = require './lib/arena'
MatchMaker = require './lib/matchMaker'

HeartsService = require './service/hearts'
HeartsBuilder = require './lib/hearts/builder'

TicTacToeService = require './service/tic_tac_toe'
TicTacToeBuilder = require './lib/tic_tac_toe/builder'

createHttp = () ->
  app = express()
  app.enable('strict routing')
  app.set 'view engine', 'jade'
  app.set 'views', __dirname + '/web/views'
  app.use '/', express.static(__dirname + '/web/public')
  app.use require("connect-assets")(src: __dirname + "/web/assets")
  app.configure 'development', ->
    app.use (req, res, next) ->
      res.locals.pretty = true
      next()

  app.get '/', (req, res) -> res.send("<a href='/game/hearts/play'>Hearts</a><br /><a href='/game/tic_tac_toe/play'>Tic Tac Toe</a>")
  return app

buildService = (serviceClass, builderClass, options) ->
  builder = new builderClass(options)
  service = new serviceClass(agentTimeout: options.turnTime || 1000)
  arena = new Arena(builder, service.handlers())
  matchMaker = new MatchMaker(arena)
  matchMaker.start()
  service

mountGame = (app, name, service, tcpPort) ->
  app.use "/game/#{name}/service.json", service.jsonHttpMiddleware()
  app.use "/game/#{name}/service.thrift", service.binaryHttpMiddleware()
  app.use "/game/#{name}/play", (req, res) ->
    res.render "#{name}/play"

  tcpServer = service.createTCPServer()
  tcpServer.listen(tcpPort)
  logger.info "TCP Server listening on", tcpServer.address()

exports.start = (options) ->
  app = createHttp()

  loggerOptions = timestamp: true, colorize: true
  if options.debug
    app.use express.logger(format: 'dev')
    loggerOptions.level = 'verbose'
    require('q').longStackSupport = true
  else
    loggerOptions.level = 'info'
  logger.add winston.transports.Console, loggerOptions

  heartsService = buildService(HeartsService, HeartsBuilder, options)
  ticTacToeService = buildService(TicTacToeService, TicTacToeBuilder, options)

  mountGame(app, "hearts", heartsService, 4001)
  mountGame(app, "tic_tac_toe", ticTacToeService, 4002)

  httpServer = app.listen(4000)
  logger.info "HTTP Server listening on", httpServer.address()

