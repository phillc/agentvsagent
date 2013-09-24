winston = require 'winston'
logger = require './lib/logger'

Arena = require './lib/arena'
MatchMaker = require './lib/matchMaker'

HeartsService = require './service/hearts'
HeartsBuilder = require './lib/hearts/builder'

TicTacToeService = require './service/tic_tac_toe'
TicTacToeBuilder = require './lib/tic_tac_toe/builder'

loadSails = (callback) ->
  sailsOptions = {
    ava: {
      port: 4000,
      http: true
    },
    log: {
      # level: 'silent'
    }
  }
  require('sails').lift sailsOptions, ->
    logger.info "HTTP Server listening on", sails.config.port
    callback()

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
  loggerOptions = timestamp: true, colorize: true
  if options.debug
    # app.use express.logger(format: 'dev')
    loggerOptions.level = 'verbose'
    require('q').longStackSupport = true
  else
    loggerOptions.level = 'info'
  logger.add winston.transports.Console, loggerOptions

  logger.info "Starting Agent vs Agent server, version #{require('./package.json').version}"

  loadSails ->

    heartsService = buildService(HeartsService, HeartsBuilder, options)
    ticTacToeService = buildService(TicTacToeService, TicTacToeBuilder, options)

    # mountGame(app, "hearts", heartsService, 4001)
    # mountGame(app, "tic_tac_toe", ticTacToeService, 4002)
    logger.info "Finished starting server"

