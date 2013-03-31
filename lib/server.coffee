express = require 'express'
winston = require 'winston'
logger = require './logger'
logger.add winston.transports.Console

HeartsService = require './hearts/service'
MatchMaker = require './matchmaker'
Arena = require './arena'

arena = new Arena()

matchMaker = new MatchMaker(arena)
matchMaker.start()

{tcpServer, binaryHttpMiddleware, jsonHttpMiddleware} = new HeartsService(arena).create()

app = express()

app.set 'view engine', 'jade'
app.configure 'development', ->
  app.use (req, res, next) ->
    res.locals.pretty = true
    next()

app.use express.logger(format: 'dev')
app.use '/game/hearts/service.json', jsonHttpMiddleware
app.use '/game/hearts/service.thrift', binaryHttpMiddleware
app.use '/game/hearts', require('connect-assets')(src: 'lib/hearts/web/assets', servePath: '/game/hearts')
app.use '/game/hearts', require('./hearts/web').app()
app.get '/', (req, res) -> res.send("<a href='/game/hearts/play'>Hearts</a>")

tcpServer.listen(4001)
logger.info "TCP Server listening on", tcpServer.address()
httpServer = app.listen(4000)
logger.info "HTTP Server listening on", httpServer.address()
