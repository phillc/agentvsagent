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

{tcpServer, httpMiddleware} = new HeartsService(arena).create()
web = require './hearts/web'

app = express()
app.use "/games/hearts/service", httpMiddleware
app.get "/", (req, res) -> res.send("Home")
app.get "/games/hearts", web.index
app.get "/games/hearts/play", web.play

tcpServer.listen(4001)
logger.info "TCP Server listening on", tcpServer.address()
httpServer = app.listen(4000)
logger.info "HTTP Server listening on", httpServer.address()
