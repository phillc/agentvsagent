winston = require 'winston'
logger = require './logger'
logger.add winston.transports.Console

HeartsService = require './hearts/service'
MatchMaker = require './matchmaker'
Arena = require './arena'

arena = new Arena()

matchMaker = new MatchMaker(arena)
matchMaker.start()

service = new HeartsService(arena)
service.start()

