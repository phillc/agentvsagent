Service = require './service'
MatchMaker = require './matchmaker'
Arena = require './arena'

arena = new Arena()

matchMaker = new MatchMaker(arena)
matchMaker.start()

service = new Service(arena)
service.start()

