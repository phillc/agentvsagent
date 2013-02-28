service = require './service'
MatchMaker = require './matchmaker'

matchMaker = new MatchMaker

serviceServer = service.createServer(matchMaker)
serviceServer.listen(4001)

console.log "Service listening on", serviceServer.address()
