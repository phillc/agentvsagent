Service = require './service'
MatchMaker = require './matchmaker'
Arena = require './arena'
IdGenerator = require './idgenerator'

idGenerator = new IdGenerator()
arena = new Arena(idGenerator)

# matchMaker = new MatchMaker()
service = new Service(arena)
service.start()

