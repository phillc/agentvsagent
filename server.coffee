net = require 'net'
express = require 'express'
winston = require 'winston'
logger = require './lib/logger'

Agent = require './lib/agent'
Entrance = require './lib/entrance'
Arena = require './lib/arena'
MatchMaker = require './lib/matchMaker'

HeartsBuilder = require './lib/hearts/builder'

createHttp = ->
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

  app.get '/', (req, res) ->
    res.send """
      <a href='/game/hearts/play'>Hearts</a>
      <br />
    """
  return app

createIoGameServer = (httpServer, entrance) ->
  io = require('socket.io')(httpServer)

createTcpGameServer = (tcpServer, entrance, options) ->
  #TODO: Yuck
  tcpServer.on 'connection', (socket) ->
    agent = new Agent(timeout: options.turnTime || 1000)
    entrance.addAgent(agent)

    agentOnSuccess = (data) ->
      socket.write(JSON.stringify(data))
      socket.write("\n")
      if data.message == "end"
        socket.end()
    agent.out.on 'success', agentOnSuccess

    agentOnFailure = (data) ->
      socket.write(JSON.stringify(data))
      socket.write("\n")
      socket.end()
    agent.out.on 'failure', agentOnFailure

    processLine = (line) ->
      console.log "PROCESSLINE", line
      try
        #TODO: yuck
        parsed = JSON.parse(line)
      catch e
        agent.forward("errored", "bad json")

      if parsed
        agent.forward(parsed.message, parsed.data)

    buffer = ''
    socket.on 'data', (data) ->
      lines = data.toString().split("\n")
      lines[0] = buffer + lines[0]
      buffer = ''
      if lines[lines.length - 1] != ""
        buffer = lines[lines.length - 1]
        lines.splice(lines.length - 1, 1)

      for line in lines
        if line != ''
          processLine(line)

    socket.on 'end', ->
      console.log 'disconnected'
      agent.out.removeListener 'success', agentOnSuccess
      agent.out.removeListener 'failure', agentOnFailure

buildArena = (builderClass, options) ->
  builder = new builderClass(options)
  arena = new Arena(builder)
  matchMaker = new MatchMaker(arena, 1000) #10000) #TODO: parameterize
  matchMaker.start()
  arena

exports.start = (options) ->
  app = createHttp()

  loggerOptions = timestamp: true, colorize: true
  if options.debug
    app.use express.logger(format: 'dev')
    loggerOptions.level = 'verbose'
  else
    loggerOptions.level = 'info'
  logger.add winston.transports.Console, loggerOptions

  logger.info "Starting Agent vs Agent server, version #{require('./package.json').version}"

  httpServer = app.listen(4000)
  logger.info "HTTP Server listening on", httpServer.address()

  tcpServer = net.createServer()
  tcpServer.listen(4001)
  logger.info "TCP Server listening on", tcpServer.address()

  heartsArena = buildArena(HeartsBuilder, options)

  ["hearts"].forEach (name) ->
    app.use "/game/#{name}/play", (req, res) ->
      res.render "#{name}/play"

  entrance = new Entrance
    hearts: heartsArena
  createIoGameServer(httpServer, entrance)
  createTcpGameServer(tcpServer, entrance, options)

