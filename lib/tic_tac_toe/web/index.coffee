express = require 'express'

exports.app = ->
  app = express()
  app.enable('strict routing')
  app.set 'views', __dirname + '/views'
  app.use '/lib', express.static(__dirname + '/public')

  app.get '/play', play
  app

play = (req, res) ->
  res.render 'play'


