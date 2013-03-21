express = require 'express'

exports.app = ->
  app = express()
  app.enable('strict routing')
  app.set 'views', __dirname + '/views'
  app.use '/lib', express.static(__dirname + '/public')

  app.get '/home', index
  app.get '/play', play
  app

index = (req, res) ->
  res.render 'index'

play = (req, res) ->
  res.render 'play'


