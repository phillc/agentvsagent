thrift = require('thrift');

Hearts = require('./lib/Hearts')

connection = thrift.createConnection('localhost', 4001)
client = thrift.createClient(Hearts, connection)


class RandomBot
  constructor: (@game) ->

  run: ->
    @game.get_game (err, response) =>
      @play

  play: ->
    @game.get_hand (err, response) =>
      @game.pass_cards cards, (err, response) =>
        #13 x
        @game.get_trick (err, response) =>
          @game.play_card card, (err, response) =>
            connection.end()



bot = RandomBot.new(client)
bot.run

