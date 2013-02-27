require 'thrift'
$:.unshift File.dirname(__FILE__) + '/lib'
require 'hearts'

socket = Thrift::Socket.new('127.0.0.1', 4001)
transport = Thrift::FramedTransport.new(socket)
protocol = Thrift::BinaryProtocol.new(transport)
client = Hearts::Client.new(protocol)

transport.open

puts "RESPONSE: #{client.get_game}"

class RandomBot
  def initialize game
    @game = game
  end

  def run
    @game.get_game
    play
    transport.close
  end

  def play
    hand = @game.get_hand
    received_cards = @game.pass_cards hand.first(3)
    trick = @game.get_trick
    @game.play_card hand.first
  end
end

bot = RandomBot.new(client)
bot.run


