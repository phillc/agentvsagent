require 'thrift'
$:.unshift File.dirname(__FILE__) + '/lib'
require 'hearts'

socket = Thrift::Socket.new('127.0.0.1', 4001)
transport = Thrift::FramedTransport.new(socket)
protocol = Thrift::BinaryProtocol.new(transport)
client = AgentVsAgent::Hearts::Client.new(protocol)


class RandomBot
  def initialize game
    @game = game
  end

  def run
    @agent = @game.enter_arena
    play
  end

  def play
    hand = @game.get_hand @agent
    puts hand.inspect
    # received_cards = @game.pass_cards @agent, hand.first(3)
    # 13.times {
    #   trick = @game.get_trick @agent
    #   @game.play_card @agent, hand.first
    # }
    # @game.get_round_results @agent
  end
end

transport.open
bot = RandomBot.new(client)
bot.run
transport.close


