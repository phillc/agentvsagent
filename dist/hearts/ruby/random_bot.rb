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
    puts "Entering arena"
    response = @game.enter_arena
    @ticket = response.ticket
    if @ticket
      puts "Got a ticket! #{@ticket.inspect}"
      play
    end
  end

  def play
    puts "playing"
    game_info = @game.get_game_info @ticket
    puts "game info: #{game_info.inspect}"
    hand = @game.get_hand @ticket
    puts "hand: #{hand.inspect}"
    received_cards = @game.pass_cards @ticket, hand.first(3)
    puts "received cards: #{received_cards.inspect}"
    # 13.times {
    #   trick = @game.get_trick @agent
    #   @game.play_card @agent, hand.first
    # }
    # @game.get_round_results @agent
  end
end

puts "Opening connection"
transport.open
bot = RandomBot.new(client)
bot.run
transport.close


