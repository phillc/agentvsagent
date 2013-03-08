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

    loop.with_index(1) do |_, count|
      hand = @game.get_hand @ticket
      puts "hand: #{hand.inspect}"

      if count % 4 != 0
        puts "passing cards"
        received_cards = @game.pass_cards @ticket, hand.shift(3)
        puts "received cards: #{received_cards.inspect}"
        hand = hand + received_cards
      end

      13.times do
        puts "playing trick"

        trick = @game.get_trick @ticket
        puts "Leading the trick" if game_info.position == trick.leader
        puts "current trick: #{trick.inspect}"

        sleep 0.25

        @game.play_card @ticket, hand.pop
      end
      break
      # @game.get_round_results @ticket
    end
  end
end

puts "Opening connection"
transport.open
bot = RandomBot.new(client)
bot.run
transport.close

puts "Finished"

