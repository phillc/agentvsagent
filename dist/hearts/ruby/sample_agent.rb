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
    @game_info = @game.get_game_info @ticket
    puts "game info: #{@game_info.inspect}"

    loop.with_index(1) do |_, round_number|
      @round_number = round_number
      play_round

      round_result = @game.get_round_result @ticket
      puts "round result: #{round_result.inspect}"
      break if round_result.status != AgentVsAgent::GameStatus::NEXT_ROUND
    end

    game_result = @game.get_game_result @ticket
    puts "game_result: #{game_result.inspect}"
  end

  def play_round
    @hand = @game.get_hand @ticket
    puts "hand: #{@hand.inspect}"

    if @round_number % 4 != 0
      cards_to_pass = @hand.shift(3)
      puts "[#{@game_info.position}] passing cards #{cards_to_pass}"
      received_cards = @game.pass_cards @ticket, cards_to_pass
      puts "received cards: #{received_cards.inspect}"
      @hand = @hand + received_cards
    end

    13.times do |trick_number|
      @trick_number = trick_number
      play_trick
    end
  end

  def play_trick
    puts "[#{@game_info.position}, round #{@round_number}, trick #{@trick_number}, playing trick"

    trick = @game.get_trick @ticket
    puts "Leading the trick #{@game_info.inspect}, #{trick.inspect}" if @game_info.position == trick.leader
    puts "current trick: #{trick.inspect}"

    if @trick_number == 0 && two_clubs = @hand.detect{|card| card.suit == AgentVsAgent::Suit::CLUBS && card.rank == AgentVsAgent::Rank::TWO }
      puts "playing two of clubs"
      card_to_play = two_clubs
    elsif trick.played[0] && matching_suit = @hand.detect{|card| card.suit == trick.played[0].suit}
      puts "playing matching suit"
      card_to_play = matching_suit
    else
      puts "playing off suit"
      card_to_play = @hand.first
    end

    @hand.delete(card_to_play)
    puts "[#{@game_info.position}] playing card: #{card_to_play.inspect}"
    trick_result = @game.play_card @ticket, card_to_play

    puts "trick result: #{trick_result.inspect}"
  end
end


puts "Opening connection"
transport.open
bot = RandomBot.new(client)
bot.run
transport.close

puts "Finished"

