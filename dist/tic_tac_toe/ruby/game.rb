require 'thrift'
$:.unshift File.dirname(__FILE__) + '/lib'
require 'tic_tac_toe'

class Game
  def initialize(info, options)
    @info = info
    @options = options
  end

  def run
    log "Starting game"

    loop do
      opponent_move = @options[:client].make_move @options[:ticket]
      log "opponent's move: #{opponent_move.inspect}"
      break if round_result.status != AgentVsAgent::GameStatus::NEXT_ROUND
    end
  end

  def log(message)
    puts "P:#{@info.position} #{message}"
  end

  def self.do_turn(&blk)
    @do_turn_fn = blk
  end

  def self.play
    host = ENV["AVA_HOST"] || "127.0.0.1"
    port = ENV["AVA_PORT"] || 4002
    socket = Thrift::Socket.new(host, port)
    transport = Thrift::FramedTransport.new(socket)
    protocol = Thrift::BinaryProtocol.new(transport)
    client = TicTacToe::Client.new(protocol)
    transport.open

    request = EntryRequest.new
    puts "Entering arena #{request.inspect}"
    response = client.enter_arena request
    ticket = response.ticket
    if ticket
      puts "playing"
      game_info = client.get_game_info ticket
      puts "game info: #{@game_info.inspect}"

      game = Game.new game_info, {
        ticket: ticket,
        client: client,
        pass_cards_fn: @do_turn_fn,
      }

      game.run
      puts "Game is over"
      game_result = client.get_game_result ticket
      puts "game result: #{game_result.inspect}"
    else
      puts "No ticket"
    end
    transport.close
  end

  include TicTacToe
end
