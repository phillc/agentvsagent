def puts(*args)
  $stderr.puts *args
end

def p(*args)
  args.map!{|arg| arg.inspect}
  puts args
end

def print(*args)
  $stderr.print *args
end

class Game
  def initialize(info, options)
    @info = info
    @options = options
  end

  def run
    log "Starting game"

    loop do
      round = create_round

      round.run

      round_result = @options[:client].get_round_result @options[:ticket]
      log "round result: #{round_result.inspect}"
      break if round_result.status != AgentVsAgent::GameStatus::NEXT_ROUND
    end
  end

  def log(message)
    puts "P:#{@info.position} #{message}"
  end

  def self.do_move(&blk)
    @move_fn = blk
  end

  def self.play
    puts "Entering arena"
    send_command "FOO"
    # response = client.enter_arena request
    # ticket = response.ticket
    # if ticket
    #   puts "playing"
    #   game_info = client.get_game_info ticket
    #   puts "game info: #{@game_info.inspect}"

    #   game = Game.new game_info, {
    #     ticket: ticket,
    #     client: client,
    #     pass_cards_fn: @pass_cards_fn,
    #     play_card_fn: @play_card_fn
    #   }

    #   game.run
    #   puts "Game is over"
    #   game_result = client.get_game_result ticket
    #   puts "game result: #{game_result.inspect}"
    # else
    #   puts "No ticket"
    # end
    # transport.close
  # rescue ::Thrift::Exception => e
  #   puts "Game ended in an exception"
  #   puts e.message
  end

  def self.send_command(command)
    $stdout << command
    $stdout << "\n"
    $stdout.flush
  end
end

