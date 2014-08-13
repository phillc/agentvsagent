require 'json'

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

class Client
  def send_command(command)
    $stdout << JSON.generate(command)
    $stdout << "\n"
    $stdout.flush
  end

  def read
    response = JSON.parse($stdin.gets.strip)
    if response["message"] == "error"
      puts "ERROR:", response
      exit
    end
    response
  end

  def send_and_receive(message, data={})
    send_command({message: message, data: data})
    read
  end
end

$client = Client.new

class Game
  attr_reader :all_moves, :recent_moves, :hands, :piles, :available_hints

  def initialize(info, move_fn)
    @position = info.position
    @move_fn = move_fn
    @hands = info.hands
    @all_moves = []
    @piles = %w(white blue green yellow red).each.with_object({}) do |suit, acc|
      acc[suit] = 0
    end

    @available_hints = 8
  end

  def run
    log "Starting game"
    moves = $client.send_and_receive("ready")["data"]["moves"]
    @recent_moves = moves
    loop do
      process_moves(@recent_moves)
      @all_moves = @all_moves + @recent_moves
      move = @move_fn.(self)
      data = $client.send_and_receive("move", move)["data"]
      moves = data["moves"]
      @recent_moves = moves
      break if data["status"] != "continue"
    end
  end

  def process_moves(moves)
    moves.each do |move|
      if move["hint"]
        @available_hints = @available_hints - 1
      elsif move["discard"]
        @available_hints = @available_hints + 1
      else
        raise "Unexpected move"
      end
    end
  end

  def log(message)
    puts "P:#{@position} #{message}"
  end

  def self.do_move(&blk)
    @move_fn = blk
  end

  def self.play
    puts "playing"

    response = $client.read
    game_info = OpenStruct.new(response["data"])
    puts "game info: #{game_info.inspect}"

    game = Game.new game_info, @move_fn

    game.run

    # puts "Game is over"
    # game_result = $client.send_and_receive("finishedGame")["data"]
    # puts "game result: #{game_result.inspect}"
  end
end

