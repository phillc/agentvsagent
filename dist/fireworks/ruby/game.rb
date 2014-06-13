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
    JSON.parse($stdin.gets.strip)
  end

  def send_and_receive(message, data={})
    send_command({message: message, data: data})
    read
  end
end

$client = Client.new

class Game
  def initialize(info, options)
    @info = info
    @options = options
  end

  def run
    log "Starting game"
  end

  def log(message)
    puts "P:#{@info.position} #{message}"
  end

  def self.do_move(&blk)
    @move_fn = blk
  end

  def self.play
    puts "playing"

    response = $client.read
    game_info = OpenStruct.new(response["data"])
    puts "game info: #{game_info.inspect}"

    game = Game.new game_info, {
      pass_cards_fn: @pass_cards_fn,
      play_card_fn: @play_card_fn
    }

    game.run

    puts "Game is over"
    game_result = $client.send_and_receive("finishedGame")["data"]
    puts "game result: #{game_result.inspect}"
  end
end

