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

# class Card
#   attr_reader :suit, :rank
# 
#   def initialize(json)
#     @json = json
#     @suit = json["suit"]
#     @rank = json["rank"]
#   end
# 
#   def to_json(options={})
#     JSON.generate(@json)
#   end
# 
#   def to_s
#     "#{@rank} of #{@suit}"
#   end
# 
#   def ==(other)
#     @rank == other.rank && @suit == other.suit
#   end
#   alias_method :eql?, :==
# end
# 
# class Trick
#   attr_reader :number, :round, :leader, :played
#   def initialize(number, round, options)
#     @number = number
#     @round = round
#     @options = options
#     @leader = nil
#     @played = []
#   end
# 
#   def run
#     log "Starting trick"
#     trick = $client.send_and_receive("readyForTrick")["data"]
#     @leader = trick["leader"]
#     @played = trick["played"].map{|c| Card.new(c)}
# 
#     card_to_play = @options[:play_card_fn].(self)
#     @round.held.delete(card_to_play)
#     trick_result = $client.send_and_receive("playCard", card: card_to_play)["data"]
#     log "trick: result #{trick_result.inspect}"
#     @played = trick_result["played"].map{|c| Card.new(c)}
#   end
# 
#   def log(message)
#     @round.log "T:#{@number} #{message}"
#   end
# end

class Round
  attr_reader :number, :game #, :tricks, :dealt, :held
  def initialize(number, game, options)
    @number = number
    @game = game
    @options = options

    # @tricks = []
    # @dealt = []
    # @passed = []
    # @received = []
    # @held = []
  end

  # def create_trick
  #   trick_number = @tricks.size + 1
  #   trick = Trick.new trick_number, self, @options
  #   @tricks.push trick
  #   trick
  # end

  def run
    log "Starting round"

    $client.send_and_receive("readyForRound")
    log "You were dealt: #{hand.inspect}"
    @dealt = hand.dup.map{|c| Card.new(c)}
    @held = hand.dup.map{|c| Card.new(c)}

    pass_cards
    play_trick
  end

  # def pass_cards
  #   if @number % 4 == 0
  #     log "Not passing cards"
  #   else
  #     log "About to pass cards"
  #     cards_to_pass = @options[:pass_cards_fn].(self)

  #     @passed = cards_to_pass
  #     @held = @held.reject{|c| @passed.include?(c)}

  #     received_cards = $client.send_and_receive("passCards", cards: cards_to_pass)["data"]["cards"]
  #     log "Received cards: #{received_cards.inspect}"
  #     @received = received_cards.map{|c| Card.new(c)}
  #     @held = @held + @received
  #   end
  # end

  # def play_trick
  #   13.times do |trick_number|
  #     trick = create_trick
  #     trick.run
  #   end
  # end

  def log(message)
    @game.log "R:#{@number} #{message}"
  end
end

class Game
  def initialize(info, options)
    @info = info
    @options = options
    @rounds = []
  end

  def create_round
    round_number = @rounds.size + 1
    round = Round.new(round_number, self, @options)
    @rounds.push round
    round
  end

  def run
    log "Starting game"

    loop do
      round = create_round

      round.run

      round_result = $client.send_and_receive("finishedRound")["data"]
      log "round result: #{round_result.inspect}"
      break if round_result["status"] != "nextRound"
    end
  end

  def log(message)
    puts "P:#{@info.position} #{message}"
  end

  def self.do_play_card(&blk)
    @play_card_fn = blk
  end

  def self.do_pass_cards(&blk)
    @pass_cards_fn = blk
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

#   module Suit
#     SPADES = "spades"
#     HEARTS = "hearts"
#     DIAMONDS = "diamonds"
#     CLUBS = "clubs"
#   end
# 
#   module Rank
#     TWO = "two"
#     QUEEN = "queen"
#   end
end


