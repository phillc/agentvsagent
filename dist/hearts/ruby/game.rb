require 'thrift'
$:.unshift File.dirname(__FILE__) + '/lib'
require 'hearts'

class Trick
  attr_reader :number, :round, :leader, :played
  def initialize(number, round, options)
    @number = number
    @round = round
    @options = options
    @leader = nil
    @played = []
  end

  def run
    log "Starting trick"
    trick = @options[:client].get_trick @options[:ticket]
    @leader = trick.leader
    @played = trick.played

    card_to_play = @options[:play_card_fn].(self)
    @round.held.delete(card_to_play)
    trick_result = @options[:client].play_card @options[:ticket], card_to_play
    log "trick: result #{trick_result.inspect}"
    @played = trick_result.played
  end

  def log(message)
    @round.log "T:#{@number} #{message}"
  end
end

class Round
  attr_reader :number, :game, :tricks, :dealt, :held
  def initialize(number, game, options)
    @number = number
    @game = game
    @options = options

    @tricks = []
    @dealt = []
    @passed = []
    @received = []
    @held = []
  end

  def create_trick
    trick_number = @tricks.size + 1
    trick = Trick.new trick_number, self, @options
    @tricks.push trick
    trick
  end

  def run
    log "Starting round"

    hand = @options[:client].get_hand @options[:ticket]
    log "You were dealt: #{hand.inspect}"
    @dealt = hand.dup
    @held = hand.dup

    pass_cards
    play_trick
  end

  def pass_cards
    if @number % 4 == 0
      log "Not passing cards"
    else
      log "About to pass cards"
      cards_to_pass = @options[:pass_cards_fn].(self)

      @passed = cards_to_pass
      @held = @held - @passed

      received_cards = @options[:client].pass_cards @options[:ticket], cards_to_pass
      log "Received cards: #{received_cards.inspect}"
      @received = received_cards
      @held = @held + @received
    end
  end

  def play_trick
    13.times do |trick_number|
      trick = create_trick
      trick.run
    end
  end

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

      round_result = @options[:client].get_round_result @options[:ticket]
      log "round result: #{round_result.inspect}"
      break if round_result.status != AgentVsAgent::GameStatus::NEXT_ROUND
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
    host = ENV["AVA_HOST"] || "127.0.0.1"
    port = ENV["AVA_PORT"] || 4001
    socket = Thrift::Socket.new(host, port)
    transport = Thrift::FramedTransport.new(socket)
    protocol = Thrift::BinaryProtocol.new(transport)
    client = AgentVsAgent::Hearts::Client.new(protocol)
    transport.open

    request = AgentVsAgent::EntryRequest.new
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
        pass_cards_fn: @pass_cards_fn,
        play_card_fn: @play_card_fn
      }

      game.run
      puts "Game is over"
      game_result = client.get_game_result ticket
      puts "game result: #{game_result.inspect}"
    else
      puts "No ticket"
    end
    transport.close
  rescue ::Thrift::Exception => e
    puts "Game ended in an exception"
    puts e.message
  end

  include AgentVsAgent
end

