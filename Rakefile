require 'pty'

class Agent
  def initialize(options={})
    @directory = options[:directory]
    @command = options[:command]
    @compile = options[:compile]
  end

  def cd
    "cd #{@directory}"
  end

  def compile!
    return unless @compile
    command = "#{cd} && #{@compile}"
    puts "compiling: #{command}"
    puts `#{command}`
    raise "Compile error" unless $?.to_i == 0
  end

  def command
    %{#{cd} && ../../../bin/ava play #{game} --run="#{@command}"}
  end
end

class HeartsAgent < Agent
  def game
    "hearts"
  end
end

class FireworksAgent < Agent
  def game
    "fireworks"
  end
end

desc "run ruby random agents"
task :agents, :number, :langs, :game, :sleep do |t, args|
  args.with_defaults number: 4, sleep: 0.1, langs: "ruby:coffee", game: "hearts"

  agent_commands = {
    "hearts" => {
      "ruby" => HeartsAgent.new(directory: 'dist/hearts/ruby', command: 'ruby my_agent.rb'),
      "coffee" => HeartsAgent.new(directory: 'dist/hearts/nodejs', command: 'coffee myAgent.coffee'),
      "haskell" => HeartsAgent.new(directory: 'dist/hearts/haskell', compile: 'cabal configure && cabal build', command: 'dist/build/myAgent/myAgent'),
      "go" => HeartsAgent.new(directory: 'dist/hearts/go', compile: 'make', command: 'bin/my_agent')
    },
    "fireworks" => {
      "ruby" => FireworksAgent.new(directory: 'dist/fireworks/ruby', command: 'ruby my_agent.rb'),
    }
  }

  agents = agent_commands[args.game].values_at(*args.langs.split(":")).shuffle

  agents.each(&:compile!)

  commands = agents.map(&:command).cycle

  pids = []
  args.number.to_i.times do |i|
    command = commands.next
    pids << fork do
      STDOUT.sync = true

      begin
        puts command
        PTY.spawn(command) do |stdin, stdout, pid|
          begin
            stdin.each { |line| puts "[#{i}] #{line}" }
          rescue Errno::EIO
            puts "Errno:EIO error, but this probably just means that the process has finished giving output"
          end
        end
      rescue PTY::ChildExited
        puts "The child process exited!"
      end
    end
    sleep args.sleep.to_f
  end

  pids.each{|pid| Process.wait(pid)}
end
