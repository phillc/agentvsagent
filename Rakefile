require 'pty'

desc "run ruby random agents"
task :agents, :number, :langs, :sleep do |t, args|
  args.with_defaults number: 4, sleep: 0.1, langs: "ruby:coffee"
  agent_commands = {
    "ruby" => 'cd dist/hearts/ruby && ruby sample_agent.rb',
    "coffee" => 'cd dist/hearts/nodejs && coffee sampleAgent.coffee',
    "haskell" => 'cd dist/hearts/haskell && cabal configure && cabal build && dist/build/sampleAgent/sampleAgent'
  }

  agents = agent_commands.values_at(*args.langs.split(":")).shuffle.cycle

  pids = []
  args.number.to_i.times do |i|
    command = agents.next
    pids << fork do
      STDOUT.sync = true

      begin
        PTY.spawn(command) do |stdin, stdout, pid|
          begin
            stdin.each { |line| print "[#{i}] #{line}" }
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
