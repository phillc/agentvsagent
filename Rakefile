require 'pty'

desc "run ruby random bots"
task :bots, :number, :sleep do |t, args|
  args.with_defaults number: 4, sleep: 0.1
  command = 'cd dist/hearts/ruby && ruby random_bot.rb'

  pids = []
  args.number.to_i.times do |i|
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
