desc "run ruby random bots"
task :bots, :number do |t, args|
  args.with_defaults number: 4
  command = 'cd dist/hearts/ruby && ruby random_bot.rb'

  pids = []
  args.number.to_i.times do |i|
    pids << fork do
      STDOUT.sync = true
      exec command
    end
    sleep 1
  end

  pids.each{|pid| Process.wait(pid)}
end
