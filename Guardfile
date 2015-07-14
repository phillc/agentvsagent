require "guard"
require 'guard/compat/plugin'

module ::Guard
  class Mocha < Plugin
    def start
      ::Guard::UI.info 'Guard::Mocha is starting'
      run_all if options[:all_on_start]
    end

    def run_all
      ::Guard::UI.info 'Guard::Mocha is running all'
      # files = Watcher.match_files(self, Dir.glob('**{,/*/**}/*.thrift'))
      # run_on_changes(Watcher.match_files(self, files))
      run_on_changes(nil)
    end

    def run_on_changes(paths)
      command = "mocha"
      ::Guard::UI.info "Guard::Mocha is running command #{command}"
      success = Kernel.system(command)

      if !success
        ::Guard::UI.error "Guard::Mocha has failed"
        throw :task_has_failed
      end
    end

    def run_on_removals(paths)
      run_on_changes(paths)
    end
  end
end

guard :process, :name => 'ava server', :command => './bin/ava server --hearts-max-points=60 --debug' do
  watch(%r{^lib/(.+)\.coffee$})
  watch('server.coffee')
end

guard :mocha do
  watch(%r{^test/.+\.coffee$})
  watch(%r{^lib/(.+)Test\.coffee$}) { |m| "spec/lib/#{m[1]}_spec.rb" }
end


less_options = {
  all_on_start: true,
  all_after_change: true,
  patterns: [%r{^src/less/(.+\.less)$}],
  output: "resources/public/css"
}

guard :less, less_options do
  less_options[:patterns].each { |pattern| watch(pattern) }
end
