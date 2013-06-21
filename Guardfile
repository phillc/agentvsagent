require "guard"
require "guard/guard"
require "json"
require "erb"

module ::Guard
  class Thrift < ::Guard::Guard
    def initialize(watchers=[], options={})
      super(watchers, options)
    end

    def start
      run_all if options[:all_on_start]
    end

    def run_all
      files = Watcher.match_files(self, Dir.glob('**{,/*/**}/*.thrift'))
      run_on_changes(Watcher.match_files(self, files))
    end

    def run_on_changes(paths)
      clean_targets!

      ["thrift"].product(language_options, paths.uniq).map{|c| c.join(" ")}.map do |command|
        puts "running #{command}"
        puts `#{command}`
      end
    end

    def run_on_removals(paths)
      run_on_changes(paths)
    end

    protected

    def clean_targets!
      if @options[:clean_target] && @options[:targets].is_a?(Hash)
        @options[:targets].keys.each do |path|
          command = "rm #{path}/*"
          puts "running #{command}"
          puts `#{command}`
        end
      end
    end

    def language_options
      if @options[:targets].is_a? Hash
        target_option = @options[:gen_folder] ? "-o" : "-out"
        @options[:targets].map do |path, language|
          "#{target_option} #{path} --gen #{language}"
        end
      else
        @options[:targets].map do |language|
          "--gen #{language}"
        end
      end
    end
  end
end

guard :shell, all_on_start: true do
  watch("thrift/hearts.thrift.erb") do
    ava_version = JSON.parse(File.read("package.json"))["version"]
    puts "AVA VERSION #{ava_version}"
    file = "thrift/hearts.thrift.erb"
    output = "thrift/gen/hearts.thrift"
    contents = ::ERB.new(File.read(file)).result(binding)
    File.open(output, 'w'){ |f| f.write(contents) }
  end

  watch("thrift/tic_tac_toe.thrift.erb") do
    ava_version = JSON.parse(File.read("package.json"))["version"]
    puts "AVA VERSION #{ava_version}"
    file = "thrift/tic_tac_toe.thrift.erb"
    output = "thrift/gen/tic_tac_toe.thrift"
    contents = ::ERB.new(File.read(file)).result(binding)
    File.open(output, 'w'){ |f| f.write(contents) }
  end
end

guard :thrift, all_on_start: true,
               clean_target: true,
               targets: { "lib/hearts/service/types" => "js:node",
                          "lib/hearts/web/public/types" => "js:jquery",
                          "dist/hearts/nodejs/lib" => "js:node",
                          "dist/hearts/java/lib" => "java",
                          "dist/hearts/haskell/lib" => "hs",
                          "dist/hearts/ruby/lib" => "rb" } do
  watch('thrift/gen/hearts.thrift')
end

guard :thrift, all_on_start: true,
               clean_target: true,
               targets: { "lib/tic_tac_toe/service/types" => "js:node",
                          "lib/tic_tac_toe/web/public/types" => "js:jquery",
                          "dist/tic_tac_toe/nodejs/lib" => "js:node",
                          "dist/tic_tac_toe/ruby/lib" => "rb" } do
  watch('thrift/gen/tic_tac_toe.thrift')
end

guard :shell do
  watch(%r{vendor/thrift/lib/nodejs/(.*)}) {|m| puts "#{m[0]} changed, packaging thrift"; puts `make package-thrift` }
  ignore! []
end
