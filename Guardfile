require "guard"
require "guard/guard"

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

guard :thrift, all_on_start: true,
               clean_target: true,
               targets: { "hearts/lib" => "js:node",
                          "hearts/web/lib" => "js:jquery",
                          "dist/hearts/nodejs/lib" => "js:node",
                          "dist/hearts/java/lib" => "java",
                          "dist/hearts/haskell/lib" => "hs",
                          "dist/hearts/ruby/lib" => "rb" } do
  watch('hearts/hearts.thrift')
end
