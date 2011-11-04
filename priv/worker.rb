#!/usr/bin/env ruby

Dir.chdir(ARGV[0])
require 'rubygems'

if File.exists?("Gemfile")
  require 'bundler'
  Bundler.setup
end

require 'rack'


def read_request
  env = {}
  
  size = STDIN.read(4)
  
  if !size || size.size != 4
    exit(0)
  end
  
  size = size.unpack("N")[0]
  
  count = STDIN.read(4).unpack("N")[0]

  count.times do
    len = STDIN.read(4).unpack("N")[0]
    key = STDIN.read(len)
    len = STDIN.read(4).unpack("N")[0]
    value = STDIN.read(len)
    env[key] = value
  end

  body_len = STDIN.read(4).unpack("N")[0]
  
  rack_input = StringIO.new(STDIN.read(body_len))
  rack_input.set_encoding(Encoding::BINARY) if rack_input.respond_to?(:set_encoding)

  env.update({"rack.version" => Rack::VERSION,
               "rack.input" => rack_input,
               "rack.errors" => $stderr,

               "rack.multithread" => false,
               "rack.multiprocess" => false,
               "rack.run_once" => false,

               "rack.url_scheme" => ["yes", "on", "1"].include?(ENV["HTTPS"]) ? "https" : "http"
             })

  # $stderr.puts env.inspect
  env
end

def handle_request(app, env)

  status, headers, body = app.call(env)

  # $stderr.puts status.inspect, headers.inspect, body.inspect
  
  packed_body = if body.respond_to?(:to_path) # file
    [1, body.to_path.size, body.to_path].pack("CNa*")
  else
    ary = []
  
    body.each do |s|
      ary << s
    end
  
    t = ary.join("")
  
    [0, t.size, t].pack("CNa*")
  end
  
  body.close if body.respond_to?(:close)
  

  packed = [status, headers.length].pack("NN") + 
           headers.map {|key,value| [key.size, key, value.to_s.size, value.to_s].pack("Na*Na*")}.join("") + 
           packed_body
  
  STDOUT.write([packed.size].pack("N"))
  STDOUT.write(packed)
  STDOUT.flush
end


def load_app
  app, options = Rack::Builder.parse_file("config.ru")
  last_mtime = File.mtime("config.ru")
  [app, last_mtime]
end

app = nil

loop do
  env = read_request
  
  if !app
    ENV["RAILS_RELATIVE_URL_ROOT"] = env["SCRIPT_NAME"]
    app, last_mtime = load_app
  end
  
  # $stderr.puts "#{(File.mtime("config.ru") - last_mtime).inspect}\r"
  if File.mtime("config.ru") > last_mtime
    app, last_mtime = load_app
    # $stderr.puts "Reload app\r"
  end
  handle_request(app, env)
end

