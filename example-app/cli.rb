#!/usr/bin/env ruby

# $LOAD_PATH << File.dirname(__FILE__) + "/lib"
# require 'web_socket'

require 'websocket-eventmachine-client'
require 'json'

@ws_host = ARGV[0]
@ws_port = ARGV[1].to_i

EM.run do
    ws = WebSocket::EventMachine::Client.connect(:uri => "ws://#{@ws_host}:#{@ws_port}/ws")

    ws.onopen do 
        puts "Connected"
    
        ts = Time.now.to_i
        msg = {
            "unique"    => "a1s2d3f4g5h6j7k8l9",
            "ts"        => ts,
            "action"    => "main/index.json",
            "query_str" => "",
            "method"    => "GET"
        }

        puts "Sending..."
        # ws.send("OLOLOLOLOLO")
        ws.send(msg.to_json)
    end    

    ws.onmessage do |msg, type|
        begin
            json = JSON.parse(msg)
            puts "Received JSON:\n#{json.to_s}\n"
        rescue
            puts "Class:#{msg.class}\nType:#{type}\nReceived message:\n#{msg}\n"
        end
    end

    ws.onclose do
        puts "Disconnected"
    end

end
