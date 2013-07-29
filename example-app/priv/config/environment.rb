# Load the rails application
require File.expand_path('../application', __FILE__)

# Initialize the rails application
SimpleDebug::Application.initialize!

Mime::Type.register "text/plist", :plist
