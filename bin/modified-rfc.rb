#!/usr/bin/env ruby

require 'time'

abort('Please enter a file name') unless ARGV[0]

print File.mtime(ARGV[0]).rfc2822
