#!/usr/bin/env ruby
# -*- mode: enh-ruby -*-

# i was specifically trying to avoid readline
require 'readline'

# typical completion; btw has to be an actual proc, can't be a lambda
Readline.completion_proc = proc do |word|
  words = %w[hurr durr lol]
  words.grep(/\A#{Regexp.quote word}/)
end

# this is how you get text onto the command line
Readline.pre_input_hook = proc do
  Readline.insert_text 'h'
  Readline.redisplay
end

# how do you make readline read just one line?
line = Readline.readline '> ', true

warn line
