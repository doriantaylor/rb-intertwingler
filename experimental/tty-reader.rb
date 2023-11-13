#!/usr/bin/env ruby
# -*- mode: enh-ruby -*-

# turns out tty-prompt subsumes tty-reader
require 'tty-prompt'
require 'pathname'

prompt = TTY::Prompt.new
reader = prompt.reader

reader.completion_handler = -> path do
  path = path.strip
  path = ?/ if path.empty?
  path = Pathname(path).expand_path

  # warn path

  until path.exist?
    path = path.parent
  end

  path.children.map(&:to_s).sort
end

reader.on(:keyctrl_a) do |event|
  event.line.move_to_start
end

reader.on(:keyctrl_e) do |event|
  event.line.move_to_end
end

reader.on(:keyctrl_u) do |event|
  line = event.line
  line.move_to_start
  line.delete line.text.length
end

reader.on(:keyctrl_w) do |event|
  line = event.line
  if m = /.*(\/[^\/]*\/*)\z/.match(line)
    chars = m.captures.first.length
    event.line.left   chars
    event.line.delete chars
  end
end

# this mysteriously does not work
# reader.on(:keyctrl_l) do |event|
#   warn event
#   # reader.cursor.clear_lines 100
#   # reader.clear_display event.line, TTY::Screen.width
# end

home = Pathname(Dir.home).realpath

wd = Pathname.pwd.realpath

short = wd.relative_path_from home
warn short

wd = short if short.to_s.length < wd.to_s.length

hm = reader.read_line '> ', value: wd.to_s

warn hm
