#!/usr/bin/env ruby


################################################################################
#
# swap_code.rb -- Ruby script providing a simple code preprocessing system. 
# Specially annotated lines are swapped at a separator, yielding active
# code that was previously outcommented and vice versa.
#
# Names of R files must be provided at the command line. The action of this
# script is potentially destructive because the input files are overwritten.
#
# The swapping mechanism only works if the activated code is also syntactically
# correct in its context. Further, the separator should usually start with a 
# character that begins a comment. The default is "#||".
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


require 'optparse'


################################################################################


class String


  # Swap self at the first occurrence of _sep_, if any, keeping leading 
  # whitespace, if any. Return modified string, or self if _sep_ does not 
  # occur.
  #
  def swap sep
    result = lstrip.partition sep
    if result[1].empty?
      self
    else
      (self[/^\s+/] || "") + result.reverse.collect(&:strip).join(' ')
    end
  end


  # Treat self as file name, read all lines, apply #swap to each line using
  # _sep_ as argument, and write the result back to self, using _linebreak_
  # as linebreak type (mac, windows, or unix, provided as symbol). Returns a
  # Boolean indicating whether the file has been modified.
  #
  def swap_lines sep, linebreak
    lines = File.readlines(self).collect(&:chomp)
    result = lines.collect {|line| line.swap sep}
    return false if lines == result
    linebreak = {unix: "\n", mac: "\r", windows: "\r\n"}.fetch(linebreak) do |k|
      raise KeyError, k.to_s
    end
    File.open(self, 'w') {|file| file.puts result.join(linebreak)}
    true
  end
  

end


################################################################################


sep, linebreak, help_msg, verbose = "#||", :unix, false, true

opts = OptionParser.new
opts.on('-h', '--help', 'Print help message and exit') {|v| help_msg = true}
opts.on('-s', '--sep SEPARATOR', 'Separator to use', String) {|v| sep = v}
opts.on('-b', '--break TYPE', 'Type of line break', String,
  [:unix, :mac, :windows]) {|v| linebreak = v.to_sym}
opts.on('-q', '--quiet', 'Run quietly') {|v| verbose = false}


filenames = opts.parse ARGV

if help_msg or filenames.empty?
  warn opts.to_s
  exit 1
end


################################################################################


filenames.each do |filename|
  modified = filename.swap_lines sep, linebreak
  warn "File '#{filename}' has been code-swapped." if verbose and modified
end


################################################################################


