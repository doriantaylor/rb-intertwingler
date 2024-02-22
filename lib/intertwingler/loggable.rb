require 'intertwingler/version'

# This module implements a snap-on #log method which will load and
# initialize {::Logger} in the instance variable `@log` if one is not
# found.
#
module Intertwingler::Loggable

  # Return a {::Logger} instance or otherwise load one with defaults.
  #
  # @return [Logger] the logger.
  #
  def log
    unless @log
      require 'logger'
      @log = Logger.new $stderr
    end
    @log
  end

end
