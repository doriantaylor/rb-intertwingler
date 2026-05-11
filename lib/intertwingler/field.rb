require_relative 'version'
require 'strscan'

# This is a set of classes that represent HTTP header/trailer field
# values of various structures. It serves the dual purpose of input
# sanitation and normalization, and a consistent,
# cryptographically-hashable, serialized representation for given
# input semantics.
#
class Intertwingler::Field
  # Leaves header value untouched, except for `#strip` around the
  # entire string.
  #
  class Verbatim < Field
  end

  # Field value gets split into an array on commas, modulo quoted
  # strings. Tokens get normalized to lower case, and that's about it.
  #
  class List < Field
  end

  # Field value elements are de-duplicated and sorted lexically on top
  # of being normalized.
  #
  class Set < List
  end

  # Field values consist of a `key` with optional `=value` pairs
  # (which may be a quoted string), such as `Cache-Control`.
  #
  class Pairs < List
  end

  # Ranked headers like `Accept` have a `q` parameter from 0 to 1
  # (which when omitted implies 1) among other possible parameters
  #
  class Ranked < List
  end

  # Special case for the `Link` header, which strictly speaking is a
  # _set_, with elements between angle brackets `<>` and parameters
  # that can be quoted strings in both keys _and_ values.
  #
  class Link < Set
  end
end
