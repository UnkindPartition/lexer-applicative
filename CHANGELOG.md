Changelog
=========

2.0
---

This is a major redesign of the API. Notable changes:

- The lexer now supports parsing the longest prefix/shortest suffix
  (see `longestShortest`)
- Instead of throwing an exception, we return a stream. The stream can be
  consumed directly, converted to a list or either-error-list of tokens.

1.1.1
-----

Add `tokensEither`

1.1
---

Upgrade to srcloc 0.5

1.0.0.1
-------

Signal a lexical error (instead of looping) when a regex does not consume any
characters
