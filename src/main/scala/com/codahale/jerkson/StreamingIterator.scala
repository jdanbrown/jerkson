package com.codahale.jerkson

import com.fasterxml.jackson.core.{JsonToken, JsonParser}

class StreamingIterator[A](parser: JsonParser, mf: Manifest[A])
        extends Iterator[A] {

  import Json._

  if (parser.getCurrentToken == null) {
    parser.nextToken()
  }
  if (parser.getCurrentToken != JsonToken.START_ARRAY) {
    throw new ParsingException("Expected array, got: %s" format parser.getCurrentToken, null)
  }
  parser.nextToken()

  def hasNext = parser.getCurrentToken != JsonToken.END_ARRAY && !parser.isClosed

  def next() = if (hasNext) {
    val value = parse[A](parser, mf)
    parser.nextToken()
    value
  } else Iterator.empty.next()
}
