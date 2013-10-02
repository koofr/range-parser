package net.koofr.rangeparser

import scala.util.Try

object RangeParser {

  def parseMulti(size: Long, rangeStr: String): Option[(String, Option[Seq[(Long, Long)]])] = {
    Some(rangeStr.split("=")) collect {
      case Array(typ, ranges) =>
        val pairs = ranges.split(",").toSeq map { range =>
          Some(range.split("-", -1)) collect {
            case Array("", ToLong(end)) => (size - end, size - 1)
            case Array(ToLong(start), "") => (start, size - 1)
            case Array(ToLong(start), ToLong(end)) => (start, end min (size - 1))
          } filter {
            case (start, end) => start <= end && start >= 0
          }
        }

        val pairsMaybe = Some(pairs).filter(_.forall(_.isDefined)).map(_.flatten)

        (typ, pairsMaybe)
    }
  }

  def parse(size: Long, rangeStr: String): Option[(String, Option[(Long, Long)])] = {
    parseMulti(size, rangeStr).map {
      case (typ, ps) => (typ, ps.filter(_.length == 1).map(_.head))
    }
  }

  def parseType(typ: String, size: Long, rangeStr: String): Option[Option[(Long, Long)]] = {
    parse(size, rangeStr).filter(_._1 == typ).map(_._2)
  }

  object ToLong {
    def unapply(str: String): Option[Long] = Try(str.toLong).toOption
  }

}
