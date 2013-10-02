package net.koofr.rangeparser

import org.specs2.mutable._

class RangeParserSpec extends Specification {
  import RangeParser._

  "RangeParser parse" should {
    "not parse malformed range" in {
      parse(200, "malformed") should beNone
    }

    "not parse range if start is greater than end" in {
      parse(200, "bytes=500-20") should equalTo(Some("bytes", None))
    }

    "not parse multiple ranges" in {
      parse(1000, "bytes=40-80,-1") should equalTo(Some("bytes", None))
    }
  }

  "RangeParser parseType" should {
    "parse range if start and end exist" in {
      parseType("bytes", 200, "bytes=0-499") should equalTo(Some(Some((0, 199))))
      parseType("bytes", 1000, "bytes=0-499") should equalTo(Some(Some((0, 499))))
      parseType("bytes", 1000, "bytes=40-80") should equalTo(Some(Some((40, 80))))
    }

    "parse range if start does not exist" in {
      parseType("bytes", 1000, "bytes=-500") should equalTo(Some(Some((500, 999))))
      parseType("bytes", 1000, "bytes=-400") should equalTo(Some(Some((600, 999))))
    }

    "parse range if end does not exist" in {
      parseType("bytes", 1000, "bytes=500-") should equalTo(Some(Some((500, 999))))
      parseType("bytes", 1000, "bytes=400-") should equalTo(Some(Some((400, 999))))
    }

    "parse range if both start and end equal 0" in {
      parseType("bytes", 1000, "bytes=0-0") should equalTo(Some(Some((0, 0))))
    }

    "parse correct type" in {
      parseType("bytes", 1000, "items=0-5") should equalTo(None)
    }
  }

  "RangeParser parseMulti" should {
    "parse single range" in {
      parseMulti(1000, "bytes=40-80") should equalTo(Some("bytes", Some(Seq((40, 80)))))
    }

    "parse multiple ranges" in {
      parseMulti(1000, "bytes=40-80,-1") should equalTo(Some("bytes", Some(Seq((40, 80), (999, 999)))))
    }

    "not parse multiple ranges if any range is incorrect" in {
      parse(200, "bytes=40-80,500-20") should equalTo(Some("bytes", None))
    }
  }

}
