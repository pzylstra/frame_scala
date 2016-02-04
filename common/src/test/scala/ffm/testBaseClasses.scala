package ffm

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar

abstract class BasicSpec extends FlatSpec with Matchers

abstract class MockSpec extends BasicSpec with MockitoSugar
