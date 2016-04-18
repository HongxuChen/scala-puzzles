import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, ParallelTestExecution}

package object s99 {
  abstract class BaseSpec extends FlatSpec with BeforeAndAfterAll with Matchers with ParallelTestExecution
}
