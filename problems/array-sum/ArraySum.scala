// Separation of concerns:

// One part of the challenge is that it requires a nested data type that contains a mixture of single elements of type A
// or sequences of the same nested data type, i.e. again single elements or sequences ...
// This is independent of whether type A can be summed over or not.
// There is also no direct need for such nested data type to be iterable.
// This first concern is implemented via NestedAs[A].
//
// The next part of the challenge is to sum up such nested sequence of As assuming the As are of a summable data type
// (like Int). This can again be split into two concerns:
// a) to flatten the nested data type into some sequence/iterable.
//    This is still independent of whether type A can be summed over or not,
//    thus can be solved in a general way, here via FlattenNestedAs.
// b) to sum up the flattened sequence assuming A is summable. This is finally implemented at ArraySum.

trait NestedAs[A]

case class A[A](a: A) extends NestedAs[A]

// Uses Scala varargs, thus as is actually of type Seq[NestedAs[A]]
case class As[A](as: NestedAs[A]*) extends NestedAs[A]

object FlattenNestedAs {
    def flatten[A](nestedAs: NestedAs[A]): Iterable[A] = nestedAs match {
        case A(a) => Iterable.single(a)
        case as: As[_] => as.as.foldLeft(Iterable.empty[A]) {
            case (it, a) => it ++ flatten(a)
        }
    }
}


object ArraySum {
    // The readme requires arraySum to accept a single parameter, being formally a list.
    // The readme requires the type Int, but summing works on any numeric type, thus we use Numeric.
    def arraySum[N: Numeric](nestedAs: NestedAs[N]): N = FlattenNestedAs.flatten(nestedAs).sum

    // We can also allow an informal list via varargs.
    // The readme requires the type Int, but summing works on any numeric type, thus we use Numeric.
    def arraySum2[N: Numeric](nestedAs: NestedAs[N]*): N =
        nestedAs.map(FlattenNestedAs.flatten(_)).flatten.sum
}

// Tests

import ArraySum.{arraySum, arraySum2}

val testInput = As(A(1), A(2), As(A(3), A(4), As(A(5))))

// Test for single list
assert(15 == arraySum(testInput))
// Test for varargs
assert(15 == arraySum2(A(1), A(2), As(A(3), A(4), As(A(5)))))

println(testInput)
