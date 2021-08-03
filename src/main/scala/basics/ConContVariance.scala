package basics

object ConContVariance {

  //ex 1
  abstract class Vehicle

  case class Car() extends Vehicle

  case class Bike() extends Vehicle

  case class Parking[A](value: A)

  case class CovParking[+A](value: A)

  case class ContrParking[-A]()

  def test1: Unit = {
    val vehicleIdentity = (vehicle:Vehicle) => vehicle

    vehicleIdentity(Car())
    vehicleIdentity(Bike())


    //Incorrect
    //val carParking: Parking[Vehicle] = Parking[Car](new Car)

    val carParking: CovParking[Vehicle] = CovParking[Car](new Car)

    val contrParking: ContrParking[Car] = ContrParking[Vehicle]()
  }

  // ex 2

  abstract class Foo[-A] {
    def foo(a: A): Unit
  }

  object NoOp extends Foo[Any] {
    override def foo(a: Any): Unit = {}
  }

  class PrintFoo[A] extends Foo[A] {
    override def foo(a: A): Unit = {
      println(a.toString)
    }
  }

  def test2(): Unit = {

    val f1: Foo[Int] = NoOp

    f1.foo(12)
  }

  def main(args: Array[String]): Unit = {
    test2()
  }



}
