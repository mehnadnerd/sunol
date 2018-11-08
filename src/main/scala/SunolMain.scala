import chisel3._
import sunol._

object SunolMain extends App {
  chisel3.Driver.execute(args, () => new SunolTop)
}

