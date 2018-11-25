import chisel3._
import sunol._

object SunolNoCache extends App {
  chisel3.Driver.execute(args, () => new SunolTop)
}

object SunolCacheOnly extends App {
  chisel3.Driver.execute(args, () => new SunolCache(64))
}

object SunolMain extends App {
  chisel3.Driver.execute(args, () => new SunolTipTop)
}

