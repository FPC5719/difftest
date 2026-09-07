package difftest

import chisel3._
import chisel3.util.Cat
import circt.stage.ChiselStage
import difftest.common.DifftestWiring
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

private class WiringExposureSource extends Module {
  val io = IO(new Bundle {})

  private val first = WireInit(0x5a.U(8.W)).suggestName("first")
  private val second = WireInit(0xa.U(4.W)).suggestName("second")
  DifftestWiring.addSource(first, "exposure_first", isHierarchical = true)
  DifftestWiring.addSource(second, "exposure_second", isHierarchical = false)
}

private class WiringExposureTop extends Module {
  val io = IO(Output(UInt(12.W)))

  Module(new WiringExposureSource)

  val first = Wire(UInt(8.W))
  val second = Wire(UInt(4.W))
  DifftestWiring.addSink(first, "exposure_first", isHierarchical = true)
  DifftestWiring.addSink(second, "exposure_second", isHierarchical = false)
  io := Cat(first, second)

  require(DifftestWiring.isEmpty)
}

class WiringExposureSpec extends AnyFlatSpec with Matchers {
  behavior of "DifftestWiring ExposureUtils migration"

  it should "route nested sources without BoringUtils ports" in {
    val chirrtl = ChiselStage.emitCHIRRTL(new WiringExposureTop)

    chirrtl should include("module WiringExposureSource")
    chirrtl should include("x_first")
    chirrtl should include("x_second")
    chirrtl should not include("bore")
  }
}
