/***************************************************************************************
 * Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * DiffTest is licensed under Mulan PSL v2.
 * You can use this software according to the terms of the Mulan PSL v2.
 * You may obtain a copy of Difftest at:
 *          http://license.coscl.org.cn/MulanPSL2
 ***************************************************************************************/

package difftest

import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import circt.stage.ChiselStage
import difftest.common.DifftestWiring
import difftest.gateway.Gateway
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

private object DirectCacheableSource {
  implicit object Key extends CacheableKey[DirectCacheableSource]
}

private class DirectCacheableSource extends Module with CacheableModule {
  val io = IO(new Bundle {})

  protected def buildModule(): Unit = {
    DifftestModule(new DiffArchFpRenameTable(192), dontCare = true)
  }
}

private class DirectCacheableTop extends Module {
  val io = IO(new Bundle {})

  // TopMain forces this direct-DPIC configuration for the ordinary XSTop path.
  Gateway.setConfig("U")
  DifftestModule(new DiffArchEvent, dontCare = true)
  DifftestModule(new DiffArchEvent, dontCare = true)
  val source0 = CacheableModule(new DirectCacheableSource)
  val source1 = CacheableModule(new DirectCacheableSource)

  DifftestModule.collect("direct-cacheable")
}

class DirectCacheableDifftestSpec extends AnyFlatSpec with Matchers {
  behavior of "direct Difftest routing through CacheableModule"

  it should "collect both cached direct-DPIC interfaces" in {
    noException should be thrownBy ChiselStage.emitCHIRRTL(new DirectCacheableTop)
    DifftestModule.get_current_interfaces().count(_._1.desiredCppName == "rat_frf") shouldBe 2
    DifftestWiring.isEmpty shouldBe true
  }
}
