/***************************************************************************************
 * Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * DiffTest is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 ***************************************************************************************/

package difftest

import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import circt.stage.ChiselStage
import difftest.common.DifftestWiring
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

private object CacheableDifftestSource {
  implicit object Key extends CacheableKey[CacheableDifftestSource]
}

private class CacheableDifftestSource extends Module with CacheableModule {
  val io = IO(new Bundle {
    val coreid = Input(UInt(8.W))
  })

  protected def buildModule(): Unit = {
    val event = DifftestModule(new DiffArchEvent, dontCare = true)
    event.coreid := io.coreid
  }
}

private class CacheableDifftestTop extends Module {
  val io = IO(new Bundle {})

  val source0 = CacheableModule(new CacheableDifftestSource)
  val source1 = CacheableModule(new CacheableDifftestSource)
  source0.io.coreid := 0.U
  source1.io.coreid := 1.U

  DifftestModule.collect("cacheable")
  require(DifftestModule.get_current_interfaces().size == 2)
  require(DifftestWiring.isEmpty)
}

class CacheableDifftestSpec extends AnyFlatSpec with Matchers {
  behavior of "Difftest gateway routing through CacheableModule"

  it should "collect one Difftest interface for every cached module instance" in {
    val chirrtl = ChiselStage.emitCHIRRTL(new CacheableDifftestTop)

    raw"(?m)^\s*inst source\d+ of CacheableDifftestSource\b".r
      .findAllMatchIn(chirrtl)
      .size shouldBe 2
  }
}
