/***************************************************************************************
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * DiffTest is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package difftest.common

import chisel3._
import chisel3.util.experimental.ExposureUtils
import chisel3.util.MixedVec
import difftest.DifftestBundle

private case class DifftestTag(
  name: String, // TODO: remove name?
  bundle: DifftestBundle,
  delay: Int
) extends ExposureTag

object DifftestWiring {
  def exposeSource[T <: Data](data: T, name: String, bundle: DifftestBundle, delay: Int): T = {
    ExposureUtils.expose(DifftestTag(name, bundle, delay), true, data.asUInt)
    data
  }

  def exposeMetadata(name: String, bundle: DifftestBundle, delay: Int): Unit = {
    ExposureUtils.exposeMetadata(DifftestTag(name, bundle, delay))
  }

  def collectSink(): Seq[(DifftestBundle, Int, Data)] = {
    val res = ExposureUtils.collect[DifftestTag]()
    res.map { case (DifftestTag(name, bundle, delay), data) =>
      (bundle, delay, data)
    }
  }

  def collectMetadata(): Seq[(DifftestBundle, Int)] = {
    val res = ExposureUtils.collectMetadata[DifftestTag]()
    res.map { case DifftestTag(name, bundle, delay) =>
      (bundle, delay)
    }
  }
}
