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
import difftest.DifftestBundle
import scala.collection.mutable

private[difftest] sealed trait DifftestSourceMetadata

private[difftest] final case class GatewaySourceMetadata(bundle: DifftestBundle, delay: Int)
  extends DifftestSourceMetadata

private final case class DifftestSourceTag(name: String, metadata: Option[DifftestSourceMetadata]) extends ExposureTag

private final case class ExposedDifftestSource(
  name: String,
  metadata: Option[DifftestSourceMetadata],
  data: Data,
)

private class WiringInfo(val data: Data, val name: String, val isHierarchical: Boolean) {
  private var nSources: Int = 0
  private var nSinks: Int = 0

  def setSource(): WiringInfo = {
    require(nSources == 0, s"$name already declared as a source")
    nSources += 1
    this
  }

  def addSink(): WiringInfo = {
    nSinks += 1
    this
  }

  def isPending: Boolean = nSources == 0 || nSinks == 0
  // (isSource, dataType, name)
  def toPendingTuple: Option[(Boolean, Data, String, Boolean)] = {
    if (isPending) {
      Some((nSources == 0, chiselTypeOf(data), name, isHierarchical))
    } else {
      None
    }
  }
}

object DifftestWiring {
  private val wires = mutable.ListBuffer.empty[WiringInfo]

  private var _exposedSources: Option[Seq[ExposedDifftestSource]] = None
  private def exposedSources: Seq[ExposedDifftestSource] = {
    _exposedSources.getOrElse {
      val sources = ExposureUtils.collect[DifftestSourceTag]().map { case (tag, data) =>
        ExposedDifftestSource(tag.name, tag.metadata, data)
      }
      _exposedSources = Some(sources)
      sources
    }
  }

  private var _collectedSources: Option[Map[String, Data]] = None
  private def collectedSources: Map[String, Data] = {
    _collectedSources.getOrElse {
      var collected = Map.empty[String, Data]
      val duplicated = mutable.ListBuffer.empty[String]
      exposedSources.filter(_.metadata.isEmpty).foreach { source =>
        collected.get(source.name) match {
          case Some(_) => duplicated += source.name
          case None => collected += source.name -> source.data
        }
      }
      require(duplicated.isEmpty, s"duplicate exposed difftest sources: ${duplicated.mkString(", ")}")
      _collectedSources = Some(collected)
      collected
    }
  }

  private def getWire(data: Data, name: String, isHierarchical: Boolean): WiringInfo = {
    wires.find(_.name == name).getOrElse {
      val info = new WiringInfo(data, name, isHierarchical)
      wires.addOne(info)
      info
    }
  }

  def addSource[T <: Data](
    data: T,
    name: String,
    isHierarchical: Boolean,
  ): T = {
    getWire(data, name, isHierarchical).setSource()
    ExposureUtils.expose(DifftestSourceTag(name, None), isUpward = true, data)
    data
  }

  def addSource[T <: Data](data: T, name: String): T = {
    addSource[T](data, name, false)
  }

  private[difftest] def exposeSource[T <: Data](
    data: T,
    name: String,
    metadata: DifftestSourceMetadata,
  ): T = {
    ExposureUtils.expose(DifftestSourceTag(name, Some(metadata)), isUpward = true, data)
    data
  }

  private[difftest] def collectMetadataSources: Seq[(DifftestSourceMetadata, Data)] = {
    exposedSources.flatMap(source => source.metadata.map(_ -> source.data))
  }

  def addSink[T <: Data](data: T, name: String, isHierarchical: Boolean): T = {
    val info = getWire(data, name, isHierarchical).addSink()
    require(!info.isPending, s"[${info.name}]: Wiring requires addSource before addSink")
    val source = collectedSources.getOrElse(
      name,
      throw new IllegalArgumentException(s"[$name]: no exposed source reached the collection point")
    )
    require(source.getWidth == data.getWidth, s"[$name]: source/sink widths differ")
    data := source
    data
  }

  def addSink[T <: Data](data: T, name: String): T = {
    addSink[T](data, name, false)
  }

  private def clearCollectedState(): Unit = {
    wires.clear()
    _exposedSources = None
    _collectedSources = None
  }

  def isEmpty: Boolean = {
    val empty = !hasPending
    if (empty && _exposedSources.isDefined) clearCollectedState()
    empty
  }
  def hasPending: Boolean = wires.exists(_.isPending)
  def getPending: Seq[(Boolean, Data, String, Boolean)] = wires.flatMap(_.toPendingTuple).toSeq

  def createPendingWires(): Seq[Data] = {
    getPending.map { case (isSource, dataType, name, isHierarchical) =>
      val target = WireInit(0.U.asTypeOf(dataType)).suggestName(name)
      if (isSource) {
        addSource(target, name, isHierarchical)
      } else {
        addSink(target, name, isHierarchical)
      }
    }
  }

  def createExtraIOs(flipped: Boolean = false): Seq[Data] = {
    getPending.map { case (isSource, dataType, name, _) =>
      def do_direction(dt: Data): Data = if (isSource) Input(dt) else Output(dt)
      def do_flip(dt: Data): Data = if (flipped) Flipped(dt) else dt
      IO(do_flip(do_direction(dataType))).suggestName(name)
    }
  }

  def createAndConnectExtraIOs(): Seq[Data] = {
    createExtraIOs().zip(createPendingWires()).map { case (io, wire) =>
      io <> wire
      io
    }
  }
}
