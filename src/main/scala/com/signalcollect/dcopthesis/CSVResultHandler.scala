/*
 *  @author Robin Hafen
 *
 *  Copyright 2013 University of Zurich
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.signalcollect.dcopthesis

import scala.collection.mutable.ArrayBuffer
import com.signalcollect.nodeprovisioning.torque.ResultHandler
import java.util.Calendar
import au.com.bytecode.opencsv.CSVWriter
import java.io.FileWriter
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: robin
 * Date: 6/18/13
 * Time: 1:27 PM
 * To change this template use File | Settings | File Templates.
 */
object CSVResultHandler { 
  class ValExtractor(val key: String) {
    def extract(map: Map[String, String]): Option[Iterable[String]] = {
      map.get(key) match { 
        case Some(value) => Some(Iterable(value))
        case None => None
      }
    }
    
    def splitOn(sep: String = ",") = new ValExtractor(key) { 
      override def extract(map: Map[String, String]) = { 
        map.get(key) map { _.split(sep) } // String of values seperated by sep
      }
    }
  }
  
  implicit def strToKey(s: String) = new ValExtractor(s)
}

import CSVResultHandler._
  
class CSVResultHandler(val keysToLog: Iterable[ValExtractor],
                       val filename: Option[String] = None,
                       val shouldAppend: Boolean = true,
                       val applyQuotes: Boolean = false) extends ResultHandler {
  
  def addEntry(data: Map[String, String]) {

    val file = filename.getOrElse (
      s"CSVResults-${Calendar.getInstance.getTime.toString}.csv")

    val fileObj = new File(file)

    val fileExists = fileObj.exists

    val writer = new CSVWriter(new FileWriter(fileObj, shouldAppend))

    val row = ArrayBuffer[String]()

    keysToLog foreach { k =>
      val extractedValues = k extract(data) getOrElse Iterable("NA")
      extractedValues foreach { v =>
        row append v
      }
    }

    if (!fileExists) {
      val header = keysToLog map { _.key }
      writer.writeNext(header.toArray)
    }

    writer.writeNext(row.toArray)

    writer.close()
  }
}
