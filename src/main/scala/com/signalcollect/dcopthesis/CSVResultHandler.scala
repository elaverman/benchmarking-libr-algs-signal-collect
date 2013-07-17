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


object CSVResultHandler { 
  /*
   * A class representing keys in a map that can
   * process the values they refer to.
   * For example, if a key refers to a value with type String of the form:
   * 1,2,3,4,5,6
   * This class may be used to transform this string to a list.
   */
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
  

/**
 * A result handler that is used to save results of the benchmark in CSV form.
 */
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
