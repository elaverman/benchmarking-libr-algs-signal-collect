/*
 *  Original authors
 *  @author Daniel Strebel
 *  @author Philip Stutz
 *  
 *  Heaviliy modified by
 *  @author Robin Hafen
 *
 *  Copyright 2012 University of Zurich
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
 *
 */

package com.signalcollect.dcopthesis


/**
 * This file includes the main evaluation program and its related resources.
 */


import com.signalcollect._
import com.signalcollect.StateForwarderEdge
import com.signalcollect.dcopthesis.libra._
import java.text.SimpleDateFormat
import java.util.Date
import java.io.FileWriter
import au.com.bytecode.opencsv.CSVWriter
import javax.swing._
import org.math.plot._
import com.signalcollect.evaluation.jobsubmission.EvaluationSuiteCreator
import com.signalcollect.nodeprovisioning.torque.LocalHost
import com.signalcollect.configuration.ExecutionMode
import com.signalcollect.evaluation.resulthandling.ConsoleResultHandler
import java.util.Calendar
import com.signalcollect.dcopthesis.FileDialect._


// A global stats object that gets updated after each cycle.
object GlobalStats {
  import scala.collection.mutable.ArrayBuffer

  // Logging of currentUtility/bestUtility ratios over algorithm's lifetime // per run
  val utilityRatios = ArrayBuffer[Double]()

  var timeToNashEquilibrium = -1
  var timeToOptimum = -1
  var totalNumberOfRuns = -1

  var currentRunNumber = 0

  def ratioAtTermination = utilityRatios(utilityRatios.size-1)

  def clear() {
    utilityRatios.clear()
    timeToNashEquilibrium = -1
    timeToOptimum = -1
  }

  // Create a frame using jmathplot for visualizing the algorithm's performance
  // during the evaluation process
  val plot = new Plot2DPanel
  val frame = new JFrame("Plots")
  frame.setSize(1000, 800)
  frame.setVisible(true)
  frame.setContentPane(plot)
  plot.addLegend("SOUTH")
}


/*
 * The main program for evaluating algorithms.
 */
object DcopEvaluation extends App {

  val evalName = "dcopthesis benchmark run"
  val dateTime = (new SimpleDateFormat("dd/MM/yyyy HH:mm:ss")).format(new Date())

  // Parameters for Kraken:
  // val jvmParameters = "-Xmx64000m -XX:+UseNUMA -XX:+UseCondCardMark -XX:+UseParallelGC"
  val jvmParameters = "-Xmx6g -XX:+UseNUMA -XX:+UseCondCardMark -XX:+UseParallelGC"

  val fastEval = new EvaluationSuiteCreator(evaluationName = evalName,
    executionHost = new LocalHost
    //new TorqueHost(torqueHostname = "kraken.ifi.uzh.ch", localJarPath = "./target/signal-collect-dcops-assembly-2.0.0-SNAPSHOT.jar", torqueUsername = System.getProperty("user.name"), priority = TorquePriority.fast)
  )

  /*
   * Execution parameters
   */
  val repetitions = 5 // how many repetitions to perform
  val graphSize = 80 
  val graphSizes = List(graphSize) // the graph's sizes (only one was used at a time)
  val domainSizes = List(8) // the domain sizes (only one was used at a time)
  val meanDegrees = List(14) // the mean degrees (once used for a graph provider)
  val numberOfGraphs = 50 // the number of graphs on which the algorithms should be run

  // The list of algorithms to be run
  val algorithmsList: List[VertexBuilder] = List(
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=1.0, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.9, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.75, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.7, stepsToRemember=50),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.6, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.5, stepsToRemember=50),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.45, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.3, stepsToRemember=50),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.3, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.15, stepsToRemember=0)

//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=1, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.85, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.6, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.45, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.3, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.15, stepsToRemember=0),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.0125, stepsToRemember=0),
//
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=1, stepsToRemember=250),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.85, stepsToRemember=250),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.6, stepsToRemember=250),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.45, stepsToRemember=250),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.3, stepsToRemember=250),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.15, stepsToRemember=250),
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.0125, stepsToRemember=250)
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.1, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.2, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.4, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.5, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.6, k = 2, const = 1),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.6, k = 2, const = 250)
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.7, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.8, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 0.9, k = 2, const = 250),
//    new DSANVertexBuilder(randomInitialState=false, pSchedule = 1, k = 2, const = 250)
      
//    new WRMIVertexBuilder(randomInitialState=false, memory=0, inertia=0.2),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.2)
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.99, inertia=0.0),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.8,  inertia=0.0),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.6,  inertia=0.0),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.4,  inertia=0.0),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.2,  inertia=0.0),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.0,  inertia=0.0),
    
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.99, inertia=0.1),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.8,  inertia=0.1),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.6,  inertia=0.1),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.4,  inertia=0.1),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.2,  inertia=0.1),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.0,  inertia=0.1)
      
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.99, inertia=0.2),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.8,  inertia=0.2),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.6,  inertia=0.2),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.4,  inertia=0.2),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.2,  inertia=0.2),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.0,  inertia=0.2)
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.99, inertia=0.4),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.8,  inertia=0.4),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.6,  inertia=0.4),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.4,  inertia=0.4),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.2,  inertia=0.4),
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.0,  inertia=0.4)
      
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=1.0),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.9),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.8),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.7),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.6),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.5),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.4),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.3),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.2),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.1),
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.0)

//    new ExpVertexBuilder(randomInitialState=false, inertia=0.9),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.8),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.7),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.6),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.5),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.4),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.3),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.2),
//    new ExpVertexBuilder(randomInitialState=false, inertia=0.1)

//    new BestResponseVertexBuilder(randomInitialState = false, inertia = 0.5),
//    new DSAVertexBuilder(false, DSAVariant.B, inertiaC = 0.2,    pSchedule = 0.5)
//    new DSAVertexBuilder(false, DSAVariant.B, inertiaC = 0.2,    pSchedule = 0.2),
//    new DSAVertexBuilder(false, DSAVariant.B, inertiaC = 0.2,    pSchedule = 0.2),
//    new DSAVertexBuilder(false, DSAVariant.B, inertiaC = 0.2,    pSchedule = 0.2)

//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.2,    pSchedule = 0.8),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.4,    pSchedule = 0.8),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.6,    pSchedule = 0.8),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.8,    pSchedule = 0.8),
//
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.2,    pSchedule = 0.5),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.4,    pSchedule = 0.5),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.6,    pSchedule = 0.5),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.8,    pSchedule = 0.5),
//
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.2,    pSchedule = 0.2),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.4,    pSchedule = 0.2),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.6,    pSchedule = 0.2),
//    new DSAVertexBuilder(false, DSAVariant.BI, inertiaC = 0.8,    pSchedule = 0.2)

    /* DSA-A */
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 1.0),
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 0.85),
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 0.6),
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 0.45),
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 0.3),
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 0.15),
//    new DSAVertexBuilder(false, DSAVariant.A, pSchedule = 0.0125),
    /* DSA-B */
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 1.0),
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 0.85),
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 0.6),
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 0.45),
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 0.3),
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 0.15),
//    new DSAVertexBuilder(false, DSAVariant.B, pSchedule = 0.0125)

//    new FPVertexBuilder(randomInitialState=false, 0.5),
//    new FPVertexBuilder(randomInitialState=false, 0.4),
//    new FPVertexBuilder(randomInitialState=false, 0.3),
//    new FPVertexBuilder(randomInitialState=false, 0.2),
//    new FPVertexBuilder(randomInitialState=false, 0.1),
//    new FPVertexBuilder(randomInitialState=false, 0.0)
      
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.9),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.8),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.7),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.6),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.5),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.4),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.3),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.2),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.1),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0)
//    new BestResponseVertexBuilder(randomInitialState=false, 0.2),
//    new BestResponseVertexBuilder(randomInitialState=false, 0.4),
//    new BestResponseVertexBuilder(randomInitialState=false, 0.6),
//    new BestResponseVertexBuilder(randomInitialState=false, 0.9),
//    new BestResponseVertexBuilder(randomInitialState=false, 0.9875),
//    new BestResponseVertexBuilder(randomInitialState=false, 1.0)

//    new MGMVertexBuilder(randomInitialState=false)


//    new SAPVertexBuilder(randomInitialState=false, ηInitial=5.0, vertexIdsSchedule=(0 until graphSize)),
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=1.0, vertexIdsSchedule=(0 until graphSize)),
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=0.5, vertexIdsSchedule=(0 until graphSize)),
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=0.25, vertexIdsSchedule=(0 until graphSize)),
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=0.1, vertexIdsSchedule=(0 until graphSize)), 
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=0.05, vertexIdsSchedule=(0 until graphSize)),
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=0.0, vertexIdsSchedule=(0 until graphSize)) 
//    new GSAPVertexBuilder(randomInitialState=false, vertexIds=(0 until graphSize), interval=10)


//    new MGMVertexBuilder(randomInitialState=true)
//    new DSANVertexBuilder(randomInitialState=true, pSchedule=1),
//    new DSAVertexBuilder(randomInitialState=false, DSAVariant.B, pSchedule = 0.30))
//    new DSAVertexBuilder(randomInitialState=false, DSAVariant.B, pSchedule = 0.8)
//    new SAPVertexBuilder(randomInitialState=false, ηInitial=1, vertexIdsSchedule=(0 until graphSize))
//    new GSAPVertexBuilder(randomInitialState=false, vertexIds=(0 until graphSize))
//    new WRMIVertexBuilder(randomInitialState=false, memory=0.99,  inertia=0.4)
      new WRMIVertexBuilder(randomInitialState=false, memory=0.99,  inertia=0.4)
//    new MGMWRMIVertexBuilder(randomInitialState=false, fadingMemory=0.9),
//    new JSFPIVertexBuilder(randomInitialState=false, inertia=0.4)
//    new TabuSearchVertexBuilder(randomInitialState=false, pSchedule=0.2, stepsToRemember=250)
  )


  // The total number of runs to be executed in this evaluation run
  GlobalStats.totalNumberOfRuns = repetitions * graphSizes.size * domainSizes.size * meanDegrees.size * numberOfGraphs * algorithmsList.size

  /**
   * How the evaluation procedure works:
   * For each configuration of an algorithm benchmark a DcopEvaluationRun is created.
   * All DcopEvualuationRuns get added to a job queue of fastEval, a EvaluationSuiteCreator object.
   * fastEvel creates for each DcopEvaluationRun a TorqueJob, which stores information about the job submission and JVM-Parameters etc.
   * TorqueJob also stores an execute function which logs information about the test run as well as actually calling the execute function on the DcopEvaluationRun.
   * fastEvel.runEvaluation calls executeJobs on the chosen host supplying all created TorqueJobs.
   */
  var itr = 0

  // The files which hold the graphs generated with a normal degree distribution
  val graphFilesNorm= for (i <- 1 to numberOfGraphs) yield {
    s"graphs/80-14-norm/graph-80-14-$i.txt"
  }

  for (algorithm <- algorithmsList;
       graphSize <- graphSizes;
       meanDegree <- meanDegrees;
       domainSize <- domainSizes; 
       graphNumber <- 0 until numberOfGraphs;
       graphProvider <- List(
           new BinaryConstraintGraphProvider(
               graphSize, meanDegree, domainSize,
               loadFrom =
                 /*Some("graphs/full/full-graph-50.txt", EdgeListFile)*/
                 Some(graphFilesNorm(graphNumber), EdgeListFile)
               ));
      i <- 0 until repetitions)
  {

         val graphBuilder = new GraphBuilder[Any, Any]()

         // Get the adjustment schedule associated with this algorithm
         val execMode = (algorithm.apply(0, Array(1,2,3)).asInstanceOf[ConstrainedVertex[_,_]]).underlyingSignalCollectSchedule

         // Set time limit
         val executionConfig = {
           if (execMode == ExecutionMode.PureAsynchronous)
               ExecutionConfiguration(execMode).withSignalThreshold(0.01).withTimeLimit(5000)
           else
               ExecutionConfiguration(execMode).withSignalThreshold(0.01).withStepsLimit(250)
         }

         // The interval in which to log stats
         val aggregationInterval = if (execMode == ExecutionMode.PureAsynchronous) 10 else 1

         // What edgeBuilder to use with this algorithm
         val edgeBuilder = algorithm match {
           case b: MGMVertexBuilder => (x:Int, y: Int) => new MGMEdge(y)
           case b: MGMWRMIVertexBuilder => (x:Int, y: Int) => new MGMEdge(y)
           case otherwise => (x: Int, y: Int) => new StateForwarderEdge(y)
         }

         // Extra information to log
         // For stats that should be logged but are available only
         // after the algorithms termination
         // see EvaluationSuiteCreator's execute function
         // or DcopEvaluationRun's postExecute function which is called
         // from EvaluationSuiteCreator's execute.
         val extraStats = Map[String,String](
            "graphSize" -> graphSize.toString,
            "domainSize" -> domainSize.toString,
            "meanDegree" -> meanDegree.toString,
            "runNumber" -> i.toString,
            "algorithmName" -> algorithm.toString,
            "graphNumber" -> graphNumber.toString
         )

         // Add the job description to the list of jobs to be run
         fastEval.addJobForEvaluationAlgorithm(
           new DcopEvaluationRun(
             algorithm.toString,
             graphBuilder = graphBuilder,
             vertexBuilder = algorithm,
             edgeBuilder = edgeBuilder,
             graphProvider = graphProvider,
             executionConfiguration = executionConfig.withGlobalTerminationCondition(
               new ThesisTerminationCondition(aggregationInterval)),
             jvmParams = jvmParameters,
             reportMemoryStats = true),

           extraInformation=extraStats
         )

         itr += 1
  }

  import CSVResultHandler._

  val now = Calendar.getInstance.getTime
  val resultsFile =
    s"results/results_${now.getDay}-${now.getMonth}_${now.getHours}-${now.getMinutes}.csv"

  // Keys that know how to extract their associated values from a list
  val keys: Iterable[ValExtractor] = Seq(
        "algorithmName", "graphNumber", "runNumber", "graphSize",
        "domainSize", "meanDegree", "timeToOptimum", "timeToNashEq",
        "computationTimeInMilliseconds", "terminationReason",
        "totalMessagesReceived", "ratioAtTermination",
        "avgNumberMessagesRecievedPerCycle",
        "ratios" splitOn (",")) // the value associated with ratios will automatically be split by , and each element will be added individually

  fastEval.setResultHandlers(
      List(new CSVResultHandler(
          keysToLog=keys, filename=Some(resultsFile))))

  fastEval.runEvaluation
}
