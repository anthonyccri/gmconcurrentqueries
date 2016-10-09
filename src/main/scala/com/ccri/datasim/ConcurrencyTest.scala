package com.ccri.datasim

import java.util.concurrent.{Callable, Executors}

import com.beust.jcommander.{Parameter, ParameterException}
import com.google.common.collect.Queues
import org.geotools.data.{DataStoreFinder, Query, Transaction}
import org.geotools.factory.CommonFactoryFinder
import org.joda.time.format.PeriodFormatterBuilder
import org.joda.time.{DateTime, Interval, Period, Seconds}
import org.slf4j.LoggerFactory

import scala.util.Random

class ConcurrencyTest

object ConcurrencyTest extends App {

  val logger = LoggerFactory.getLogger(classOf[ConcurrencyTest])

  object Args {
    @Parameter(names = Array("-i"), required = true, description = "Instance")
    var instance: String = _

    @Parameter(names = Array("-z"), required = true, description = "Zookeepers")
    var zookeepers: String = _

    @Parameter(names = Array("-u"), required = false, description = "User")
    var user: String = "root"

    @Parameter(names = Array("-p"), required = false, description = "Password")
    var password: String = "secret"

    @Parameter(names = Array("-c"), required = true, description = "Catalog")
    var catalog: String = _

    @Parameter(names = Array("-f"), required = true, description = "Feature")
    var feature: String = _

    @Parameter(names = Array("-t"), required = true, description = "Time Span (yyyy-MM-ddTHH:mm:ss.SSSZ/yyyy-MM-ddTHH:mm:ss.SSSZ")
    var timespan: String = _

    @Parameter(names = Array("-mt"), required = true, description = "Max Time (P10D or P15M")
    var maxtime: String = _

    @Parameter(names = Array("-g"), required = true, description = "Geo Bounds (x1,y1,x2,y2)")
    var geobounds: String = _

    @Parameter(names = Array("-mg"), required = true, description = "Max geo bounds (xoffeset, yoffset)")
    var maxGeo: String = _

    @Parameter(names = Array("-n"), required = false, description = "Iterations (default 1000)")
    var iterations: Integer = new Integer(1000)

    @Parameter(names = Array("-concurrent"), required = false, description = "Max concurrent queries")
    var maxConcurrent: Int = _

    @Parameter(names = Array("-s"), required = false, description = "Seed, default random")
    var seed: Int = Random.nextInt(Int.MaxValue)
  }

  val jc = JCommanderBridge.create(Args)
  try {
    jc.parse(args.toArray: _*)
  } catch {
    case pe: ParameterException =>
      println("error parsing args: " + pe.getMessage)
      jc.usage()
      System.exit(1)
  }
  logger.info(s"Connecting to Accumulo ${Args.instance}, ${Args.catalog} and testing ${Args.feature}")

  val Array(minx, miny, maxx, maxy) = Args.geobounds.split(",").map(_.toDouble)
  val Array(xoffset, yoffset)       = Args.maxGeo.split(",").map(_.toDouble)
  val timespan                      = Interval.parse(Args.timespan)
  val maxtime                       = Period.parse(Args.maxtime)
  logger.debug(s"maxtime is $maxtime")

  import scala.collection.JavaConversions._

  val connectionParams = Map(
    "instanceId" -> Args.instance,
    "zookeepers" -> Args.zookeepers,
    "user"       -> Args.user,
    "password"   -> Args.password,
    "tableName"  -> Args.catalog
  )
  val ds = DataStoreFinder.getDataStore(connectionParams)


  def timeQuery(q: Query, id: Int): (Long, Long) = {
    logger.debug(s"Running query $q with id $id")
    val start = System.currentTimeMillis()
    val fr = ds.getFeatureReader(q, Transaction.AUTO_COMMIT)
    var i = 0L
    while (fr.hasNext) {
      i += 1
      fr.next()
    }
    fr.close()
    val end = System.currentTimeMillis()
    (end - start, i)
  }

  val rand = new java.util.Random(Args.seed)
  val ff = CommonFactoryFinder.getFilterFactory2

  val maxxspan = maxx - xoffset - minx
  val maxyspan = maxy - yoffset - miny
  val start = timespan.getStart
  val end   = timespan.getEnd
  val maxmillisoffset = maxtime.toDurationFrom(start).toStandardSeconds.getSeconds * -1000
  logger.debug(s"maxmillisoffset is $maxmillisoffset")
  val maxseconds = Seconds.secondsBetween(start, end).getSeconds - maxmillisoffset/1000.0
  logger.debug(s"maxseconds is $maxseconds")

  val queries = Queues.newLinkedBlockingQueue[(Query,Int)](Args.iterations + 10)
  (0 until Args.iterations + 10).foreach { _ => queries.put(generateRandomQuery()) }

  def generateRandomQuery(): (Query, Int) = {
    val (lx, ly) = (minx + rand.nextDouble()*maxxspan, miny + rand.nextDouble()*maxyspan)
    val (ux, uy) = (lx + rand.nextDouble()*xoffset, ly + rand.nextDouble()*yoffset)
    logger.debug(s"bounds = $lx, $ly, $ux, $uy")
    val secondstoaddtostart = rand.nextDouble() * maxseconds
    logger.debug(s" Adding $secondstoaddtostart to $start")
    val qstart = start.plusSeconds(secondstoaddtostart.toInt)
    val qend   = qstart.plusMillis((rand.nextDouble() * maxmillisoffset).toInt)
    val f = ff.and(
      ff.bbox("geom", lx, ly, ux, uy, "EPSG:4326"),
      ff.between(ff.property("dtg"), ff.literal(qstart.toDate), ff.literal(qend.toDate)))
    (new Query(Args.feature, f), f.toString.hashCode)
  }

  val es = Executors.newFixedThreadPool(Args.maxConcurrent)

  logger.info("Warming up by sending 10 queries through")
  val warmup = (0 until 10).map { _ =>
    val c = new Callable[(Long, Long)] {
      var result: (Long, Long) = null
      override def call(): (Long, Long) = {
        if(result == null ) {
          result = timeQuery(queries.poll()._1, -1)
        }
        result
      }
    }
    es.submit(c)
  }
  logger.info(s"Warm up length = ${warmup.length}")
  warmup.foreach(_.get())

  case class QueryResult(q: Query, id: Int, elapsed: Long, resultCount: Long)

  val expStart = DateTime.now()
  logger.info("Starting test")
  val callables =
    (0 until Args.iterations).map { _ =>
      val c = new Callable[QueryResult] {
        var result: QueryResult = null
        override def call(): QueryResult = {
          if(result == null) {
            val (q, id) = queries.poll()
            val (elapsed, count) = timeQuery(q, id)
            result = QueryResult(q, id, elapsed, count)
          }
          result
        }
      }
      es.submit(c)
    }

  logger.info("Submitted all queries, waiting for completion")

  callables.foreach { c =>
    val QueryResult(q, id, elapsedMillis, count) = c.get()
    logger.info(s"Completed query ($id) in ${elapsedMillis/1000.0} seconds with $count results")
  }

  logger.info("Shutting down concurrent query executors")
  es.shutdownNow()

  val expEnd = DateTime.now()
  val yearsAndMonths = new PeriodFormatterBuilder()
    .printZeroAlways()
    .appendMinutes()
    .appendSuffix(" minute", " minutes")
    .appendSeconds()
    .appendSuffix(" second", " seconds")
    .toFormatter

  logger.info("Disposing data store")
  ds.dispose()

  logger.info(s"Experiment completed in ${yearsAndMonths.print(new Interval(expStart, expEnd).toPeriod)}")
}
