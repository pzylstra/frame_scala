package ffm.io.r

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException

import ffm.fire.FireModelResult
import ffm.fire.FireModelRunResult
import ffm.fire.StratumPathsFlames


/**
 * Wraps a ResultSet object as a Scala iterator.
 * 
 * It is handy to have access a ResultSet like a Scala iterator because
 * we can then use methods such as `map` on the records.
 * 
 * @param rs the ResultSet object.
 */
class ResultSetIterator(rs: ResultSet) extends Iterator[ResultSet] {
    def hasNext: Boolean = rs.next()
    def next: ResultSet = rs
}


/**
 * Manages writing of results to a SQLite database.
 * 
 * This class manages a connection to a SQLite database
 * and provides methods to write simulation results. The only methods
 * of interest to a client are [[insertResult]], [[isOpen]], and [[close]].
 * 
 * Database instances are created with the companion [[Database$]] object.
 * 
 * @param conn the underlying JDBC database connection.
 * 
 * @param useTransactions whether to use database transactions (commit / rollback) when 
 *   writing data.
 * 
 */
class Database (val conn: Connection, val useTransactions: Boolean) {

  import Database._
  
  /**
   * Insert a result into this database. 
   * 
   * The integer replicate ID in all database tables will be 
   * auto-incremented (start value 1).
   * 
   * @param res a result object. 
   *   
   * @return `true` if data were written successfully.
   */
  def insertResult(res: FireModelResult): Unit = {
    val s = new Session()
    s.insertResults(res)
  }

  /**
   * Checks if this database is open and available for writing.
   * 
   * @param checkWritable if `true` (default) check that the database
   *   can be written to; if `false`, only check that the database is open.
   */
  def isOpen(checkWritable: Boolean = true): Boolean = {
    if (conn.isClosed()) false
    else if (checkWritable) !conn.isReadOnly()
    else true
  }

  /**
   * Closes the database.
   */
  def close(): Unit = conn.close()
  

  /**
   * Provides variables and methods for the insertion of a single
   * model result.
   */
  class Session() {
    /** Integer replicate ID value to use in all tables updated in this session. */
    val repId: Int = {
      val stmt = conn.createStatement()
      stmt.setQueryTimeout(5) // query timeout of 5 seconds

      try {
        val resN = stmt.executeQuery("select count(*) as N from Sites")
        resN.next()
        val nrecs = resN.getInt(1)

        val id = 
          if (nrecs == 0) 1
          else {
            val resId = stmt.executeQuery("select max(repId) as R from Sites")
            resId.next()
            resId.getInt(1)
          }
        
        id

      } catch {
        case e: Throwable => throw new Error(e)
      } finally {
        stmt.close()
      }
    }
 
   
    /** The method to write data to tables with or without transactions. */
    val writer = 
      if (useTransactions) Writer.withTransaction(conn)_
      else Writer.withoutTransaction(conn)_

    
    /** Inserts the result which was passed to the Session constructor. */
    def insertResults(res: FireModelResult): Unit = {
      writer {
        TableSites.inserter(conn, repId, res)
        TableStrata.inserter(conn, repId, res)
        TableRuns.inserter(conn, repId, res)
        TableFlameSummaries.inserter(conn, repId, res)
        TableIgnitionPaths.inserter(conn, repId, res)
        TableSurfaceResults.inserter(conn, repId, res)
        TableROS.inserter(conn, repId, res)
      }
    }
       
  }
}


/**
 * Companion object used to create [[Database!]] instances.
 * 
 * Note: originally this object also contained the traits and objects
 * representing database tables, but these were later moved out to 
 * fix odd `class not found` errors when running from R.
 */
object Database {
  
  val TableNames = Vector(
      "Sites",
      "Runs",
      "Strata",
      "FlameSummaries",
      "SurfaceResults",
      "IgnitionPaths",
      "ROS")

      
  /** Integer index to identify a run with the canopy. */
  val RunWithCanopy = 1
  
  /** Integer index to identify a run without the canopy. */
  val RunWithoutCanopy = 2
      
      
  /**
   * Creates a new Database object based on a new or existing database file.
   * 
   * @param path directory path and name for the database file.
   * 
   * @param deleteIfExists if `true` the file given by `path` will be deleted 
   *   and re-created if it already exists
   *   
   * @param useTransactions if `true` all database insertions into the database 
   *   will be done within WRITE transactions; if `false` all insertions will
   *   be done directly which is much faster but less safe.
   *   
   * @return An [[Option]] containing the [[Database$]] instance if 
   *   creation was successful, otherwise [[None]].
   */
  def create(path: String, deleteIfExists: Boolean, useTransactions: Boolean): Option[Database] = {
    require(path.trim() != "", "database path must be provided")
    
    val f = new java.io.File(path)
    
    if (f.exists()) {
      if (deleteIfExists) {
        // Delete existing file if possible and create a new database
        if (f.delete()) createDB(f, useTransactions)
        else None
        
      } else {
        // Open existing database
        openDB(f, useTransactions)
      }
      
    } else {
      // New file
      createDB(f, useTransactions)
    }
  }
  
  private def createDB(f: java.io.File, useTransactions: Boolean): Option[Database] = {
    val conn = DriverManager.getConnection("jdbc:sqlite:" + f.getAbsolutePath)

    val stmt = conn.createStatement()
    conn.setAutoCommit(true)    

    try {
      for (tblname <- TableNames) {
        val sql = "drop table if exists " + tblname
        stmt.executeUpdate(sql)
      }
      
      stmt.executeUpdate(TableSites.createSQL)
      stmt.executeUpdate(TableRuns.createSQL)
      stmt.executeUpdate(TableStrata.createSQL)
      stmt.executeUpdate(TableFlameSummaries.createSQL)
      stmt.executeUpdate(TableSurfaceResults.createSQL)
      stmt.executeUpdate(TableIgnitionPaths.createSQL)
      stmt.executeUpdate(TableROS.createSQL)
      
      Some( new Database(conn, useTransactions) )
      
    } catch {
      case ex: SQLException =>
        conn.close()
        throw ex
      
    } finally {
      stmt.close()
    }
  }
  
  private def openDB(f: java.io.File, useTransactions: Boolean): Option[Database] = {
    val conn = DriverManager.getConnection("jdbc:sqlite:" + f.getAbsolutePath)
    
    val rs = new ResultSetIterator( conn.getMetaData().getTables(null, null, "%", null) )

    val existingTables = rs.map(r => r.getString(3).toLowerCase()).toSet
 
    val expectedTables = (TableNames map (_.toLowerCase()))
    
    val found = expectedTables map (tbl => existingTables.contains(tbl))
    if (found.contains(false)) {
      // One or more of the required tables missing
      None
    } else {
      // All required tables present
      Some( new Database(conn, useTransactions) )
    }
  }
  
}

/**
 * Executes statements to write data to a database.
 * 
 * Example:
 * {{{
 * val repId: Int = ...
 * val res: FireModelResult = ...
 * 
 * Writer.withTransaction(conn) {
 *   TableSites.inserter(conn, repId, res)
 * }
 * }}}
 */
object Writer {
  /**
   * Executes database statements within a WRITE transaction.
   * 
   * Takes a SqlJet database object plus a block of statements, and 
   * executes the statements within a WRITE transaction.  If the 
   * statements are executed successfully the transaction is committed
   * and the method returns `true`; otherwise the transaction is rolled 
   * back and the method returns `false`.
   * 
   * @param base a SqlJet database object
   * @param stmts block of statements to execute
   * @return `true` if successful
   */
  def withTransaction(conn: Connection)(stmts: => Unit): Boolean = {
    conn.setAutoCommit(false)
    try {
      stmts
      conn.commit()
      true
      
    } catch {
      case ex: SQLException =>
        conn.rollback()
        false
    }
  }
  
  /**
   * Executes database statements directly (no transaction).
   * 
   * Takes a SqlJet database object plus a block of statements, and 
   * attempts to execute the statements directly. This will be faster,
   * but less safe, than using transactions over large numbers of
   * operations.
   * 
   * @param base a SqlJet database object
   * @param stmts block of statements to execute
   * @return `true` if successful
   */
  def withoutTransaction(conn: Connection)(stmts: => Unit): Boolean = {
    conn.setAutoCommit(true)
    try {
      stmts
      true
      
    } catch {
      case ex: SQLException =>
        false
    }
  }
}


/**
 * Base trait for database table objects.
 */
sealed trait Table {
  /** Table name. */
  def name: String
  
  def Fields: Enumeration
  
  /** SQL statement to create table. */
  def createSQL: String
  
  /** SQL statement to insert values. */
  def insertSQL: String

  /** Method to insert a data record into a table. */
  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit
}


/////////////////////////////////////////////////////////////////////////////
object TableSites extends Table {
  val name = "Sites"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val WindSpeed, Temperature, Slope, FuelLoad = Value
    val MeanFinenessLeaves, MeanFuelDiameter, DeadFuelMoistureProp = Value
  }

  val createSQL = """
      CREATE TABLE Sites (
      repId INT NOT NULL, 
      windSpeed REAL,
      temperature REAL,
      slope REAL,
      fuelLoad REAL,
      meanFinenessLeaves REAL,
      meanFuelDiameter REAL,
      deadFuelMoistureProp REAL,
      
      PRIMARY KEY(repId))"""

  val insertSQL = """
      INSERT INTO Sites 
      (repId, windSpeed, temperature, slope, fuelLoad, 
      meanFinenessLeaves, meanFuelDiameter, deadFuelMoistureProp) VALUES
      (?,?,?,?,?,?,?,?)
    """
  
  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val site = res.site
    
    val pstmt = conn.prepareStatement(insertSQL)
    
    pstmt.setInt(Fields.RepId.id, repId)
    pstmt.setDouble(Fields.WindSpeed.id, site.weather.windSpeed)
    pstmt.setDouble(Fields.Temperature.id, site.weather.temperature)
    pstmt.setDouble(Fields.Slope.id, site.surface.slope)
    pstmt.setDouble(Fields.FuelLoad.id, site.surface.fuelLoad)
    pstmt.setDouble(Fields.MeanFinenessLeaves.id, site.surface.meanFinenessLeaves)
    pstmt.setDouble(Fields.MeanFuelDiameter.id, site.surface.meanFuelDiameter)
    pstmt.setDouble(Fields.DeadFuelMoistureProp.id, site.surface.deadFuelMoistureProp)
    
    pstmt.executeUpdate()
    pstmt.close()
  }
}


/////////////////////////////////////////////////////////////////////////////
object TableRuns extends Table {
  val name = "Runs"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val RunIndex, CanopyIncluded = Value
  }

  val createSQL = """
      CREATE TABLE Runs (
      repId INT NOT NULL,
      runIndex INT NOT NULL, 
      canopyIncluded BOOLEAN,
      PRIMARY KEY(repId, runIndex))"""
  
  val insertSQL = """
      INSERT INTO Runs
      (repId, runIndex, canopyIncluded) VALUES
      (?,?,?)
    """


  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val pstmt = conn.prepareStatement(insertSQL)
    
    pstmt.setInt(Fields.RepId.id, repId)
    pstmt.setInt(Fields.RunIndex.id, Database.RunWithCanopy) 
    pstmt.setBoolean(Fields.CanopyIncluded.id, true)
    pstmt.addBatch()
    
    pstmt.setInt(Fields.RepId.id, repId)
    pstmt.setInt(Fields.RunIndex.id, Database.RunWithoutCanopy) 
    pstmt.setBoolean(Fields.CanopyIncluded.id, false)
    pstmt.addBatch()
    
    pstmt.executeBatch()
    pstmt.close()
  }
}


/////////////////////////////////////////////////////////////////////////////
object TableStrata extends Table {
  val name = "Strata"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val Level = Value
  }

  val createSQL = """
      CREATE TABLE Strata (
      repId INT NOT NULL, 
      level TEXT NOT NULL,
      PRIMARY KEY(repId, level))"""

  val insertSQL = """
      INSERT INTO Strata
      (repId, level) VALUES (?,?)
    """
  
  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val levels = res.site.vegetation.strata map (_.level)
    val pstmt = conn.prepareStatement(insertSQL)
    
    levels foreach { level =>
      pstmt.setInt(Fields.RepId.id, repId)
      pstmt.setString(Fields.Level.id, level.toString())
      pstmt.addBatch()
    }
    
    pstmt.executeBatch()
    pstmt.close()
  }
}


/////////////////////////////////////////////////////////////////////////////
object TableFlameSummaries extends Table {
  val name = "FlameSummaries"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val RunIndex, Level, FlameLength, FlameAngle, FlameHeight = Value
  }

  val createSQL = """
      CREATE TABLE FlameSummaries(
      repId INT NOT NULL,
      runIndex INT NOT NULL,
      level TEXT NOT NULL,
      flameLength REAL,
      flameAngle REAL,
      flameHeight REAL,
      PRIMARY KEY(repId, runIndex, level))"""
  
  val insertSQL = """
      INSERT INTO FlameSummaries
      (repId, runIndex, level, flameLength, flameAngle, flameHeight) VALUES
      (?,?,?,?,?,?)
    """

  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val pstmt = conn.prepareStatement(insertSQL)
  
    def dorun(runIndex: Int, runres: FireModelRunResult) = {
      runres.flameSummaries.values foreach { fsum =>
        pstmt.setInt(Fields.RepId.id, repId)
        pstmt.setInt(Fields.RunIndex.id, runIndex)
        pstmt.setString(Fields.Level.id, fsum.level.toString())
        pstmt.setDouble(Fields.FlameLength.id, fsum.flameLength)
        pstmt.setDouble(Fields.FlameAngle.id, fsum.flameAngle)
        pstmt.setDouble(Fields.FlameHeight.id, fsum.flameHeight)
        
        pstmt.addBatch()
      }
    }
    
    dorun(Database.RunWithCanopy, res.resWithCanopyEffect)
    dorun(Database.RunWithoutCanopy, res.resWithoutCanopyEffect)
    
    pstmt.executeBatch()
    pstmt.close()
  }
}


/////////////////////////////////////////////////////////////////////////////
object TableSurfaceResults extends Table {
  val name = "SurfaceResults"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val RunIndex, WindSpeed, FlameLength, FlameAngle, FlameHeight = Value
  }

  val createSQL = """
      CREATE TABLE SurfaceResults(
      repId INT NOT NULL,
      runIndex INT NOT NULL,
      windSpeed REAL,
      flameLength REAL,
      flameAngle REAL,
      flameHeight REAL,
      PRIMARY KEY(repId, runIndex))"""
  
  val insertSQL = """
      INSERT INTO SurfaceResults
      (repId, runIndex, windSpeed, flameLength, flameAngle, flameHeight) VALUES
      (?,?,?,?,?,?)
    """

  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val pstmt = conn.prepareStatement(insertSQL)
    
    def dorun(runIndex: Int, runres: FireModelRunResult) = {
      val sout = runres.surfaceOutcome
      
      pstmt.setInt(Fields.RepId.id, repId)
      pstmt.setInt(Fields.RunIndex.id, runIndex)
      pstmt.setDouble(Fields.WindSpeed.id, sout.windSpeed)
      pstmt.setDouble(Fields.FlameLength.id, sout.flameSummary.flameLength)
      pstmt.setDouble(Fields.FlameAngle.id, sout.flameSummary.flameAngle)
      pstmt.setDouble(Fields.FlameHeight.id, sout.flameSummary.flameHeight)
      
      pstmt.addBatch()
    }
    
    dorun(Database.RunWithCanopy, res.resWithCanopyEffect)
    dorun(Database.RunWithoutCanopy, res.resWithoutCanopyEffect)
    
    pstmt.executeBatch()
    pstmt.close()
  }
    
}


/////////////////////////////////////////////////////////////////////////////
object TableIgnitionPaths extends Table {
  val name = "IgnitionPaths"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val RunIndex, Level, PathType, Species, SegIndex, X0, Y0, X1, Y1, Length, FlameLength = Value
  }

  val createSQL = """
      CREATE TABLE IgnitionPaths(
        repId INT NOT NULL,
        runIndex INT NOT NULL,
        level TEXT NOT NULL,
        pathType TEXT NOT NULL,
        species TEXT NOT NULL,
        segIndex INT,
        x0 REAL,
        y0 REAL,
        x1 REAL,
        y1 REAL,
        length REAL,
        flameLength REAL,
        PRIMARY KEY(repId, runIndex, level, pathType, species, segIndex))
    """
  
  val insertSQL = """
      INSERT INTO IgnitionPaths
      (repId, runIndex, level, pathType, species, segIndex, x0, y0, x1, y1, length, flameLength) 
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
    """

  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val pstmt = conn.prepareStatement(insertSQL)

    def dorun(runIndex: Int, runres: FireModelRunResult): Unit = {
      runres.pathsAndFlames.values foreach { pnf =>      
        worker(pnf, "plant")
        worker(pnf, "stratum")
      }
      
      def worker(pathsFlames: StratumPathsFlames, pathType: String) {
        val level = pathsFlames.stratum.level
 
        val (paths, flames) = pathType match {
          case "plant"   => (pathsFlames.plantPaths, pathsFlames.plantFlameSeries)
          case "stratum" => (pathsFlames.stratumPaths, pathsFlames.stratumFlameSeries)

          // just in case
          case _         => throw new Error("Unrecognized path type: " + pathType)
        }

        paths foreach { pp =>
          val spc = pp.speciesComponent
          val spname = spc.species.name

          (0 until pp.segments.size) foreach { i =>
            val seg = pp.segments(i)
            
            pstmt.setInt(Fields.RepId.id, repId)
            pstmt.setInt(Fields.RunIndex.id, runIndex)
            pstmt.setString(Fields.Level.id, level.toString)
            pstmt.setString(Fields.PathType.id, pathType)
            pstmt.setString(Fields.Species.id, spname)
            pstmt.setInt(Fields.SegIndex.id, i)
            pstmt.setDouble(Fields.X0.id, seg.start.x)
            pstmt.setDouble(Fields.Y0.id, seg.start.y)
            pstmt.setDouble(Fields.X1.id, seg.end.x)
            pstmt.setDouble(Fields.Y1.id, seg.end.y)
            pstmt.setDouble(Fields.Length.id, seg.length)
            pstmt.setDouble(Fields.FlameLength.id, seg.flameLength)

            pstmt.addBatch()
          }
        }
      }

    }

    dorun(Database.RunWithCanopy, res.resWithCanopyEffect)
    dorun(Database.RunWithoutCanopy, res.resWithoutCanopyEffect)
    
    pstmt.executeBatch()
    pstmt.close()
  }
}


/////////////////////////////////////////////////////////////////////////////
object TableROS extends Table {
  val name = "ROS"
  
  object Fields extends Enumeration {
    val RepId = Value(1)
    val RunIndex, Level, Ros = Value
  }

  val createSQL = """
      CREATE TABLE ROS(
      repId INT NOT NULL,
      runIndex INT NOT NULL,
      level TEXT NOT NULL,
      ros REAL,
      PRIMARY KEY(repId, runIndex, level))"""
  
  val insertSQL = """
      INSERT INTO ROS
      (repId, runIndex, level, ros) VALUES
      (?,?,?,?)
    """

  def inserter(conn: Connection, repId: Int, res: FireModelResult): Unit = {
    val pstmt = conn.prepareStatement(insertSQL)
    
    def dorun(runIndex: Int, runres: FireModelRunResult) = {
		  runres.ratesOfSpread foreach { case (level, ros) =>
		    pstmt.setInt(Fields.RepId.id, repId)
		    pstmt.setInt(Fields.RunIndex.id, runIndex)
		    pstmt.setString(Fields.Level.id, level.toString())
		    pstmt.setDouble(Fields.Ros.id, ros)
		    
		    pstmt.addBatch()
		  }
    }
        
    dorun(Database.RunWithCanopy, res.resWithCanopyEffect)
    dorun(Database.RunWithoutCanopy, res.resWithoutCanopyEffect)

    pstmt.executeBatch()
    pstmt.close()
  }
}

