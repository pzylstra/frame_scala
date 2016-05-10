package ffm.io.r

import org.tmatesoft.sqljet.core.SqlJetException
import org.tmatesoft.sqljet.core.SqlJetTransactionMode
import org.tmatesoft.sqljet.core.table.ISqlJetTable
import org.tmatesoft.sqljet.core.table.SqlJetDb

import ffm.fire.FireModelResult
import ffm.fire.IgnitionPath
import ffm.fire.StratumFlameSummary
import ffm.fire.StratumOutcome
import ffm.fire.SurfaceOutcome
import ffm.forest.Site
import ffm.forest.StratumLevel
import ffm.util.Counter

/**
 * Manages writing of results to a SQLite database.
 * 
 * This class manages a connection to a SQLite database
 * and provides methods to write simulation results. The only methods
 * of interest to a client are [[insertResult]], [[isOpen]], and [[close]].
 * 
 * Database instances are created with the companion [[Database$]] object.
 * 
 * @param base the underlying SqlJet database object.
 * 
 * @param useTransactions whether to use database transactions (commit / rollback) when 
 *   writing data.
 * 
 */
class Database (val base: SqlJetDb, val useTransactions: Boolean) {

  import Database._

  private val repCounter = Counter[Long](from = 1L, step = 1L)

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
  def insertResult(res: FireModelResult): Boolean = {
    val s = new Session(res)
    s.insert()
  }

  /**
   * Checks if this database is open and available for writing.
   * 
   * @param checkWritable if `true` (default) check that the database
   *   can be written to; if `false`, only check that the database is open.
   */
  def isOpen(checkWritable: Boolean = true): Boolean =
    if (checkWritable) base.isOpen && base.isWritable
    else base.isOpen

  /**
   * Closes the database.
   */
  def close(): Unit = base.close()
  
  
  /**
   * Provides variables and methods for the insertion of a single
   * model result.
   */
  class Session(res: FireModelResult) {
    /** Auto-incremented replicate ID. */
    val repId = repCounter.next()
   
    /** The method to write data to tables with or without transactions. */
    val writer = 
      if (useTransactions) Writer.withTransaction(base)_
      else Writer.withoutTransaction(base)_

    
    /** Inserts the result which was passed to the Session constructor. */
    def insert(): Boolean = {
      insertSiteData()
      insertLevels()
      insertStratumResults(res)
      insertRunResults(res)
      
      // TODO: set this flag based on success/failure of db transactions
      true
    }
    
    /** Inserts site meta-data into the Sites table. */
    def insertSiteData(): Boolean = {
      writer {
        val tbl = base.getTable(TableSites.name)
        val rec = TableSites.Rec(repId, res.site)
        TableSites.inserter(tbl, rec)
      }
    }

    /** Inserts data into the Strata table. */
    def insertLevels(): Boolean = {
      val levels = res.site.vegetation.strata map (_.level)
      
      writer {
        val tbl = base.getTable(TableStrata.name)
        val rec = TableStrata.Rec(repId, levels)
        TableStrata.inserter(tbl, rec)
      }
    }
    
    /** Inserts overall summary results into the StratumResults table. */
    def insertStratumResults(res: FireModelResult): Boolean = {
      writer { 
        val tbl = base.getTable(TableStratumResults.name)
        for (sr <- res.stratumResults) {
          val rec = TableStratumResults.Rec(repId, sr)
          TableStratumResults.inserter(tbl, rec)
        }
      }
    }

    /** Inserts results for each ignition run (either 1 or 2). */
    def insertRunResults(res: FireModelResult): Boolean = {
      isAllTrue { 
        res.runResults.zipWithIndex map { case (runres, i) =>
          val runId = i + 1L  
          insertSurfaceResult(runres.surfaceOutcome, runId)
          insertStratumOutcomes(runres.stratumOutcomes, runId)
        }
      }
    }
    
    /**
     * Inserts results for surface flames and wind.
     * 
     * @param runIndex ignitino run identifier: either 1 or 2.
     */
    def insertSurfaceResult(surf: SurfaceOutcome, runIndex: Long): Boolean = {
      writer {  
        val tbl = base.getTable(TableSurfaceResults.name)
        val rec = TableSurfaceResults.Rec(repId, runIndex, surf)
        TableSurfaceResults.inserter(tbl, rec)
      }
    }

    /**
     * Inserts results for each stratum in an ignition run.
     * 
     * @param runIndex ignition run identifier: either 1 or 2.
     * @param souts sequences of [[ffm.fire.StratumOutcome]] objects.
     */
    def insertStratumOutcomes(souts: IndexedSeq[StratumOutcome], runIndex: Long): Boolean = {
      writer { 
        val tbl = base.getTable(TableIgnitionPaths.name)
        souts foreach { sout =>
          val rec = TableIgnitionPaths.Rec(repId, runIndex, sout)
          TableIgnitionPaths.inserter(tbl, rec)
        }
      }
    }

    /** 
     *  Checks if a sequence of logical values are all true.
     */
    def isAllTrue(bs: Seq[Boolean], resultIfEmpty: Boolean = false): Boolean =
      if (bs.isEmpty) resultIfEmpty
      else bs.reduce(_ && _)
    
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

  /**
   * Creates a new database.
   * 
   * @param path directory path and name for the database file.
   * 
   * @param deleteIfExists if `false` and the file given by `path` exists,
   *   do not create the database ([[None]] is returned); 
   *   if `true` (default) any existing file is deleted.
   *   
   * @param useTransactions if `true` all database insertions into the creted
   *   database will be done within WRITE transactions; if `false` all insertions
   *   will be done directly which is much faster but less safe.
   *   
   * @return An [[Option]] containing the [[Database$]] instance if 
   *   creation was successful, otherwise [[None]].
   */
  def create(path: String, deleteIfExists: Boolean, useTransactions: Boolean): Option[Database] = {
    require(path.trim() != "", "database path must be provided")
    
    val f = new java.io.File(path)
    
    if (checkFile(f, deleteIfExists)) Some( createDB(f, useTransactions) )
    else None
  }
  
  private def checkFile(f: java.io.File, deleteIfExists: Boolean): Boolean = {
    if (!f.exists()) true
    else if (deleteIfExists) f.delete()
    else false
  }
  
  private def createDB(f: java.io.File, useTransactions: Boolean): Database = {
    val db = SqlJetDb.open(f, true)
    db.getOptions.setAutovacuum(true)

    db.beginTransaction(SqlJetTransactionMode.WRITE)
    try {
      db.getOptions.setUserVersion(1)

      db.createTable(TableSites.createSQL)
      db.createTable(TableStrata.createSQL)
      db.createTable(TableStratumResults.createSQL)
      db.createTable(TableSurfaceResults.createSQL)
      db.createTable(TableIgnitionPaths.createSQL)
      db.commit()
      
      new Database(db, useTransactions)

    } catch {
      case ex: Exception => 
        db.rollback()
        throw ex
    }
  }

}

/**
 * Executes statements to write data to a database.
 * 
 * Example:
 * {{{
 * Writer.withTransaction(baseDb) {
 *   val tbl = base.getTable(TableStrata.name)
 *   val rec = TableStrata.Rec(repId, levelsLookup)
 *   TableStrata.inserter(tbl, rec)
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
  def withTransaction(base: SqlJetDb)(stmts: => Unit): Boolean = {
    base.beginTransaction(SqlJetTransactionMode.WRITE)
    try {
      stmts
      base.commit()
      true
      
    } catch {
      case ex: SqlJetException =>
        base.rollback()
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
  def withoutTransaction(base: SqlJetDb)(stmts: => Unit): Boolean = {
    try {
      stmts
      true
      
    } catch {
      case ex: SqlJetException =>
        false
    }
  }
}


/**
 * Base trait for database table objects.
 */
sealed trait Table {
  /** Abstract type parameter for a table record, to be implemented by Table classes. */
  type Record

  /** Table name. */
  def name: String
  
  /** SQL statement to create table. */
  def createSQL: String

  /** Method to insert a data record into a table. */
  def inserter(tbl: ISqlJetTable, data: Record): Unit

  // Inserting numbers into a ISqlJetTable requires them
  // being passed as Object, hence these methods:
  
  /** Wraps a Long value as an AnyRef as required by SqlJet methods. */
  def &(x: Long) = x.asInstanceOf[AnyRef]
  
  /** Wraps a Double value as an AnyRef as required by SqlJet methods. */
  def &(x: Double) = x.asInstanceOf[AnyRef]
}


/////////////////////////////////////////////////////////////////////////////
object TableSites extends Table {
  val name = "Sites"

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

  case class Rec(repId: Long, site: Site)
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit = {
    val w = rec.site.weather
    val s = rec.site.surface
    tbl.insert(
        &(rec.repId), 
        &(w.windSpeed),
        &(w.temperature),
        &(s.slope),
        &(s.fuelLoad),
        &(s.meanFinenessLeaves),
        &(s.meanFuelDiameter),
        &(s.deadFuelMoistureProp))     
  }
}


/////////////////////////////////////////////////////////////////////////////
object TableStrata extends Table {
  val name = "Strata"

  val createSQL = """
      CREATE TABLE Strata (
      repId INT NOT NULL, 
      level TEXT NOT NULL,
      PRIMARY KEY(repId, level))"""

  case class Rec(repId: Long, levels: IndexedSeq[StratumLevel])
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit = {
    rec.levels foreach { level =>
      tbl.insert(&(rec.repId), level.toString)
    }
  }
}

/////////////////////////////////////////////////////////////////////////////
object TableStratumResults extends Table {
  val name = "StratumResults"

  val createSQL = """
      CREATE TABLE StratumResults(
      repId INT NOT NULL,
      level TEXT NOT NULL,
      flameLength REAL,
      flameAngle REAL,
      flameHeight REAL,
      PRIMARY KEY(repId, level))"""

  case class Rec(repId: Long, flameSummary: StratumFlameSummary)
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit =
    tbl.insert(
      &(rec.repId),
      rec.flameSummary.level.toString,
      &(rec.flameSummary.flameLength),
      &(rec.flameSummary.flameAngle),
      &(rec.flameSummary.flameHeight))
}

/////////////////////////////////////////////////////////////////////////////
object TableSurfaceResults extends Table {
  val name = "SurfaceResults"

  val createSQL = """
      CREATE TABLE SurfaceResults(
      repId INT NOT NULL,
      runIndex INT NOT NULL,
      windSpeed REAL,
      flameLength REAL,
      flameAngle REAL,
      flameHeight REAL,
      PRIMARY KEY(repId, runIndex))"""

  case class Rec(repId: Long, runIndex: Long, surf: SurfaceOutcome)
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit =
    tbl.insert(
      &(rec.repId),
      &(rec.runIndex),
      &(rec.surf.windSpeed),
      &(rec.surf.flameSummary.flameLength),
      &(rec.surf.flameSummary.flameAngle),
      &(rec.surf.flameSummary.flameHeight))
}

/////////////////////////////////////////////////////////////////////////////
object TableIgnitionPaths extends Table {
  val name = "IgnitionPaths"

  val createSQL = """
        CREATE TABLE IgnitionPaths(
        repId INT NOT NULL,
        runIndex INT NOT NULL,
        level TEXT NOT NULL,
        pathType TEXT NOT NULL,
        species TEXT NOT NULL,
        x0 REAL,
        y0 REAL,
        x1 REAL,
        y1 REAL,
        length REAL)"""

  case class Rec(repId: Long, runIndex: Long, so: StratumOutcome)
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit = {
    worker("plant", rec.so.plantPaths)
    worker("stratum", rec.so.stratumPaths)

    def worker(pathType: String, paths: IndexedSeq[IgnitionPath]) {
      paths foreach { pp =>
        val spc = pp.speciesComponent
        val spname = spc.species.name

        pp.segments foreach { seg =>
          tbl.insert(
            &(rec.repId),
            &(rec.runIndex),
            rec.so.stratum.level.toString,
            pathType,
            spname,
            &(seg.start.x),
            &(seg.start.y),
            &(seg.end.x),
            &(seg.end.y),
            &(seg.length))
        }
      }
    }
  }
}

