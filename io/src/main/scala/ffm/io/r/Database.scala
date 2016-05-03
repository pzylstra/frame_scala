package ffm.io.r

import org.tmatesoft.sqljet._
import org.tmatesoft.sqljet.core.SqlJetTransactionMode
import org.tmatesoft.sqljet.core.table.SqlJetDb

import ffm.fire.FireModelResult
import ffm.forest.StratumLevel
import org.tmatesoft.sqljet.core.internal.SqlJetTransactionState
import ffm.util.Counter
import ffm.fire.FireModelRunResult
import ffm.fire.SurfaceOutcome
import ffm.fire.StratumOutcome
import ffm.fire.IgnitionPath
import org.tmatesoft.sqljet.core.table.ISqlJetTable
import ffm.fire.StratumFlameSummary
import ffm.forest.SpeciesComponent
import ffm.forest.Species

class Database private (base: SqlJetDb) {

  import Database._

  private val repCounter = Counter[Long](from = 1L, step = 1L)

  def insertResult(res: FireModelResult): Boolean = {
    val s = new Session(res)
    s.insert()
  }

  def close(): Unit = base.close()

  class Session(res: FireModelResult) {
    val repId = repCounter.next()

    val strata = res.site.vegetation.strata
    val levels = strata map (_.level)
    val levelsLookup: Map[StratumLevel, Long] = Map() ++ (levels zip (1L to levels.size))

    case class SpeciesInLevel(level: StratumLevel, sp: Species)

    val spInLevels = strata flatMap (s => s.speciesComponents.map(sc => SpeciesInLevel(s.level, sc.species)))
    val sppLookup: Map[SpeciesInLevel, Long] = Map() ++ (spInLevels zip (1L to spInLevels.size))

    def insert(): Boolean = {
      insertLevels()
      insertStratumResults(res)
      insertRunResults(res)
      true
    }

    def insertLevels(): Unit = {
      base.beginTransaction(SqlJetTransactionMode.WRITE)
      try {
        val tbl = base.getTable(TableStrata.name)
        val rec = TableStrata.Rec(repId, levelsLookup)
        TableStrata.inserter(tbl, rec)
      } finally {
        base.commit()
      }
    }

    def insertStratumResults(res: FireModelResult): Unit = {
      base.beginTransaction(SqlJetTransactionMode.WRITE)
      try {
        val tbl = base.getTable(TableStratumResults.name)
        for (sr <- res.stratumResults) {
          val levelId = levelsLookup(sr.level)
          val rec = TableStratumResults.Rec(repId, levelId = levelId, flameSummary = sr)
          TableStratumResults.inserter(tbl, rec)
        }
      } finally {
        base.commit()
      }
    }

    def insertRunResults(res: FireModelResult): Unit = {
      insertRunResult(res.runResults(0), 1L)
      if (res.runResults.size > 1) insertRunResult(res.runResults(1), 2L)
    }

    def insertRunResult(runres: FireModelRunResult, runIndex: Long): Unit = {
      insertSurfaceResult(runres.surfaceOutcome, runIndex)
      insertStratumOutcomes(runres.stratumOutcomes, runIndex)
    }

    def insertSurfaceResult(surf: SurfaceOutcome, runIndex: Long): Unit = {
      base.beginTransaction(SqlJetTransactionMode.WRITE)
      try {
        val tbl = base.getTable(TableSurfaceResults.name)
        val rec = TableSurfaceResults.Rec(repId, runIndex, surf)
        TableSurfaceResults.inserter(tbl, rec)

      } finally {
        base.commit()
      }
    }

    def insertStratumOutcomes(souts: IndexedSeq[StratumOutcome], runIndex: Long): Unit = {
      base.beginTransaction(SqlJetTransactionMode.WRITE)
      try {
        val tbl = base.getTable(TableIgnitionPaths.name)
        souts foreach { sout =>
          val levelId = levelsLookup(sout.stratum.level)
          val rec = TableIgnitionPaths.Rec(repId, runIndex, levelId, sout)
          TableIgnitionPaths.inserter(tbl, rec)
        }
      } finally {
        base.commit()
      }
    }
  }
}

object Database {

  def create(path: String, deleteIfExists: Boolean = false): Database = {
    val f = new java.io.File(path)
    if (f.exists()) {
      if (deleteIfExists) f.delete()
      else throw new Error(s"Output file $path exists")
    }

    val db = SqlJetDb.open(f, true)
    db.getOptions.setAutovacuum(true)

    db.beginTransaction(SqlJetTransactionMode.WRITE)
    try {
      db.getOptions.setUserVersion(1)

      db.createTable(TableStrata.createSQL)
      db.createTable(TableStratumResults.createSQL)
      db.createTable(TableSurfaceResults.createSQL)
      db.createTable(TableIgnitionPaths.createSQL)

      db.commit()
      new Database(db)

    } finally {
    }
  }

}

sealed trait Table {
  // abstract type parameter to be implemented by Table classes
  type Record

  def name: String
  def createSQL: String

  def inserter(tbl: ISqlJetTable, data: Record): Unit

  // Inserting numbers into a ISqlJetTable requires them
  // being passed as Object, hence these methods:
  def &(x: Long) = x.asInstanceOf[AnyRef]
  def &(x: Double) = x.asInstanceOf[AnyRef]
}

/////////////////////////////////////////////////////////////////////////////
object TableStrata extends Table {
  val name = "Strata"

  val createSQL = """
      CREATE TABLE Strata (
      repId INT NOT NULL, 
      id INT NOT NULL, 
      level TEXT NOT NULL,
      PRIMARY KEY(repId, id))"""

  case class Rec(repId: Long, levelsLookup: Map[StratumLevel, Long])
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit = {
    for ((level, id) <- rec.levelsLookup) {
      tbl.insert(&(rec.repId), &(id), level.toString)
    }
  }
}

/////////////////////////////////////////////////////////////////////////////
object TableStratumResults extends Table {
  val name = "StratumResults"

  val createSQL = """
      CREATE TABLE StratumResults(
      repId INT NOT NULL,
      levelId INT NOT NULL,
      flameLength REAL,
      flameAngle REAL,
      flameHeight REAL,
      PRIMARY KEY(repId, levelId))"""

  case class Rec(repId: Long, levelId: Long, flameSummary: StratumFlameSummary)
  type Record = Rec

  def inserter(tbl: ISqlJetTable, rec: Rec): Unit =
    tbl.insert(
      &(rec.repId),
      &(rec.levelId),
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
        levelId INT NOT NULL,
        pathType TEXT NOT NULL,
        species TEXT NOT NULL,
        x0 REAL,
        y0 REAL,
        x1 REAL,
        y1 REAL,
        length REAL)"""

  case class Rec(repId: Long, runIndex: Long, levelId: Long, so: StratumOutcome)
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
            &(rec.levelId),
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

