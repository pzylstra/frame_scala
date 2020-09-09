package ffm.runner

import ffm.fire._
import ffm.forest._
import ffm.io.r.Database


/**
 * Provides methods to run the model.
 */
object Runner {
  /**
   * Runs the model for the given site and returns a result object.
   */
  def run(site: Site): FireModelResult = {
    // run the fire model
    val pathModel = new DefaultIgnitionPathModel
    val plantFlameModel = DefaultPlantFlameModel
    val windModel = DefaultVegetationWindModel

    DefaultSingleSiteFireModelRunner.run(pathModel, plantFlameModel, windModel)(site)    
  }
  
  /** 
   * Runs the model and writes the results to a new database.
   * 
   * If there is an existing file with the same path and filename, and
   * `deleteIfExists` is `true`, the file will be deleted. If `deleteIfExists` 
   * is false or an attempt to delete the existing file fails, an error is
   * thrown.
   * 
   * @param site a model site object.
   * 
   * @param dbpath path and filename for the new output database.
   * 
   * @param deleteIfExists whether to delete an existing file
   * 
   * @param useTransactions whether to use transactions for all current and
   *   subsequent insertions into the created database.
   *   
   * @return a new [[Database]] object.
   */
  def run(site: Site, dbpath: String, deleteIfExists: Boolean, useTransactions: Boolean): Database = {
    Database.create(dbpath, deleteIfExists, useTransactions) match {
      case Some(db) => 
        db.insertResult( run(site) )
        db
        
      case None =>
        throw new RuntimeException("Could not create database")
    }
  }
  
  /**
   * Runs the model and writes results to the database provided.
   * 
   * An error results if the database is not open and writable.
   * 
   * @param site a model site object.
   * @param db the output database.
   * 
   * @return the output database.
   */
  def run(site: Site, db: Database): Unit = {
    require(db.isOpen(), "The database is not open")
 
    db.insertResult( run(site) )
  }
}

