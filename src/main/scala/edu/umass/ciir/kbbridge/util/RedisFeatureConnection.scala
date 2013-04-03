package edu.umass.ciir.kbbridge.util

import redis.clients.jedis.Jedis
import scala.collection.JavaConversions._

/**
 *
 */
class RedisFeatureConnection(redisSvr:String,  redisPort:Int,  redisTimeout:Int, database:Int) {

    final def connectOnLocal(): Jedis = {
      var jedis: Jedis = new Jedis(redisSvr, redisPort, redisTimeout)
      System.out.println("Connecting to redis " + redisSvr + ":" + redisPort + " (timeout = " + redisTimeout + ")")
      jedis.connect
      jedis.select(database)
      return jedis
    }


    var jedisVar = connectOnLocal()
    def jedis:Jedis = {
      if(!jedisVar.isConnected){
        jedisVar = connectOnLocal()
      }
      jedisVar
    }

    def disconnect(){
      if(jedisVar.isConnected){
        jedisVar.disconnect()
      }
    }


}

class RedisConnectionPool(redisSvr:String,  redisPort:Int,  redisTimeout:Int, database:Int, poolSize:Int) {

  def createConnection():RedisFeatureConnection = {
    new RedisFeatureConnection(redisSvr, redisPort, redisTimeout, database)
  }

  val queue = new java.util.concurrent.LinkedBlockingQueue[RedisFeatureConnection](poolSize)
  for(_ <- 1 to poolSize){
    queue.add(createConnection())
  }


  def fetchRedis():RedisFeatureConnection = {
    queue.take()
  }

  def releaseRedis(conn:RedisFeatureConnection){
    queue.put(conn)
  }

  def disconnect() {
    for(conn <- queue.iterator()) conn.disconnect()
  }

  override def toString = "RedisConnectionPool "+redisSvr+":"+redisPort+" (db: "+database+")"
}
//   final val redisSrv= conf.getProperty("redisSvr", "localhost")
//    final val redisPort = Integer.parseInt(conf.getProperty("redisPort", "6379"))
//    final val redisTimeout = Integer.parseInt(conf.getProperty("redisTimeout", "999999"))
