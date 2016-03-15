package db.web

import com.twitter.finagle.Http
import com.twitter.finagle.http.RequestBuilder
import com.twitter.util.Await
import db.web.OmatomeruParser.GaChaResult
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods

/**
  * Created by heejong.lee on 2/24/16.
  */
trait OmatomeruParser {
  def getGacha() : String = {
    val client = Http.newService("omatomeru.info:80")
    val request = RequestBuilder().url("http://omatomeru.info/pz-reagatya/kuji.php").addFormElement(("type","rare")).buildFormPost()
    val response = client(request)
    val result = Await.result(response)
    val rawString = result.getContentString()
    val json = JsonMethods.parse(rawString)

    implicit val format = DefaultFormats
    json.extract[GaChaResult].getString
  }
}

object OmatomeruParser {
  case class GaChaResult(id: String, name: String, rarity: Int, file: String) {
    def getPicUrl : String = {
      s"http://omatomeru.info/pz-reagatya/files/l-mns/$file"
    }
    def getString : String = {
      s"[No.$id $name ★${rarity+2}]($getPicUrl)"
    }
  }
}
