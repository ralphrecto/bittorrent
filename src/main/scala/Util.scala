import java.security.MessageDigest
import java.net.URLEncoder

/**
 * Created by ralphrecto on 1/17/16.
 */
object Util {
  private val MD5_JAVASEC_ALGO_NAME = "MD5"
  private val STD_CHARSET = "UTF-8"

  implicit class FunctionPipe[A](underlying : A) {
    def |>[B](f: A => B) : B = f(underlying)
  }

  // TODO: does this generate a 20 byte hash?
  def md5Digest(str: String): String = {
    val digest: Array[Byte] =
      MessageDigest.getInstance(MD5_JAVASEC_ALGO_NAME).digest(str.getBytes(STD_CHARSET))
    new String(digest, STD_CHARSET)
  }

  def urlEncode(str: String): String = {
    URLEncoder.encode(str, STD_CHARSET)
  }
}
