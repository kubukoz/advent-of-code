import scala.quoted._
import java.nio.file.Files
import java.nio.file.Paths

inline transparent def readFileNow(path: String): String = ${
  readFileNowImpl('path)
}

def readFileNowImpl(pathExpr: Expr[String])(using Quotes): Expr[String] =
  import quotes.reflect.*

  Expr(Files.readString(Paths.get(pathExpr.valueOrAbort)).strip())

inline def showType[A]: Unit = ${ showTypeImpl[A] }

def showTypeImpl[A: Type](using Quotes): Expr[Unit] = {
  import quotes.reflect._
  quotes.reflect.report.info(
    Type.show[A]
  )
  '{ () }
}
