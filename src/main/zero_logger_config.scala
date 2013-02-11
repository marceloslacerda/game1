import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.formatters.SimpleFormatter
import scala.util.logging.ConsoleLogger

// Set global default logging level to Warning, and send logs to ConsoleLogger
object ZeroLoggerFactory {
  final def newLogger(singleton: Singleton) =
    (Filter.Warning, new SimpleFormatter(singleton) with ConsoleLogger)
}

package com.botequim.witchcraft {
  object ZeroLoggerFactory {
    // Set package com.yourDomain.yourProject's default logging level to Info
    final def newLogger(singleton: Singleton) =
      (Filter.Info, new SimpleFormatter(singleton) with ConsoleLogger)

    // Set Sample's logging level to Finest
    final def newLogger(singleton: Sample.type) =
      (Filter.Finest, new SimpleFormatter(singleton) with ConsoleLogger)
  }
}
