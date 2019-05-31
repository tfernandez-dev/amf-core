package amf.plugins.document.graph.parser

import amf.core.AMFCompilerRunCount
import amf.core.parser.{EmptyFutureDeclarations, ErrorHandler, FutureDeclarations, ParsedReference, ParserContext}

import scala.collection.mutable

class GraphParserContext(rootContextDocument: String = "",
                         refs: Seq[ParsedReference] = Seq.empty,
                         futureDeclarations: FutureDeclarations = EmptyFutureDeclarations(),
                         parserCount: Int = AMFCompilerRunCount.nextRun(),
                         eh: Option[ErrorHandler] = None,
                         val compactUris: mutable.Map[String, String] = mutable.Map(),
                         var baseId: Option[String] = None
                        ) extends ParserContext(rootContextDocument, refs, futureDeclarations, parserCount, eh) {

}
