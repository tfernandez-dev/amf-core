package amf.plugins.document.graph.parser

import amf.core.parser.errorhandler.AmfParserErrorHandler
import amf.core.parser.{EmptyFutureDeclarations, FutureDeclarations, ParsedReference, ParserContext}

import scala.collection.mutable

class GraphParserContext(rootContextDocument: String = "",
                         refs: Seq[ParsedReference] = Seq.empty,
                         futureDeclarations: FutureDeclarations = EmptyFutureDeclarations(),
                         eh: AmfParserErrorHandler,
                         val compactUris: mutable.Map[String, String] = mutable.Map(),
                         var baseId: Option[String] = None)
    extends ParserContext(rootContextDocument, refs, futureDeclarations, eh) {}
