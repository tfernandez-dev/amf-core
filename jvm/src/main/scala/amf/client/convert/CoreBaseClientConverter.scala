package amf.client.convert

import java.util
import java.util.Optional
import java.util.concurrent.CompletableFuture

import amf.client.reference.{ReferenceResolver => ClientReferenceResolver}
import amf.client.resource.{ResourceLoader => ClientResourceLoader}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.compat.java8.FutureConverters
import scala.compat.java8.OptionConverters._
import scala.concurrent.{ExecutionContext, Future}

trait CoreBaseClientConverter extends CoreBaseConverter {

  override type ClientOption[E] = util.Optional[E]
  override type ClientList[E]   = util.List[E]
  override type ClientMap[V]    = util.Map[String, V]

  override type ClientFuture[T] = CompletableFuture[T]

  override type ClientLoader    = ClientResourceLoader
  override type Loader          = ClientResourceLoader
  override type ClientReference = ClientReferenceResolver

  override protected def asClientOption[Internal, Client](
      from: Option[Internal],
      matcher: InternalClientMatcher[Internal, Client]): Optional[Client] =
    from.map(matcher.asClient).asJava

  override protected def asClientOptionWithEC[Internal, Client](from: Option[Internal],
                                                                matcher: InternalClientMatcherWithEC[Internal, Client])(
      implicit executionContext: ExecutionContext): ClientOption[Client] =
    from.map(matcher.asClient).asJava

  override private[convert] def asClientList[A, B](from: Seq[A], matcher: InternalClientMatcher[A, B]): util.List[B] =
    from.map(matcher.asClient).asJava

  override private[convert] def asClientListWithEC[Internal, Client](
      from: Seq[Internal],
      matcher: InternalClientMatcherWithEC[Internal, Client])(
      implicit executionContext: ExecutionContext): ClientList[Client] =
    from.map(matcher.asClient).asJava

  override protected def asClientMap[Internal, Client](
      from: mutable.Map[String, Internal],
      matcher: InternalClientMatcher[Internal, Client]): util.Map[String, Client] = {
    from.map { case (k, v) => k -> matcher.asClient(v) }.asJava
  }

  override protected def asClientImmutableMap[Internal, Client](
      from: Map[String, Internal],
      matcher: InternalClientMatcher[Internal, Client]): util.Map[String, Client] = {
    from.map { case (k, v) => k -> matcher.asClient(v) }.asJava
  }

  override protected def asClientLinkedMap[Internal, Client](
      from: mutable.LinkedHashMap[String, Internal],
      matcher: InternalClientMatcher[Internal, Client]): util.Map[String, Client] = {
    from.map { case (k, v) => k -> matcher.asClient(v) }.asJava
  }

  override protected def asInternalSeq[Client, Internal](
      from: util.List[Client],
      matcher: ClientInternalMatcher[Client, Internal]): mutable.Buffer[Internal] =
    from.asScala.map(matcher.asInternal)

  override protected def asInternalSeqWithEC[Client, Internal](from: util.List[Client],
                                                               matcher: ClientInternalMatcherWithEC[Client, Internal])(
      implicit executionContext: ExecutionContext): mutable.Buffer[Internal] = from.asScala.map(matcher.asInternal)

  override protected def asClientFuture[T](from: Future[T])(
      implicit executionContext: ExecutionContext): ClientFuture[T] =
    FutureConverters.toJava(from).toCompletableFuture

  override protected def asInternalFuture[Client, Internal](
      from: CompletableFuture[Client],
      matcher: ClientInternalMatcher[Client, Internal])(implicit executionContext: ExecutionContext): Future[Internal] =
    FutureConverters.toScala(from).map(matcher.asInternal)

  override protected def toScalaOption[E](from: Optional[E]): Option[E] = from.asScala

//  override protected def toScalaOptionWithEC[E](from: Optional[E])(
//      implicit executionContext: ExecutionContext): Option[E] = from.asScala

  override protected def toClientOption[E](from: Option[E]): ClientOption[E] = from.asJava

//  override protected def toClientOptionWithEC[E](from: Option[E])(
//      implicit executionContext: ExecutionContext): ClientOption[E] = from.asJava

  override protected def asInternalMap[Client, Internal](from: ClientMap[Client], m: ClientInternalMatcher[Client, Internal]): Map[String, Internal] = {
    from.asScala.toMap.foldLeft(Map[String,Internal]()) { case (acc, (e,i)) =>
      acc + (e -> m.asInternal(i))
    }
  }

}
