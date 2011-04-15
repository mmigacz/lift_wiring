package wiring {
package snippet {

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqWiringSupport
import xml.{Text, NodeSeq}
import net.liftweb.common.Box._
import net.liftweb.http.{S, WiringUI, SHtml}
import lib.MyWiringUI


class HelloWorld {

  val tags = "aa" :: "tag1" :: "tag2" :: "tag3" :: "tag4" :: "tag44" :: Nil

  val tagFilter = ValueCell[Box[String]](Full(""))
  val tagList = tagFilter.lift(_.map( p =>tags.filter( _.startsWith(p) )).getOrElse(Nil))
  val selectedTags = ValueCell[List[String]](Nil)


  def filter = {
    "#tag_filter" #> SHtml.ajaxText(tagFilter.openOr(""), v  => {tagFilter.set(Full(v)); println("tag filter = " + v + ", " + tagList.currentValue._1); Noop})
  }


  def tags(xhtml: NodeSeq): NodeSeq = WiringUI.toNode(xhtml, tagList, JqWiringSupport.fade)((tags, ns) => renderTags(tags, ns))


  def tags2(xhtml: NodeSeq): NodeSeq = {
    MyWiringUI.toNode(xhtml, tagList, JqWiringSupport.fade)((tags, ns) => renderTags(tags, ns))
  }


  def renderTags( tags: List[String], ns: NodeSeq ): NodeSeq = {
    ("li" #> tags.map( row =>
      ".tag" #> SHtml.a(() => addTag(row), Text(row))
    ))(ns)
  }



  def selected(xhtml: NodeSeq): NodeSeq = WiringUI.toNode(xhtml, selectedTags, JqWiringSupport.fade)(
    (tags, ns) => {
      ("li" #> tags.map( row =>
        ".tag" #> SHtml.a(() => removeTag(row), Text(row))
      ))(ns)
    }
  )

  def addTag(tag: String) = {
    if(!selectedTags.currentValue._1.contains(tag))
      selectedTags.atomicUpdate( list => list :+ tag  )
    Noop
  }

  def removeTag(tag: String) = {
    selectedTags.atomicUpdate( _.filterNot( _ == tag) )
    Noop
  }


}

}
}
