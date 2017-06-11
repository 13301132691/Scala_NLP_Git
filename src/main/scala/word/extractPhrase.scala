/**
  * 本文借鉴了hanlp中的短语提取识别模块，将其中涉及到的互信息、左右熵等函数由java代码转化为scala代码
  **/
package word

import scala.util.control._
import java.util
import java.util.Set

import com.hankcs.hanlp.corpus.occurrence.{PairFrequency, TriaFrequency}
import com.hankcs.hanlp.dictionary.stopword.CoreStopWordDictionary
import com.hankcs.hanlp.seg.common.Term
import com.hankcs.hanlp.tokenizer.NotionalTokenizer
import com.hankcs.hanlp.dictionary.CoreDictionary

object extractPhrase extends App {

  //选取百分点官网的介绍作为分析文本
  val text: String = "百分点集团成立于2009年，是中国领先的大数据技术与应用服务商。百分点现有员工500+人，" +
    "包括2位国家千人计划入选者，30多位博士，和来自于国内外一流大学与技术公司的300多人的研发团队。" +
    "百分点首席科学家团队由多名国际顶尖的华人学者组成。\n\n七年来，百分点坚持自主创新，" +
    "沉淀了丰富和坚实的企业级大数据技术和应用实践，软件著作、专利与技术创新过百项。" +
    "百分点产品线已涵盖大数据技术层、管理层和应用层，其核心产品包括技术层的大数据操作系统（BD-OS），" +
    "管理层的用户画像标签管理系统，以及应用层的推荐引擎、分析引擎和营销引擎。作为企业级大数据技术与应用的践行者，" +
    "百分点一直专注于满足市场对海量异构数据的融合与应用需求，使企业能高效、便捷地进行数据资产管理和价值实现。\n\n" +
    "目前，百分点已为近2,000家互联网及实体企业提供大数据技术平台搭建和大数据驱动的SaaS应用，" +
    "客户涵盖制造、金融、汽车、零售、快消、电商、媒体、政府等行业的龙头企业，" +
    "如华为、TCL、长虹、建设银行、华夏银行、王府井百货、汤臣倍健、1号店、第一财经和中关村在线等。" +
    "同时已经与微软、华为、惠普等国际IT巨头开展战略合作，并共同为客户设计解决方案。\n\n" +
    "技术和应用是百分点的大核心竞争力，技术上，百分点拥有成熟的大数据技术与管理平台，高性能的实时与离线计算能力和丰富的算法库及商业模型；" +
    "应用上，百分点基于三大核心引擎的全业务驱动产品体系，帮助企业深入挖掘大数据的商业价值。" +
    "\n\n在融资方面，百分点已完成D轮融资，累计融资额10亿元人民币，投资方包括IDG、高瓴、光大证券、浙报传媒等，创企业级大数据领域融资最高记录。" +
    "在未来，百分点将进一步深入行业垂直化解决方案，并逐步推动企业向云端服务迁移。"
  val phraseList = extractPhrase(text, 20)
  println(phraseList)

  //主函数：用于提取文本中的关键短语
  //输入：分析文本和所需关键词个数
  def extractPhrase(text: String, size: Int): util.List[ String ] = {
    val phraseList: util.List[ String ] = new util.LinkedList()
    val occurrence: Occurrence = new Occurrence()
    val filterChain = CoreStopWordDictionary.FILTER
    var sentence_iter1: util.Iterator[ util.List[ Term ] ] = null
    var sentence_iter2: util.Iterator[ PairFrequency ] = null
    var sentence: util.List[ Term ] = null
    sentence_iter1 = NotionalTokenizer.seg2sentence(text, filterChain).iterator
    while (sentence_iter1.hasNext) {
      sentence = sentence_iter1.next
      occurrence.addAll(sentence)
    }
    compute(occurrence)
    var phrase: PairFrequency = null
    val loop = new Breaks
    sentence_iter2 = occurrence.getPhraseByScore(occurrence.getBiGram).iterator
    loop.breakable {
      while (sentence_iter2.hasNext) {
        phrase = sentence_iter2.next
        if (phraseList.size == size) loop.break()
        phraseList.add(phrase.first + phrase.second)
      }
    }
    phraseList
  }

  def compute(occur: Occurrence) {
    val entrySetPair: Set[ util.Map.Entry[ String, PairFrequency ] ] = occur.getBiGram
    var total_mi = 0.0
    var total_le = 0.0
    var total_re = 0.0

    val sentence_iter3: util.Iterator[ util.Map.Entry[ String, PairFrequency ] ] = entrySetPair.iterator
    var entry: util.Map.Entry[ String, PairFrequency ] = null
    var value: PairFrequency = null
    while (sentence_iter3.hasNext) {
      entry = sentence_iter3.next
      value = entry.getValue
      value.mi = computeMutualInformation(occur, value)
      value.le = computeLeftEntropy(occur, value)
      value.re = computeRightEntropy(occur, value)
      total_mi += value.mi
      total_le += value.le
      total_re += value.re
    }
  }

  def computeMutualInformation(occur: Occurrence, pair: PairFrequency): Double = {
    val p: Double = Math.max(1.0E-10, pair.getValue.intValue.toDouble / occur.getTotalPair)
    val p1: Double = Math.max(1.0E-10, CoreDictionary.getTermFrequency(pair.first).toDouble)
    val p2: Double = CoreDictionary.getTermFrequency(pair.second).toDouble
    Math.log(p / p1 / p2)
  }


  def computeEntropy(entrySet: util.Set[ util.Map.Entry[ String, TriaFrequency ] ]): Double = {
    var totalFrequency: Double = 0.0
    var entry: util.Map.Entry[ String, TriaFrequency ] = null
    var sentence_iter4: util.Iterator[ util.Map.Entry[ String, TriaFrequency ] ] = entrySet.iterator
    while (sentence_iter4.hasNext) {
      entry = sentence_iter4.next
      totalFrequency += entry.getValue.getValue.intValue.toDouble
    }
    var le: Double = 0.0
    var p: Double = 0.0
    sentence_iter4 = entrySet.iterator
    while (sentence_iter4.hasNext) {
      val entry = sentence_iter4.next
      p = entry.getValue.getValue.intValue.toDouble / totalFrequency
      le += -p * Math.log(p)
    }
    le
  }

  def computeLeftEntropy(occur: Occurrence, pair: PairFrequency): Double = {
    val entrySet = occur.getTrieTria.prefixSearch(pair.getKey + '\u0001')
    computeEntropy(entrySet)
  }

  def computeRightEntropy(occur: Occurrence, pair: PairFrequency): Double = {
    val entrySet = occur.getTrieTria.prefixSearch(pair.getKey + '\u0000')
    computeEntropy(entrySet)
  }
}
