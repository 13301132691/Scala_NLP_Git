package word;

/**
 * Created by Administrator on 2017/6/11.
 */
import com.hankcs.hanlp.collection.trie.bintrie.BinTrie;
import com.hankcs.hanlp.corpus.occurrence.PairFrequency;
import com.hankcs.hanlp.corpus.occurrence.TermFrequency;
import com.hankcs.hanlp.corpus.occurrence.TriaFrequency;
import com.hankcs.hanlp.dictionary.CoreDictionary;
import com.hankcs.hanlp.seg.common.Term;
import com.hankcs.hanlp.tokenizer.NotionalTokenizer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

public class Occurrence {
    public static final char RIGHT = '\u0000';
    static final char LEFT = '\u0001';
    double totalTerm;
    double totalPair;
    BinTrie<PairFrequency> triePair = new BinTrie();
    BinTrie<TermFrequency> trieSingle = new BinTrie();
    BinTrie<TriaFrequency> trieTria = new BinTrie();

    public Occurrence() {
        this.totalTerm = this.totalPair = 0.0D;
    }

    public void addPair(String first, String second) {
        this.addPair(first, '\u0000', second);
    }

    public void addTerm(String key) {
        TermFrequency value = (TermFrequency)this.trieSingle.get(key);
        if(value == null) {
            value = new TermFrequency(key);
            this.trieSingle.put(key, value);
        } else {
            value.increase();
        }

        ++this.totalTerm;
    }

    private void addPair(String first, char delimiter, String second) {
        String key = first + delimiter + second;
        PairFrequency value = (PairFrequency)this.triePair.get(key);
        if(value == null) {
            value = PairFrequency.create(first, delimiter, second);
            this.triePair.put(key, value);
        } else {
            value.increase();
        }

        ++this.totalPair;
    }

    public void addTria(String first, String second, String third) {
        String key = first + '\u0000' + second + '\u0000' + third;
        TriaFrequency value = (TriaFrequency)this.trieTria.get(key);
        if(value == null) {
            value = TriaFrequency.create(first, '\u0000', second, third);
            this.trieTria.put(key, value);
        } else {
            value.increase();
        }

        key = second + '\u0000' + third + '\u0001' + first;
        value = (TriaFrequency)this.trieTria.get(key);
        if(value == null) {
            value = TriaFrequency.create(second, third, '\u0001', first);
            this.trieTria.put(key, value);
        } else {
            value.increase();
        }

    }

    public void addAll(String[] termList) {
        String[] var2 = termList;
        int i = termList.length;

        int var4;
        for(var4 = 0; var4 < i; ++var4) {
            String term = var2[var4];
            this.addTerm(term);
        }

        String first = null;
        String[] var8 = termList;
        var4 = termList.length;

        for(int var9 = 0; var9 < var4; ++var9) {
            String current = var8[var9];
            if(first != null) {
                this.addPair(first, current);
            }

            first = current;
        }

        for(i = 2; i < termList.length; ++i) {
            this.addTria(termList[i - 2], termList[i - 1], termList[i]);
        }

    }

    public List<PairFrequency> getPhraseByScore(Set<Entry<String, PairFrequency>> entrySetPair) {
        List<PairFrequency> pairFrequencyList = new ArrayList(entrySetPair.size());
        Iterator var2 = entrySetPair.iterator();

        while(var2.hasNext()) {
            Entry<String, PairFrequency> entry = (Entry)var2.next();
            pairFrequencyList.add(entry.getValue());
        }

        Collections.sort(pairFrequencyList, new Comparator<PairFrequency>() {
            public int compare(PairFrequency o1, PairFrequency o2) {
                return -Double.compare(o1.score, o2.score);
            }
        });
        return pairFrequencyList;
    }

    public void addAll(List<Term> resultList) {
        String[] termList = new String[resultList.size()];
        int i = 0;

        for(Iterator var4 = resultList.iterator(); var4.hasNext(); ++i) {
            Term word = (Term)var4.next();
            termList[i] = word.word;
        }

        this.addAll(termList);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("二阶共现：\n");
        Iterator var2 = this.triePair.entrySet().iterator();

        Entry entry;
        while(var2.hasNext()) {
            entry = (Entry)var2.next();
            sb.append(entry.getValue()).append('\n');
        }

        sb.append("三阶共现：\n");
        var2 = this.trieTria.entrySet().iterator();

        while(var2.hasNext()) {
            entry = (Entry)var2.next();
            sb.append(entry.getValue()).append('\n');
        }

        return sb.toString();
    }

    public Double getTotalPair() {
        return this.totalPair;
    }

    public Double getTotalTerm() {
        return this.totalTerm;
    }

    public Set<Entry<String, TermFrequency>> getUniGram() {
        return this.trieSingle.entrySet();
    }

    public Set<Entry<String, PairFrequency>> getBiGram() {
        return this.triePair.entrySet();
    }

    public BinTrie<TriaFrequency> getTrieTria() {
        return this.trieTria;
    }
    public Set<Entry<String, TriaFrequency>> getTriGram() {
        return this.trieTria.entrySet();
    }
}
