# coding: utf-8

from __future__ import print_function

import numpy as np
import re


class Searcher:
    def __init__(self, dataset):
        self.data = dataset.data
        self.label_names = dataset.target_names
        self.X = dataset.X
        self.y = dataset.y
        self.n_classes = len(dataset.target_names)
        self.c_ary = dataset.y[:, np.newaxis] == np.arange(self.n_classes)[np.newaxis, :]
        self.vocabulary = dataset.vectorizer.vocabulary_
        
    def words_by_char(self, char):
        words = []
        p = re.compile(char)
        for word in sorted(self.vocabulary):
            m = p.match(word)
            if m:
                words.append(word)
                self.word_info(word)

        print("%d words starting with '%s'" % (len(words), char))

        return words

    def word_info(self, word):
        wid = self.vocabulary[word]
        w_ary = (self.X[:, wid] > 0).toarray()
        n_c = np.sum(self.c_ary * w_ary, axis=0)
        n = np.sum(n_c)

        print("'%s' (%d) %d [%s]" %
              (word, wid, n, ', '.join([str(n) for n in n_c])))
        
        return w_ary

    def docs_by_word(self, word):
        return np.flatnonzero(self.word_info(word))

    def kwic(self, word, n_docs=10, n_snippets=5, span=40):
        wid = self.vocabulary[word]
        w_ary = (self.X[:, wid] > 0).toarray()
        nx = np.sum(w_ary)

        pw = re.compile(r'\b%s\b' % word, re.IGNORECASE)
        pn = re.compile(r'\s+')

        for i, x in enumerate(np.flatnonzero(w_ary)[:n_docs]):
            text = pn.sub(' ', self.data[x])
            name = self.label_names[self.y[x]]

            indexes = [m.start() for m in pw.finditer(text)]
            na = len(indexes)
            print("%d/%d: '%s' [%d] %d appearances" %
                  (i + 1, nx, name, len(text), na))

            for a, start in enumerate(indexes[:n_snippets]):
                idx0 = start - span
                idx1 = start + len(word) + span
                snippet = text[idx0:idx1]
                
                if idx0 > 0:
                    snippet = '..' + snippet

                if idx1 < len(text) - 1:
                    snippet += '..'

                print("%d/%d [%d]:\t%s" % (a + 1, na, start, snippet))

            print()

            

from collections import Counter
from itertools import islice, izip


def bigrams(dataset):
    c = Counter()
    for text in dataset.data:
        words = re.findall(r'\w+', text)
        c += Counter(izip(words, islice(words, 1, None)))

    print(c.most_common(10))
