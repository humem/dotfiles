# coding: utf-8

from __future__ import unicode_literals
from __future__ import print_function


import logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


def log_console():
    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    logger.addHandler(ch)



import unicodedata
import MeCab


# convert zenkaku to hankazu, lower characters
HANKAKU_LOWER = lambda text: unicodedata.normalize('NFKC', text).lower()

# select major nouns
MAJOR_NOUNS = lambda fs: fs[0] == '名詞' and fs[1] in {'一般', '固有名詞'}
# select content words
CONTENT_WORDS = lambda fs: fs[0] in {'名詞', '動詞', '形容詞', '副詞'}


def ja_analyzer(text, normalizer=HANKAKU_LOWER, selector=MAJOR_NOUNS):
    """Analyze Japanese text and select words."""
    words = []
    
    if normalizer is not None:
        text = normalizer(text)
    
    # tokenize and select words
    tagger = MeCab.Tagger()
    # text object must be assinged to a variable not to be deleted by garbage collection!
    # http://shogo82148.github.io/blog/2012/12/15/mecab-python/
    encoded = text.encode('utf-8')
    node = tagger.parseToNode(encoded).next
    while node.next:
        try:
            features = node.feature.decode('utf-8').split(',')
            if selector is None or selector(features):
                lemma = features[-3]
                if lemma == '*':
                    # do not forget to convert str to unicode!
                    # http://cocodrips.hateblo.jp/entry/2014/03/20/193240
                    lemma = node.surface.decode('utf-8')
                words.append(lemma)
        except Exception as e:
            logger.warning("Failed to analyze: %s", e)
            logger.warning("%s at %s", node.surface, text)
        node = node.next

    return words



import os, tarfile, glob, codecs, pickle, shutil
import re
from time import time

from sklearn import datasets
from sklearn import feature_extraction

import sys
if sys.version_info[0] == 3:
    from urllib.request import urlopen
else:
    from urllib2 import urlopen


CACHE_NAME = "ldcc_dataset.pkz"


def load_ldcc_dataset(data_home=None, analyzer=ja_analyzer):
    """Load the livedoor news corpus creative commons (LDCC) dataset including vectorized data."""
    data_home = datasets.base.get_data_home(data_home=data_home)
    cache_path = os.path.join(data_home, CACHE_NAME)
    ldcc_home = os.path.join(data_home, "ldcc")
    
    dataset = load_zipped_pickle(cache_path)
    if dataset is None:
        dataset = download_ldcc(working_dir=ldcc_home)
        # vectorize the data
        vectorizer = feature_extraction.text.TfidfVectorizer(analyzer=analyzer, min_df=2)
        logger.info("Analyzing documents with %s", vectorizer)
        t0 = time()
        X = vectorizer.fit_transform(dataset.data)
        logger.info("done in %0.3fs" % (time() - t0))
        # update the dataset and store it as a zipped pickle
        dataset.update(dict(vectorizer=vectorizer, X=X, n_features=X.shape[1]))
        save_zipped_pickle(cache_path, dataset)

    logger.info("LDCC dataset: n_samples=%d, n_classes=%d, n_features=%d",
                dataset.n_samples, dataset.n_classes, dataset.n_features)

    return dataset


LDCC_SITE = "http://www.rondhuit.com/download/"
LDCC_DISTFILE = "ldcc-20140209.tar.gz"
LDCC_URL = LDCC_SITE + LDCC_DISTFILE
TEXT_FOLDER = "text"


def download_ldcc(working_dir, cleanup=False):
    """Download the LDCC corpus and make a dataset."""
    
    if not os.path.exists(working_dir):
        os.makedirs(working_dir)

    archive_path = os.path.join(working_dir, LDCC_DISTFILE)
    if not os.path.exists(archive_path):
        logger.info("Downloading %s (9 MB)", LDCC_URL)
        opener = urlopen(LDCC_URL)
        open(archive_path, 'wb').write(opener.read())

    logger.info("Decompressing %s", archive_path)
    tarfile.open(archive_path, "r:gz").extractall(path=working_dir)
    # delete license files not to be loaded
    text_path = os.path.join(working_dir, TEXT_FOLDER)
    for path in glob.glob(text_path + '/*/LICENSE.txt'):
        os.remove(path)

    logger.info("Making a dataset from %s", text_path)
    dataset = datasets.base.load_files(text_path, encoding='utf-8', shuffle=False)
    # parse LDCC texts and extract contents
    data = zip(*[parse_ldcc(text) for text in dataset.data])
    dataset.update(dict(urls=data[0], dates=data[1], titles=data[2], data=data[3], footers=data[4]))
    dataset.update(dict(n_samples=len(dataset.data), n_classes=len(dataset.target_names)))

    if cleanup:
        shutil.rmtree(working_dir)

    return dataset
 
    
_FOOTER_SEPARATOR = re.compile('\n(■|【|\n\n\n)')


def parse_ldcc(text):
    """Parse LDCC text and extract URL, date, title, content, footer."""
    lines = text.split('\n')
    url, date, title, body = lines[0], lines[1], lines[2], '\n'.join(lines[3:])

    match = _FOOTER_SEPARATOR.search(body)
    pos = len(body) if match is None else match.start()

    return url, date, title, body[:pos].strip(), body[pos:].strip()
    

def save_zipped_pickle(path, obj):
    logger.info("Saving a zipped pickle %s", path)
    compressed = codecs.encode(pickle.dumps(obj), 'zlib_codec')
    open(path, 'wb').write(compressed)
    

def load_zipped_pickle(path):
    obj = None
    if os.path.exists(path):
        logger.info("Loading a zipped pickle %s", path)
        try:
            with open(path, 'rb') as f:
                compressed = f.read()
            uncompressed = codecs.decode(compressed, 'zlib_codec')
            obj = pickle.loads(uncompressed)
        except Exception as e:
            logger.warning("Cache loading failed %s", path)
            logger.warning(e)
            
    return obj
    


from sklearn import cluster
from sklearn import metrics
import numpy as np


def clustering(dataset):
    vectorizer = dataset.vectorizer
    X = dataset.X
    true_k = dataset.n_classes
    labels = dataset.target

    km = cluster.KMeans(n_clusters=true_k, max_iter=100, n_init=1)

    print("Clustering sparse data with %s" % km)
    t0 = time()
    km.fit(X)
    print("done in %0.3fs" % (time() - t0))
    print()

    print("Homogeneity: %0.3f" % metrics.homogeneity_score(labels, km.labels_))
    print("Completeness: %0.3f" % metrics.completeness_score(labels, km.labels_))
    print("V-measure: %0.3f" % metrics.v_measure_score(labels, km.labels_))
    print("Adjusted Rand-Index: %.3f"
          % metrics.adjusted_rand_score(labels, km.labels_))
    print("Silhouette Coefficient: %0.3f"
          % metrics.silhouette_score(X, labels, sample_size=1000))
    print()

    print("Top terms per cluster:")
    order_centroids = km.cluster_centers_.argsort()[:, ::-1]
    terms = vectorizer.get_feature_names()
    sizes = np.sum(km.labels_[:, np.newaxis] == np.arange(true_k), axis=0)
    for i in range(true_k):
        print("Cluster %d (%d):" % (i, sizes[i]), end='')
        for ind in order_centroids[i, :10]:
            print(' %s' % terms[ind], end='')
        print()



from sklearn import linear_model, svm, neighbors, naive_bayes
from sklearn import cross_validation
from sklearn import decomposition, pipeline, preprocessing
from sklearn import feature_selection


def classify_generator(dataset):
    X = dataset.X
    y = dataset.target

    while True:
        clf, X = (yield X, y)

        print()
        print("Cross validation with %s" % clf)
        t0 = time()
        scores = cross_validation.cross_val_score(clf, X, y, cv=5, n_jobs=1)
        print("done in %0.3fs" % (time() - t0))
        print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
    

def classify_each(dataset):
    generator = classify_generator(dataset)
    X, y = generator.next()

    for clf in (
            linear_model.LogisticRegression(),
            linear_model.PassiveAggressiveClassifier(),
            svm.LinearSVC(),
            neighbors.KNeighborsClassifier(),
            naive_bayes.MultinomialNB(),
            ):
        generator.send((clf, X))

    generator.close()

        
def classify_lsa(dataset, n_components=100, clf=svm.LinearSVC()):
    generator = classify_generator(dataset)
    X, y = generator.next()

    if n_components > 0:
        print("Performing dimensionality reduction using LSA: n_components=%d" %
              n_components)
        t0 = time()
        svd = decomposition.TruncatedSVD(n_components=n_components)
        lsa = pipeline.make_pipeline(svd, preprocessing.Normalizer(copy=False))
        X = lsa.fit_transform(X)
        print("done in %0.3fs" % (time() - t0))

        explained_variance = svd.explained_variance_ratio_.sum()
        print("Explained variance of the SVD step: {}%".format(
            int(explained_variance * 100)))

    generator.send((clf, X))
    generator.close()
    

def classify_chi2(dataset, k=1000, clf=svm.LinearSVC()):
    generator = classify_generator(dataset)
    X, y = generator.next()

    if k > 0:
        print("Performing univariate feature selection using chi2 test: k=%d" % k)
        t0 = time()
        selector = feature_selection.SelectKBest(feature_selection.chi2, k=k)
        X = selector.fit_transform(X, y)
        print("done in %0.3fs" % (time() - t0))

    generator.send((clf, X))
    generator.close()
