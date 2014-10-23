# coding: utf-8

import numpy as np
from itertools import chain, repeat
from sklearn import datasets, cluster


class Numberer:
    def __init__(self):
            self.dict = {}

    def hash(self, value):
            if value in self.dict:
                    return self.dict[value]
            else:
                    key = len(self.dict)
                    self.dict[value] = key
                    return key
 

class Target:
    def __init__(self, dataset):
        self.dataset = dataset
        # identical tags are indexed by tagid, and each idendical tag is an array of revisioned tags.
        # each revisioned tag represents its content, a dict of group, name, log and others,
        # and it is referred by a tag, a tuple of tagid and revision.
        self.tags = []
        # each snapshot is an array of bundles, and each bundle is an array of tags.
        self.snapshots = []
        # the cursor indicates the current snapshot in the snapshots array.
        self.cursor = -1
        
        ntags = np.unique(dataset.target).shape[0]
        bundle = [self.tagger((dataset.target == i).nonzero()[0],
                              name=dataset.target_names[i]) for i in xrange(ntags)]
        log = 'import ' + self.repr_dataset()
        self.set_snapshot([bundle], log=log)

    def __repr__(self):
        return '\n'.join((
            "'dataset': <%s>" % self.repr_dataset(),
            "'tags': <%d tags>" % len(self.tags),
            "'snapshots': <%d snapshots>" % len(self.snapshots),
            "'cursor': %d" % self.cursor))

    def repr_dataset(self):
        return "'%s', %d samples, %d categories" % (self.dataset.description,
                                                    len(self.dataset.data),
                                                    len(self.dataset.target_names))

    def repr_tag(self, tag):
        tagid, revision = tag
        if revision < 0:
            revision += len(self.tags[tagid])
            
        latest = revision == len(self.tags[tagid]) - 1
        tagged = self.tagged(tag)
        log = tagged['log'] if 'log' in tagged else None

        return "<%s%s%s>" % (
            self.tagged_summary(tag),
            ', latest rev' if latest else '',
            ", '%s'" % log if log else '')
        
    def repr_bundle(self, **kwargs):
        bundle = self.bundle(**kwargs)

        return "<%d tags:\n %s>" % (
            len(bundle),
            ',\n '.join(["%d. %s" % (index, self.tagged_summary(tag))
                         for index, tag in enumerate(bundle)]))

    def repr_snapshot(self, cursor=None):
        if cursor is None:
            cursor = self.cursor

        snapshot = self.snapshots[cursor]
        n_bundles = len(snapshot['bundles'])
        
        return "<%d bundles at cursor %d: %s\n %s>" % (
            n_bundles,
            cursor,
            "'%s'" % snapshot['log'] if 'log' in snapshot else '',
            ',\n '.join([self.repr_bundle(cursor=cursor, order=order)
                         for order in xrange(n_bundles)]))

    def tagger(self, group, **kwargs):
        self.tags.append([dict(group=group, **kwargs)])
        
        return len(self.tags) - 1, 0
        
    def revise(self, tag, group, **kwargs):
        tagid = tag[0]
        self.tags[tagid].append(dict(group=group, **kwargs))

        return tagid, len(self.tags[tagid]) - 1

    def tagged(self, tag):
        tagid, revision = tag

        return self.tags[tagid][revision]
               
    def tagged_group(self, tag):
        return self.tagged(tag)['group']

    def tagged_size(self, tag):
        return len(self.tagged_group(tag))

    def tagged_name(self, tag):
        name = None
        tagid, revision = tag
        if revision < 0:
            revision += len(self.tags[tagid])

        for rev in reversed(xrange(revision + 1)):
            tagged = self.tagged((tagid, rev))
            if 'name' in tagged:
                name = tagged['name']
                break
        
        return name

    def tagged_summary(self, tag):
        return "'%s' (%d, %d) [%d]" % (
            self.tagged_name(tag), tag[0], tag[1], self.tagged_size(tag))

    def set_snapshot(self, bundles, **kwargs):
        ssid = len(self.snapshots)
        self.snapshots.append(dict(bundles=bundles, **kwargs))
        self.cursor = ssid
        
        return ssid

    def bundle(self, cursor=None, order=0):
        if cursor is None:
            cursor = self.cursor

        return self.snapshots[cursor]['bundles'][order]

    def bundle_dataset(self, **kwargs):
        bundle = self.bundle(**kwargs)

        return datasets.base.Bunch(X=self._X(bundle),
                                   y=self._y(bundle),
                                   names=self._names(bundle),
                                   tags=bundle)

    def _body(self, bundle):
        return np.concatenate([self.tagged_group(tag) for tag in bundle])

    def _X(self, bundle):
        return self.dataset.X[self._body(bundle)]

    def _y(self, bundle):
        counts = zip(xrange(len(bundle)),
                     [self.tagged_size(tag) for tag in bundle])

        return np.array(list(chain.from_iterable([repeat(*t) for t in counts])))
    
    def _names(self, bundle):
        return [self.tagged_name(tag) for tag in bundle]
        
    def _action_log(self, action, indexes, **kwargs):
        bundle = self.bundle(**kwargs)
        tags = [bundle[i] for i in indexes]

        return ('*%s ' % action) + ' , '.join([self.tagged_summary(tag) for tag in tags])
        
    def extract(self, indexes, **kwargs):
        bundle = self.bundle(**kwargs)

        return self.set_snapshot(
            [[bundle[i] for i in indexes],
             [bundle[j] for j in xrange(len(bundle)) if j not in indexes]],
            log=self._action_log('extract', indexes, **kwargs))
        
    def join(self, indexes, **kwargs):
        bundle = self.bundle(**kwargs)
        tag = bundle[indexes[0]]
        body = self._body([bundle[i] for i in indexes])
        log = self._action_log('join', indexes, **kwargs)

        revtag = self.revise(tag, body, log=log)

        newbundle = []
        for i in xrange(len(bundle)):
            if i not in indexes:
                newbundle.append(bundle[i])
            elif i == indexes[0]:
                newbundle.append(revtag)
                
        return self.set_snapshot([newbundle], log=log)

    def split(self, index, splitter=None, **kwargs):
        if splitter is None:
            splitter = cluster.KMeans()

        bundle = self.bundle(**kwargs)
        tag = bundle[index]
        name = self.tagged_name(tag)
        X = self.dataset.X[self.tagged_group(tag)]
        log = self._action_log('split', [index], **kwargs) + ' with \n' + repr(splitter)

        splitter.fit(X)
        splitted = [self.tagger((splitter.labels_ == i).nonzero()[0],
                                name="%s-%d" % (name, i),
                                log=log + '-%d' % i) for i in xrange(splitter.n_clusters)]
        newbundle = []
        for i in xrange(len(bundle)):
            if i != index:
                newbundle.append(bundle[i])
            else:
                newbundle.extend(splitted)

        return self.set_snapshot([newbundle], log=log)
