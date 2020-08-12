import pretty_midi, argparse
from places import tonic, positions, notenumbers, highest
from graphviz import Digraph
from operator import attrgetter


def findtempo(midifilename, minimumintempo=20.0):
    min_in_tempo = minimumintempo
    raw_tempo = pretty_midi.PrettyMIDI(midifilename).get_tempo_changes()[-1][-1]
    if not raw_tempo:
        raw_tempo = 120
    in_tempo = max(min_in_tempo, raw_tempo)
    secsperthirty = 15.0 / in_tempo
    return raw_tempo, secsperthirty


def secsfromthirty(seconds, ratio):
    return int(seconds / ratio + 0.5) + 1


class Note(object):
    restLength = 0.015

    def thirtytwo_to_seconds(self, thirtytwo):
        return (thirtytwo - 1) * self.secsperthirty

    def seconds_to_thirtytwo(self, seconds):
        return secsfromthirty(seconds, self.secsperthirty)

    def __init__(self, thirtytwo, pitch, secsperthirty):
        self.thirtytwo = thirtytwo
        self.pitch = pitch
        self.secsperthirty = secsperthirty
        self.reset()

    def reset(self):
        self.unmatched = True
        self.unplaced = True
        self.end = None

    def _getnotenumber(self):
        return notenumbers[self.pitch]
    notenumber = property(_getnotenumber)

    def findend(self, noteList):
        leastDiff = 100000000
        foundFlag = False
        distance = self._findDistance(foundFlag, leastDiff, noteList)
        self.end = self.start + distance - Note.restLength

    def _start(self):
        return self.thirtytwo_to_seconds(self.thirtytwo)
    start = property(_start)

    def _findDistance(self, foundFlag, leastDiff, noteList, endstop=33):
        for othernote in noteList:
            foundFlag, leastDiff = self._updateLeastDistance(foundFlag, leastDiff, othernote)
        if foundFlag:
            return leastDiff * self.secsperthirty
        return (endstop - self.thirtytwo) * self.secsperthirty

    def _updateLeastDistance(self, foundFlag, leastDiff, othernote):
        if othernote is not self:
            if othernote.thirtytwo > self.thirtytwo:
                diff = othernote.thirtytwo - self.thirtytwo
                if diff < leastDiff:
                    leastDiff = diff
                    foundFlag = True
        return foundFlag, leastDiff

    def __str__(self):
        return " ".join([self.pitch, str(self.thirtytwo)])


class Riff(list):
    stops = [1, 5, 9, 13, 17, 21, 25, 29, 3, 7, 11, 15, 19, 23, 27, 31,
             2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32]
    thirttostop = dict([(stop, n) for n, stop in enumerate(stops)])

    def __init__(self, *arguments, **keywordarguemnts):
        list.__init__(self, *arguments, **keywordarguemnts)
        self.empty = [True] * len(Riff.stops)

    def checkforspace(self, newmember):
        for member in self:
            if member.thirtytwo == newmember.thirtytwo:
                if member.pitch == newmember.pitch:
                    return True
                return False
        return True

    def append(self, newmember):
        clear = True
        for member in self:
            if member.thirtytwo == newmember.thirtytwo:
                clear = False
                if member.pitch != newmember.pitch:
                    for n, stop in enumerate(Riff.stops):
                        if self.empty[n]:
                            self._slideMember(n, newmember, stop)
                            break

        if clear:
            self._addMember(newmember)

    def _addMember(self, newmember):
        list.append(self, newmember)
        self.empty[Riff.thirttostop[newmember.thirtytwo]] = False

    def _slideMember(self, n, newmember, stop):
        newmember.thirtytwo = stop
        self.empty[n] = False
        list.append(self, newmember)


class RuleArgument(object):
    def __init__(self, mark):
        self.mark = mark

    def __str__(self):
        return str(self.mark)


class TimeArgument(RuleArgument):
    def _getthirtytwo(self):
        return self.mark
    def _setthirtytwo(self, newValue):
        self.mark = newValue
    thirtytwo = property(_getthirtytwo, _setthirtytwo)


class PitchArgument(RuleArgument):
    def _getpitch(self):
        return self.mark
    def _setpitch(self, newValue):
        self.mark = newValue
    pitch = property(_getpitch, _setpitch)


class Source(RuleArgument):
    def __init__(self, mark, qualifier=None):
        RuleArgument.__init__(self, mark)
        self.qualifier = qualifier

    def __str__(self):
        if self.qualifier is None:
            return str(self.mark)
        return f"[{self.qualifier}] {self.mark}"


class TimeSource(Source, TimeArgument):
    def __init__(self, mark, qualifier=None):
        Source.__init__(self, mark, qualifier)


class PitchSource(Source, PitchArgument):
    def __init__(self, mark, qualifier=None):
        Source.__init__(self, mark, qualifier)


class Target(RuleArgument):
    def __str__(self):
        return str(self.mark)


class TimeTarget(Target, TimeArgument):
    pass


class PitchTarget(Target, PitchArgument):
    pass


class Rule(object):
    lastPrecedence = 0
    rank = 0
    orderspan = -10000
    qualifierspan = -1000

    def __init__(self, source, target, precedence=None, banner=None):
        self.source = source
        self.target = target
        self.banner = banner
        self.reset(precedence, source, target)

    def reset(self, precedence, source, target):
        self._setRuleArgument(source, 'source', TimeSource, PitchSource, Source)
        self._setRuleArgument(target, 'target', TimeTarget, PitchTarget, Target)
        self._setPrecedence(precedence)
        self.results = []

    @staticmethod
    def makelist(elementtype, value):
        if isinstance(value, elementtype):
            return [value]
        return value

    def __str__(self):
        return ", ".join([str(self.source), str(self.target)])

    def __lt__(self, other):
        return self._comparison(other, int.__lt__, int.__lt__)

    def __gt__(self, other):
        return self._comparison(other, int.__gt__, int.__gt__)

    def __le__(self, other):
        return self._comparison(other, int.__lt__, int.__le__)

    def __ge__(self, other):
        return self._comparison(other, int.__gt__, int.__ge__)

    def __eq__(self, other):
        return (self.__class__.rank == other.__class__.rank) and (self.precedence == other.precedence)

    def brace(self, qualifierType=str, markType=int):
        markList = Rule.makelist(markType, self.source.mark)
        if self.source.qualifier is None:
            qualList = None
        else:
            qualList = Rule.makelist(qualifierType, self.source.qualifier)
        return False, True, qualList, markList

    def drawNotes(self, match, matchedNotes, noteList, qualList, thirt):
        for note in noteList:
            if note.unmatched:
                if note.thirtytwo == thirt:
                    if self.source.qualifier is None:
                        match = True
                        matchedNotes.append(note)
                        break
                    else:
                        if note.pitch in qualList:
                            match = True
                            matchedNotes.append(note)
                            break
        return match

    def addNotes(self, applied, match, matchedNotes, trail=None, flag='unmatched',):
        if match:
            applied = True
            if self.banner:
                print(self.banner)
            if trail is None:
                self.results.append((self.target, matchedNotes), )
            else:
                self.results.append((trail, self.target, matchedNotes), )
            [setattr(note, flag, False) for note in matchedNotes]
        return applied

    def _setRuleArgument(self, provided, attribute, timetype, pitchtype, intendedtype):
        if isinstance(provided, intendedtype):
            setattr(self, attribute, provided)
        elif isinstance(provided, int):
            setattr(self, attribute, timetype(provided))
        else:
            setattr(self, attribute, pitchtype(provided))

    def _setPrecedence(self, precedence):
        if precedence is None:
            self.precedence = Rule.lastPrecedence + 1
            Rule.lastPrecedence += 1
        else:
            self.precedence = precedence
            Rule.lastPrecedence = max(self.precedence, Rule.lastPrecedence + 1)

    def _getorder(self):
        if isinstance(self.source.mark, set) or isinstance(self.source.mark, list):
            result = self.__class__.orderspan * len(self.source.mark) + self.precedence
            if isinstance(self.source.qualifier, set) or isinstance(self.source.qualifier, list):
                result += self.__class__.qualifierspan * len(self.source.qualifier)
            return result
        return self.precedence
    order = property(_getorder)

    def _comparison(self, other, disjunct, conjunct):
        return disjunct(self.__class__.rank, other.__class__.rank) or \
               (int.__eq__(self.__class__.rank, other.__class__.rank) and
                conjunct(self.order, other.order))


class TimeToPitch(Rule):
    rank = 1000


class PitchToTime(Rule):
    rank = 2000
    orderspan = 10000
    qualifierspan = 1000


class White(TimeToPitch):
    Rank = 1500

    def __init__(self, source, target, precedence=None):
        TimeToPitch.__init__(self, source, target, precedence)

    def apply(self, noteList):
        applied, match, qualList, thirtList = self.brace()
        while match:
            matchedNotes = []
            for thirt in thirtList:
                match = self.drawNotes(False, matchedNotes, noteList, qualList, thirt)
                if not match:
                    break
            applied = self.addNotes(applied, match, matchedNotes)
        return applied


class Black(PitchToTime):
    Rank = 2500

    def __init__(self, source, target, precedence=None, banner=None):
        PitchToTime.__init__(self, source, target, precedence, banner)

    def apply(self, halfList):
        applied, match, qualSet, pitchList = self.brace(int, str)
        self.results = []
        numpitches = len(pitchList)
        for pulltarget, notelist in halfList:
            applied = self._unhalf(applied, notelist, numpitches, pitchList, pulltarget, qualSet)
        return applied

    def _unhalf(self, applied, notelist, numpitches, pitchList, pulltarget, qualSet):
        if len(notelist) >= numpitches:
            placednotes = []
            match = self._placeAll(False, notelist, pitchList, placednotes, qualSet)
            applied = self.addNotes(applied, match, placednotes, pulltarget, 'unplaced')
        return applied

    def _placeAll(self, match, notelist, pitchList, placednotes, qualSet):
        for note in notelist:
            if note.unplaced:
                match = True
                if note.pitch in pitchList:
                    placednotes.append(note)
                    if self.source.qualifier is not None:
                        if note.thirtytwo not in qualSet:
                            match = False
                            break
                else:
                    match = False
                    break
        return match


class RuleCollection(object):
    def __init__(self, inmidifilename, pull, push):
        self.pull = pull
        self.push = push
        self.banners = []
        self.raw_tempo, self.secsperthirty = findtempo(inmidifilename)

    def inhale(self, drawgraph):
        self.pull.sort()
        return sum([self._applyRule(drawgraph, rule, self.inriff) for rule in self.pull], []), \
               list(filter(lambda x: x.unmatched, self.inriff))

    def exhale(self, halves, drawgraph):
        self.push.sort()
        return sum([self._applyRule(drawgraph, rule, halves) for rule in self.push], []), \
               list(filter(lambda x: x.unplaced and not x.unmatched, self.inriff))

    def go(self, inriff, drawGraph=False, zigzaglimit=6, zigzagtarget=7, echomargin=9, maxnotes=17, minnotes=9):
        inhilation, inremains, outnotes, outremains, result, slidelist = self._assemble(drawGraph, inriff)
        [self._writenotes(result, rhythm, slidelist, targ) for targ, rhythm, nl in outnotes]
        if len(result) < minnotes:
            [result.append(slider) for slider in slidelist]
        notesequence = self._improve(result, zigzaglimit, zigzagtarget, 'countZigZags')
        self.notedict = dict([(note.thirtytwo, note) for note in notesequence])
        notesequence = self._improve(result, echomargin, echomargin, 'countEch')
        self._fixLength(maxnotes, notesequence, result)
        [note.findend(result) for note in result]
        return result, inhilation, outnotes, inremains, outremains

    def _fixLength(self, maxnotes, notesequence, result):
        if (1 not in self.notedict) and (3 not in self.notedict):
            result.append(Note(1, 'I', self.secsperthirty))
            result.append(Note(3, 'I', self.secsperthirty))
        if len(notesequence) > maxnotes:
            for note in notesequence:
                if (note.thirtytwo % 2) == 0:
                    result.remove(note)

    def countEch(self, notesequence):
        echocount = 0
        for note in notesequence:
            if note.thirtytwo > 16:
                break
            echo = note.thirtytwo + 16
            if echo in self.notedict:
                if note.pitch == self.notedict[echo].pitch:
                    echocount += 1
        return len(notesequence) - echocount

    def _improve(self, result, featurelimit, featuretarget, method):
        result = sorted(result, key=attrgetter('thirtytwo'))
        featurecount = getattr(self, method)(result)
        if featurecount > featurelimit:
            improvement = True
            newcount = featurecount
            lastcount = featurecount
            self._searchForImprovement(featuretarget, improvement, lastcount, method, newcount, result)
        return result

    def _searchForImprovement(self, featuretarget, improvement, lastcount, method, newcount, result):
        while improvement:
            improvement = False
            for n, a in enumerate(result):
                for b in result[n + 1:]:
                    if a.pitch != b.pitch:
                        RuleCollection.swapPitches(a, b)
                        newcount = getattr(self, method)(result)
                        if newcount < lastcount:
                            improvement = True
                            break
                        RuleCollection.swapPitches(a, b)
                if newcount <= featuretarget:
                    break
            if newcount <= featuretarget:
                break
            lastcount = newcount

    @staticmethod
    def swapPitches(a, b):
        apitch = a.pitch
        a.pitch = b.pitch
        b.pitch = apitch

    def countZigZags(self, sequence):
        lastp = sequence[0].notenumber
        lastdir = 0
        result = 0
        for note in sequence[1:]:
            if note.notenumber > lastp:
                if lastdir == -1:
                    result += 1
                lastdir = 1
            elif note.notenumber < lastp:
                if lastdir == 1:
                    result += 1
                lastdir = -1
            lastp = note.notenumber
        return result

    def fullGraph(self, title='all rules'):
        dot = RuleCollection._startGraph(title)
        for box in (self.pull, self.push):
            [RuleCollection._addRuleToGraph(dot, rule) for rule in box]
        dot.render('allrules.gv', view=True)

    @staticmethod
    def _startGraph(title):
        dot = Digraph(comment=title, format='PNG', engine='neato')
        dot.attr(overlap='scale')
        dot.attr(splines='true')
        return dot

    @staticmethod
    def _addRuleToGraph(dot, rule):
        rulemark = str(rule.source.mark)
        targetmark = str(rule.target.mark)
        dot.node(rulemark, rulemark)
        if rule.source.qualifier is None:
            dot.edge(rulemark, targetmark)
        else:
            dot.edge(rulemark, targetmark, label=str(rule.source.qualifier))

    def _assemble(self, drawGraph, inriff):
        self.inriff = inriff
        inhilation, inremains, outnotes, outremains, result = self._findRiff(drawGraph)
        self._writeGraph(drawGraph)
        return inhilation, inremains, outnotes, outremains, result, []

    def _findRiff(self, drawGraph):
        result = Riff()
        if drawGraph:
            self.banners = []
        inhilation, inremains = self.inhale(drawGraph)
        outnotes, outremains = self.exhale(inhilation, drawGraph)
        return inhilation, inremains, outnotes, outremains, result

    def _writeGraph(self, drawGraph):
        if drawGraph:
            dot = RuleCollection._startGraph('rules applied')
            for rule in self.banners:
                RuleCollection._addRuleToGraph(dot, rule)
            dot.render('transformations.gv')

    def _applyRule(self, drawgraph, rule, noteBatch):
        if drawgraph:
            if rule.apply(noteBatch):
                self.banners.append(rule)
        else:
            rule.apply(noteBatch)
        return  rule.results

    def _writenotes(self, result, rhythm, slidelist, targ):
        pulltar = targ.pitch
        tme = rhythm.thirtytwo
        if isinstance(pulltar, str):
            self._singleTarget(pulltar, result, slidelist, tme)
        else:
            self._manyTargets(pulltar, result, slidelist, tme)

    def _manyTargets(self, pulltar, result, slidelist, tme):
        if isinstance(tme, int):
            RuleCollection._assign(Note(tme, pulltar[0], self.secsperthirty), result, slidelist)
        else:
            self._manyTimes(pulltar, result, slidelist, tme)

    def _manyTimes(self, pulltar, result, slidelist, tme):
        tarlength = len(pulltar)
        timelist = list(tme)
        timelist.sort()
        [RuleCollection._assign(Note(thirt, pulltar[n % tarlength], self.secsperthirty), result, slidelist)
            for n, thirt in enumerate(timelist)]

    def _singleTarget(self, pulltar, result, slidelist, tme):
        if isinstance(tme, int):
            RuleCollection._assign(Note(tme, pulltar, self.secsperthirty), result, slidelist)
        else:
            for thirt in tme:
                RuleCollection._assign(Note(thirt, pulltar, self.secsperthirty), result, slidelist)

    @staticmethod
    def _assign(newNote, result, slidelist):
        if result.checkforspace(newNote):
            result.append(newNote)
        else:
            slidelist.append(newNote)


def collection(infile):
    return RuleCollection(infile, [
        White(TimeSource(1), PitchTarget("I")),
        White(TimeSource({21, 23}), PitchTarget("I")),
        White(TimeSource({6, 22}), PitchTarget("I")),
        White(TimeSource({7, 23}, 'bV'), PitchTarget("I")),
        White(TimeSource({7, 23}, 'bII'), PitchTarget("V")),
        White(TimeSource({7, 23}, 'bVI'), PitchTarget("II")),
        White(TimeSource({7, 23}, 'bV-'), PitchTarget("I+")),
        White(TimeSource(23, 'bIII'), PitchTarget("I")),
        White(TimeSource(23, 'VII'), PitchTarget("V")),
        White(TimeSource(23, 'bV'), PitchTarget("II")),
        White(TimeSource(23, 'bIII+'), PitchTarget("I+")),
        White(TimeSource({2, 25}, ['bIII', 'I']), PitchTarget("I")),
        White(TimeSource(11, 'I'), PitchTarget("I")),
        White(TimeSource(11, 'V'), PitchTarget("V")),
        White(TimeSource(11, 'II'), PitchTarget("II")),
        White(TimeSource(11, 'VI-'), PitchTarget("VI")),
        White(TimeSource(11, 'I+'), PitchTarget("I+")),
        White(TimeSource(11, 'II+'), PitchTarget("I")),
        White(TimeSource(17, "I"), PitchTarget("I")),
        White(TimeSource(17, "V"), PitchTarget("V")),
        White(TimeSource(17, "II"), PitchTarget("II")),
        White(TimeSource(17, "VI"), PitchTarget("VI")),
        White(TimeSource(17, "III"), PitchTarget("III")),
        White(TimeSource(17, "VII"), PitchTarget("VII")),
        White(TimeSource(17, "bV"), PitchTarget("bV")),
        White(TimeSource(17, "I+"), PitchTarget("I+")),
        White(TimeSource(5, 'III+'), PitchTarget("III")),
        White(TimeSource(5, 'VII'), PitchTarget("VII")),
        White(TimeSource(7, 'III+'), PitchTarget("bVII")),
        White(TimeSource(7, 'VII'), PitchTarget("IV")),
        White(TimeSource(5, 'III'), PitchTarget("III-")),
        White(TimeSource(7, 'III'), PitchTarget("bVII-")),
        White(TimeSource(5, 'III-'), PitchTarget("III-")),
        White(TimeSource(7, 'III-'), PitchTarget("bVII-")),
        White(TimeSource({15, 31}), PitchTarget("bIII")),
        White(TimeSource(15, 'bIII'), PitchTarget("bVII")),
        White(TimeSource(15, 'VII'), PitchTarget("bV")),
        White(TimeSource(15, 'bV'), PitchTarget("bII")),
        White(TimeSource(15, 'bIII-'), PitchTarget("bVII-")),
        White(TimeSource({29, 31}), PitchTarget("I")),
        White(TimeSource({19, 23, 25}, 'V'), PitchTarget("I")),
        White(TimeSource({19, 23, 25}, 'II'), PitchTarget("V")),
        White(TimeSource({19, 23, 25}, 'VI'), PitchTarget("II")),
        White(TimeSource({19, 23, 25}, 'V-'), PitchTarget("I-")),
        White(TimeSource({19, 27}, 'I'), PitchTarget("II")),
        White(TimeSource({19, 27}, 'V'), PitchTarget("VI")),
        White(TimeSource({19, 27}, 'II'), PitchTarget("III")),
        White(TimeSource({19, 27}, 'VI'), PitchTarget("VII")),
        White(TimeSource({19, 27}, 'I-'), PitchTarget("II-")),
        White(TimeSource({19, 27}, 'I+'), PitchTarget("II+")),
        White(TimeSource({5, 25}, ['bIII', 'IV']), PitchTarget("II")),
        White(TimeSource(9), PitchTarget("bIII")),
        White(TimeSource({9, 25}), PitchTarget("I")),
        White(TimeSource(5, 'II'), PitchTarget("bIII")),
        White(TimeSource(5, 'VI'), PitchTarget("bVII")),
        White(TimeSource(5, 'I+'), PitchTarget("IV")),
        White(TimeSource(5, 'V'), PitchTarget("I+")),
        White(TimeSource(5, 'II-'), PitchTarget("bIII-")),
        White(TimeSource(5, 'II+'), PitchTarget("bIII+")),
        White(TimeSource(13), PitchTarget("bIII")),
        White(TimeSource(27), PitchTarget("bIII")),
        White(TimeSource({11, 27}, 'II+'), PitchTarget("bIII")),
        White(TimeSource({11, 27}, 'II'), PitchTarget("bIII-")),
        White(TimeSource({11, 27}, 'VI'), PitchTarget("bVII-")),
        White(TimeSource({11, 27}, 'I+'), PitchTarget("IV")),
        White(TimeSource({3, 11}, 'I'), PitchTarget("bIII")),
        White(TimeSource({3, 11}, 'V'), PitchTarget("bVI")),
        White(TimeSource({3, 11}, 'II'), PitchTarget("bII")),
        White(TimeSource({3, 11}, 'VI'), PitchTarget("bVI")),
        White(TimeSource({3, 11}, 'I+'), PitchTarget("bIII+")),
        White(TimeSource({3, 11}, 'I-'), PitchTarget("bIII-")),
        White(TimeSource(7, 'IV'), PitchTarget("III")),
        White(TimeSource(7, 'I+'), PitchTarget("bVII")),
        White(TimeSource(7, 'V-'), PitchTarget("IV")),
        White(TimeSource(7, 'II'), PitchTarget("IV")),
        White(TimeSource(7, 'VI'), PitchTarget("I+")),
        White(TimeSource(7, 'IV-'), PitchTarget("III-")),
        White(TimeSource(7, 'II-'), PitchTarget("IV-")),
        White(TimeSource(7, 'II+'), PitchTarget("IV+")),
        White(TimeSource(15), PitchTarget("IV")),
        White(TimeSource(5, 'IV'), PitchTarget("IV")),
        White(TimeSource(5, 'I+'), PitchTarget("I+")),
        White(TimeSource(5, 'V'), PitchTarget("V")),
        White(TimeSource(5, 'II'), PitchTarget("II")),
        White(TimeSource(5, 'VI'), PitchTarget("VI")),
        White(TimeSource(5, 'I-'), PitchTarget("I")),
        White(TimeSource(5, 'IV-'), PitchTarget("IV-")),
        White(TimeSource({1, 6, 17, 22}, 'I'), PitchTarget("IV")),
        White(TimeSource({1, 6, 17, 22}, 'V'), PitchTarget("I")),
        White(TimeSource({1, 6, 17, 22}, 'II'), PitchTarget("I+")),
        White(TimeSource({1, 6, 17, 22}, 'VI'), PitchTarget("V")),
        White(TimeSource({1, 6, 17, 22}, 'I-'), PitchTarget("IV-")),
        White(TimeSource({1, 6, 17, 22}, 'I+'), PitchTarget("IV+")),
        White(TimeSource({1, 17}, 'IV'), PitchTarget("bVII-")),
        White(TimeSource({1, 17}, 'I+'), PitchTarget("IV")),
        White(TimeSource({1, 17}, 'V'), PitchTarget("I+")),
        White(TimeSource({1, 17}, 'II'), PitchTarget("V")),
        White(TimeSource({1, 17}, 'VI'), PitchTarget("II")),
        White(TimeSource({1, 17}, 'III'), PitchTarget("VI")),
        White(TimeSource({1, 17}, 'VI'), PitchTarget("III")),
        White(TimeSource({1, 17}, 'bV'), PitchTarget("VII")),
        White(TimeSource({1, 17}, 'VII'), PitchTarget("bV")),
        White(TimeSource({1, 17}, 'IV-'), PitchTarget("bVII-")),
        White(TimeSource({5, 21}), PitchTarget("IV")),
        White(TimeSource({2, 18}), PitchTarget("bV")),
        White(TimeSource(5, 'I'), PitchTarget("bV")),
        White(TimeSource(5, 'V'), PitchTarget("bII")),
        White(TimeSource(5, 'II'), PitchTarget("bVI")),
        White(TimeSource(5, 'VI'), PitchTarget("bIII")),
        White(TimeSource(5, 'I-'), PitchTarget("bV-")),
        White(TimeSource(3), PitchTarget("V")),
        White(TimeSource(28), PitchTarget("V")),
        White(TimeSource(7, 'bV'), PitchTarget("V")),
        White(TimeSource(7, 'bII'), PitchTarget("II")),
        White(TimeSource(7, 'bVI'), PitchTarget("VI")),
        White(TimeSource(7, 'bIII'), PitchTarget("III")),
        White(TimeSource(7, 'bVII'), PitchTarget("VII")),
        White(TimeSource(7, 'IV'), PitchTarget("bV")),
        White(TimeSource(7, 'I'), PitchTarget("bII+")),
        White(TimeSource(7, 'V-'), PitchTarget("bVI")),
        White(TimeSource({4, 9, 20, 25}, 'I'), PitchTarget("V")),
        White(TimeSource({4, 9, 20, 25}, 'V'), PitchTarget("II")),
        White(TimeSource({4, 9, 20, 25}, 'II'), PitchTarget("VI")),
        White(TimeSource({4, 9, 20, 25}, 'VI'), PitchTarget("III")),
        White(TimeSource({4, 9, 20, 25}, 'III'), PitchTarget("VII")),
        White(TimeSource({4, 9, 20, 25}, 'VII'), PitchTarget("bV")),
        White(TimeSource({4, 9, 20, 25}, 'bV'), PitchTarget("bII")),
        White(TimeSource({4, 9, 20, 25}, 'bII'), PitchTarget("bVI")),
        White(TimeSource({4, 9, 20, 25}, 'bVI'), PitchTarget("bIII")),
        White(TimeSource({4, 9, 20, 25}, 'bIII'), PitchTarget("bVII")),
        White(TimeSource({4, 9, 20, 25}, 'bVII'), PitchTarget("IV")),
        White(TimeSource({4, 9, 20, 25}, 'IV'), PitchTarget("I+")),
        White(TimeSource({4, 9, 20, 25}, 'I-'), PitchTarget("V-")),
        White(TimeSource({4, 20}), PitchTarget("I")),
        White(TimeSource({3, 8}), PitchTarget("V")),
        White(TimeSource({19, 24}), PitchTarget("IV")),
        White(TimeSource(21), PitchTarget("bVII")),
        White(TimeSource(23, 'V'), PitchTarget("bVII")),
        White(TimeSource(23, 'II+'), PitchTarget("IV")),
        White(TimeSource(23, 'VI'), PitchTarget("I+")),
        White(TimeSource(23, 'V-'), PitchTarget("bVII-")),
        White(TimeSource(23, 'II'), PitchTarget("bV")),
        White(TimeSource(23, 'VI'), PitchTarget("bII")),
        White(TimeSource(23, 'II+'), PitchTarget("bV")),
        White(TimeSource(23, 'II-'), PitchTarget("bV-")),
        White(TimeSource(11), PitchTarget("bV")),
        White(TimeSource(5, 'bIII+'), PitchTarget("bVII")),
        White(TimeSource(5, 'bIII'), PitchTarget("II")),
        White(TimeSource(5, 'bVII'), PitchTarget("VI")),
        White(TimeSource(5, 'IV'), PitchTarget("I")),
        White(TimeSource(5, 'I'), PitchTarget("V")),
        White(TimeSource(5, 'V'), PitchTarget("II")),
        White(TimeSource(5, 'II'), PitchTarget("VI")),
        White(TimeSource(5, 'VI'), PitchTarget("III")),
        White(TimeSource(5, 'III'), PitchTarget("VII")),
        White(TimeSource(5, 'VII'), PitchTarget("bV")),
        White(TimeSource(5, 'bV'), PitchTarget("bIII")),
        White(TimeSource(5, 'bIII+'), PitchTarget("bVII")),
        White(TimeSource(5, 'bIII-'), PitchTarget("II-")),
        White(TimeSource(19, 'I'), PitchTarget("II")),
        White(TimeSource(19, 'V-'), PitchTarget("VI")),
        White(TimeSource(19, 'II'), PitchTarget("III")),
        White(TimeSource(19, 'bII'), PitchTarget("bIII")),
        White(TimeSource(19, 'III'), PitchTarget("bV")),
        White(TimeSource(19, 'bIII'), PitchTarget("IV")),
        White(TimeSource(19, 'IV'), PitchTarget("V")),
        White(TimeSource(19, 'bV'), PitchTarget("bVI")),
        White(TimeSource(19, 'bVI'), PitchTarget("bVII")),
        White(TimeSource(19, 'VI'), PitchTarget("VII")),
        White(TimeSource(19, 'bVII-'), PitchTarget("I")),
        White(TimeSource(19, 'I-'), PitchTarget("II-")),
        White(TimeSource(19, 'I+'), PitchTarget("II+")),
        White(TimeSource({3, 19}, 'bVII'), PitchTarget("I-")),
        White(TimeSource({3, 19}, 'IV'), PitchTarget("V")),
        White(TimeSource({3, 19}, 'I-'), PitchTarget("II")),
        White(TimeSource({3, 19}, 'VI'), PitchTarget("VII")),
        White(TimeSource({3, 19}, 'VII'), PitchTarget("bII")),
        White(TimeSource({3, 19}, 'bII'), PitchTarget("bIII")),
        White(TimeSource({3, 19}, 'II'), PitchTarget("III")),
        White(TimeSource({3, 19}, 'bIII'), PitchTarget("IV")),
        White(TimeSource({3, 19}, 'III'), PitchTarget("bV")),
        White(TimeSource({3, 19}, 'bV'), PitchTarget("bVI")),
        White(TimeSource({3, 19}, 'V'), PitchTarget("VI")),
        White(TimeSource({3, 19}, 'bVII-'), PitchTarget("I-")),
        White(TimeSource(19, 'V'), PitchTarget("I+")),
        White(TimeSource(19, 'II'), PitchTarget("V")),
        White(TimeSource(19, 'VI'), PitchTarget("II")),
        White(TimeSource(19, 'III'), PitchTarget("VI")),
        White(TimeSource(19, 'VII'), PitchTarget("III")),
        White(TimeSource(19, 'bV'), PitchTarget("VII")),
        White(TimeSource(19, 'bIII'), PitchTarget("bV")),
        White(TimeSource(19, 'IV'), PitchTarget("bVII")),
        White(TimeSource(19, 'I'), PitchTarget("IV")),
        White(TimeSource(19, 'V-'), PitchTarget("I")),
        White(TimeSource(19, 'bVII-'), PitchTarget("I+")),
        White(TimeSource(19, 'IV'), PitchTarget("V")),
        White(TimeSource(1, 'V'), PitchTarget("I+")),
        White(TimeSource(1, 'II'), PitchTarget("V")),
        White(TimeSource(1, 'VI'), PitchTarget("II")),
        White(TimeSource(1, 'III'), PitchTarget("VI")),
        White(TimeSource(1, 'VII'), PitchTarget("III")),
        White(TimeSource(1, 'bV'), PitchTarget("VII")),
        White(TimeSource(1, 'bIII'), PitchTarget("bV")),
        White(TimeSource(1, 'bVII'), PitchTarget("bIII")),
        White(TimeSource(1, 'IV'), PitchTarget("bVII")),
        White(TimeSource(1, 'V-'), PitchTarget("I")),
        White(TimeSource(25, 'IV'), PitchTarget("I+")),
        White(TimeSource(25, 'I+'), PitchTarget("V")),
        White(TimeSource(25, 'V'), PitchTarget("II")),
        White(TimeSource(25, 'II'), PitchTarget("VI")),
        White(TimeSource(25, 'VI'), PitchTarget("III")),
        White(TimeSource(25, 'III'), PitchTarget("VII")),
        White(TimeSource(25, 'VII'), PitchTarget("bV")),
        White(TimeSource(25, 'bV'), PitchTarget("bII")),
        White(TimeSource(25, 'bII'), PitchTarget("bVI")),
        White(TimeSource(25, 'bVI'), PitchTarget("bIII")),
        White(TimeSource(25, 'bVII-'), PitchTarget("IV")),
        White(TimeSource(25, 'bIII'), PitchTarget("I+")),
        White(TimeSource(25, 'bVII'), PitchTarget("V")),
        White(TimeSource(25, 'IV-'), PitchTarget("I")),
        White(TimeSource(25, 'bIII-'), PitchTarget("I")),
        White(TimeSource(17), PitchTarget("II+")),
        White(TimeSource(27), PitchTarget("II+")),
        White(TimeSource({9, 25}), PitchTarget("bIII+")),
        White(TimeSource({3, 29}, 'I'), PitchTarget("bVII-")),
        White(TimeSource({3, 29}, 'V'), PitchTarget("IV")),
        White(TimeSource({3, 29}, 'II+'), PitchTarget("I")),
        White(TimeSource({3, 29}, 'VI'), PitchTarget("V")),
        White(TimeSource({3, 29}, 'bIII'), PitchTarget("bII")),
        White(TimeSource({3, 29}, 'bII'), PitchTarget("VII")),
        White(TimeSource({3, 29}, 'III'), PitchTarget("II")),
        White(TimeSource({3, 29}, 'IV'), PitchTarget("bIII")),
        White(TimeSource({3, 29}, 'bV'), PitchTarget("III")),
        White(TimeSource({3, 29}, 'bVI'), PitchTarget("bV")),
        White(TimeSource({3, 29}, 'bVII'), PitchTarget("bVI")),
        White(TimeSource({3, 29}, 'VII'), PitchTarget("VI")),
        White(TimeSource({3, 29}, 'I+'), PitchTarget("bVII")),
        White(TimeSource(29, 'I'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'V'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'II'), PitchTarget('V')),
        White(TimeSource(29, 'VI'), PitchTarget('II')),
        White(TimeSource(29, 'III'), PitchTarget('VI')),
        White(TimeSource(29, 'VII'), PitchTarget('III')),
        White(TimeSource(29, 'bV'), PitchTarget('VII')),
        White(TimeSource(29, 'bVII'), PitchTarget('bV')),
        White(TimeSource(29, 'IV'), PitchTarget('bVII')),
        White(TimeSource(27, 'IV'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'I+'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'V'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'II'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'VI'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'III'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'VII'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'bV'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'bII'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'bVII'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'I+'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'I+'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'II'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(27, 'VI'), PitchTarget(['bIII', 'I'])),
        White(TimeSource(13, 'I'), PitchTarget(['I', 'IV'])),
        White(TimeSource(13, 'V'), PitchTarget(['I', 'IV'])),
        White(TimeSource(13, 'II'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'VI'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'III'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'VII'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'bV'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'bII'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'bVI'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'bIII'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'bVII'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(13, 'IV'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(29, 'I+'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'V'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'II'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'VI'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'III'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'VII'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'bV'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'bII'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'bVII'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'IV'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'I'), PitchTarget(['IV', 'I', 'bIII'])),
        White(TimeSource(29, 'II+'), PitchTarget('V')),
        White(TimeSource(27), PitchTarget(['bIII', 'I'])),
        White(TimeSource(13, 'I+'), PitchTarget(['I', 'IV'])),
        White(TimeSource(13, 'II+'), PitchTarget(['III', 'III', 'bVII'])),
        White(TimeSource(23), PitchTarget(['bIII', 'IV'])),
    ], [
        Black(PitchSource('I', 1), TimeTarget(1)),
        Black(PitchSource('I', 9), TimeTarget(9)),
        Black(PitchSource('I', 5), TimeTarget({2, 18})),
        Black(PitchSource('I', 13), TimeTarget({3, 19})),
        Black(PitchSource('VI'), TimeTarget(7)),
        Black(PitchSource('II', 1), TimeTarget(11)),
        Black(PitchSource('I', 11), TimeTarget(29)),
        Black(PitchSource('I', 17), TimeTarget(11)),
        Black(PitchSource('I', 19), TimeTarget(27)),
        Black(PitchSource('I', 21), TimeTarget({15, 31})),
        Black(PitchSource('I', 27), TimeTarget(25)),
        Black(PitchSource('I', 29), TimeTarget({19, 23, 25})),
        Black(PitchSource('I+', {3, 19}), TimeTarget(21)),
        Black(PitchSource('bII', {3, 19}), TimeTarget(21)),
        Black(PitchSource('bIII', {3, 19}), TimeTarget(21)),
        Black(PitchSource('III', {3, 19}), TimeTarget(21)),
        Black(PitchSource('IV', {3, 19}), TimeTarget(21)),
        Black(PitchSource('bV', {3, 19}), TimeTarget(21)),
        Black(PitchSource('bVI', {3, 19}), TimeTarget(21)),
        Black(PitchSource('VI', {3, 19}), TimeTarget(21)),
        Black(PitchSource('bVII', {3, 19}), TimeTarget(21)),
        Black(PitchSource('VII', {3, 19}), TimeTarget(21)),
        Black(PitchSource('I+', {1, 17}), TimeTarget(19)),
        Black(PitchSource('bII', {1, 17}), TimeTarget(19)),
        Black(PitchSource('II', {1, 17}), TimeTarget(19)),
        Black(PitchSource('bIII', {1, 17}), TimeTarget(19)),
        Black(PitchSource('III', {1, 17}), TimeTarget(19)),
        Black(PitchSource('IV-', {1, 17}), TimeTarget(19)),
        Black(PitchSource('bV', {1, 17}), TimeTarget(19)),
        Black(PitchSource('V', {1, 17}), TimeTarget(19)),
        Black(PitchSource('bVI', {1, 17}), TimeTarget(19)),
        Black(PitchSource('VI', {1, 17}), TimeTarget(19)),
        Black(PitchSource('bVII', {1, 17}), TimeTarget(19)),
        Black(PitchSource('VII', {1, 17}), TimeTarget(19)),
        Black(PitchSource('I-', {1, 17}), TimeTarget(19)),
        Black(PitchSource('I', {1, 17}), TimeTarget(19)),
        Black(PitchSource('I-', {5, 21}), TimeTarget(23)),
        Black(PitchSource('I', {5, 21}), TimeTarget(23)),
        Black(PitchSource('I+', {5, 21}), TimeTarget(23)),
        Black(PitchSource('I', {7, 23}), TimeTarget(25)),
        Black(PitchSource('I-', {7, 23}), TimeTarget(25)),
        Black(PitchSource('I+', {7, 23}), TimeTarget(25)),
        Black(PitchSource('I-', {7, 23}), TimeTarget(25)),
        Black(PitchSource('I+', {7, 23}), TimeTarget(25)),
        Black(PitchSource('I', {9, 25}), TimeTarget(27)),
        Black(PitchSource('I-', {9, 25}), TimeTarget(27)),
        Black(PitchSource('I-', {11, 27}), TimeTarget(29)),
        Black(PitchSource('I+', {11, 27}), TimeTarget(29)),
        Black(PitchSource('I', {13, 29}), TimeTarget(31)),
        Black(PitchSource('I', {11, 27}), TimeTarget(31)),
        Black(PitchSource('I+', {4, 20}), TimeTarget(1)),
        Black(PitchSource('I+', {9, 25}), TimeTarget(3)),
        Black(PitchSource('I-', {13, 29}), TimeTarget(19)),
        Black(PitchSource('I+', 13), TimeTarget(25)),
        Black(PitchSource('I-'), TimeTarget({3, 19})),
        Black(PitchSource('II', 3), TimeTarget({3, 19})),
        Black(PitchSource('II', 7), TimeTarget({1, 17})),
        Black(PitchSource('II+', 13), TimeTarget({19, 27})),
        Black(PitchSource('II+', {11, 27}), TimeTarget(15)),
        Black(PitchSource('II', {11, 27}), TimeTarget(15)),
        Black(PitchSource('II-', {13, 29}), TimeTarget(17)),
        Black(PitchSource('II', 23), TimeTarget(23)),
        Black(PitchSource('II', 29), TimeTarget({8, 24})),
        Black(PitchSource('II', 27), TimeTarget({11, 27})),
        Black(PitchSource('II+', 23), TimeTarget({5, 25})),
        Black(PitchSource('II+', 1), TimeTarget(17)),
        Black(PitchSource('III', 5), TimeTarget(7)),
        Black(PitchSource('II', 13), TimeTarget({5, 21, 31})),
        Black(PitchSource('IV', 9), TimeTarget(15)),
        Black(PitchSource('IV', {1, 17}), TimeTarget(5)),
        Black(PitchSource('IV', {6, 22}), TimeTarget(23)),
        Black(PitchSource('IV', 5), TimeTarget({1, 6, 17, 22})),
        Black(PitchSource('IV', 7), TimeTarget({5, 21})),
        Black(PitchSource('IV', 25), TimeTarget({9, 25})),
        Black(PitchSource('VI', {3, 8, 19, 24}), TimeTarget(7)),
        Black(PitchSource('V', 1), TimeTarget({4, 9, 20, 25})),
        Black(PitchSource('V', 19), TimeTarget({4, 20})),
        Black(PitchSource('V', {19, 24}), TimeTarget(19)),
        Black(PitchSource('V', 23), TimeTarget(3)),
        Black(PitchSource('V', {3, 8}), TimeTarget(11)),
        Black(PitchSource('V', 7), TimeTarget({3, 8, 19, 24})),
        Black(PitchSource('bVII-'), TimeTarget({13, 29})),
        Black(PitchSource('III', {21, 23}), TimeTarget(5)),
        Black(PitchSource('bIII', 23), TimeTarget({3, 17, 19})),
        Black(PitchSource('bIII', 25), TimeTarget({13, 29})),
        Black(PitchSource('bIII', 17), TimeTarget({11, 27})),
        Black(PitchSource('bIII', 15), TimeTarget({9, 15, 25})),
        Black(PitchSource('bIII', 28), TimeTarget({3, 11})),
        Black(PitchSource('bIII', 5), TimeTarget(13)),
        Black(PitchSource('bIII+', 1), TimeTarget({9, 25})),
        Black(PitchSource('bIII+', 17), TimeTarget({11, 27})),
        Black(PitchSource('bV', {2, 18}), TimeTarget(15)),
        Black(PitchSource('bV', {7, 23}), TimeTarget(7)),
        Black(PitchSource('bV'), TimeTarget({2, 7, 18, 23})),
        Black(PitchSource('bVI'), TimeTarget({6, 22})),
        Black(PitchSource('bVII-', {15, 31}), TimeTarget(21)),
        Black(PitchSource('bVII-', {13, 29}), TimeTarget(19)),
        Black(PitchSource('bVII-', {11, 27}), TimeTarget(17)),
        Black(PitchSource('bVII-', {9, 25}), TimeTarget(15)),
        Black(PitchSource('bVII-', {7, 23}), TimeTarget(13)),
        Black(PitchSource('bVII-', {5, 21}), TimeTarget(11)),
        Black(PitchSource('bVII-', {3, 19}), TimeTarget(9)),
        Black(PitchSource('bVII-', {1, 7}), TimeTarget(7)),
        Black(PitchSource('bVII', {9, 25}), TimeTarget(5)),
        Black(PitchSource('bVII'), TimeTarget(23)),
        Black(PitchSource(['I', 'IV'], {19, 27}), TimeTarget(13)),
        Black(PitchSource(['I', 'I+'], {21, 23}), TimeTarget({1, 17})),
        Black(PitchSource('III+', 1), TimeTarget({3, 19})),
        Black(PitchSource('III+', 3), TimeTarget({7, 23})),
        Black(PitchSource('III+', 5), TimeTarget({5, 21})),
        Black(PitchSource('III+', 7), TimeTarget({9, 15, 25, 31})),
        Black(PitchSource(['IV', 'I', 'bIII'], {19, 23, 25}), TimeTarget(29)),
        Black(PitchSource(['bIII'], {23, 25}), TimeTarget(29)),
        Black(PitchSource(['bIII'], {23, 25}), TimeTarget(29)),
        Black(PitchSource(['bIII'], {21, 23}), TimeTarget(27)),
        Black(PitchSource(['bIII'], {19, 21}), TimeTarget(25)),
        Black(PitchSource(['bIII'], {17, 19}), TimeTarget(23)),
        Black(PitchSource(['bIII'], {15, 17}), TimeTarget(21)),
        Black(PitchSource(['bIII'], {13, 15}), TimeTarget(19)),
        Black(PitchSource(['bIII'], {11, 13}), TimeTarget(17)),
        Black(PitchSource(['bIII'], {9, 11}), TimeTarget(15)),
        Black(PitchSource(['bIII'], {7, 9}), TimeTarget(13)),
        Black(PitchSource(['bIII'], {5, 7}), TimeTarget(11)),
        Black(PitchSource(['bIII'], {3, 5}), TimeTarget(9)),
        Black(PitchSource(['bIII'], {1, 3}), TimeTarget(7)),
        Black(PitchSource(['bIII', 'I'], {29, 31}), TimeTarget(27)),
        Black(PitchSource(['bIII', 'V'], {21, 23}), TimeTarget(29)),
        Black(PitchSource(['bIII', 'IV'], {5, 25}), TimeTarget(23)),])


def readinnotes(midiobj, secsperthirty):
    inNotes, inoffset = findoffset(midiobj)
    for instrument in midiobj.instruments:
        [getnotestart(inNotes, inoffset, note, secsperthirty) for note in instrument.notes]
    [note.findend(inNotes) for note in inNotes]
    return inoffset, inNotes


def getnotestart(inNotes, inoffset, note, secsperthirty):
    offsetPitch = note.pitch - inoffset
    if offsetPitch >= highest:
        offsetPitch -= 12 * (offsetPitch - highest) // 12
    inNotes.append(Note(secsfromthirty(note.start, secsperthirty), positions[offsetPitch], secsperthirty))


def findoffset(midiobj):
    pitches = set()
    [pitches.update({note.pitch for note in instrument.notes}) for instrument in midiobj.instruments]
    inmin = min(pitches)
    if tonic in pitches:
        inoffset = 12 * ((inmin - tonic) // 12)
    else:
        inoffset = min([abs(tonic - pitch) for pitch in pitches])
    return [], inoffset


def getnoteset(result):
    return {(note.pitch, note.thirtytwo) for note in result}


def fromFile(infilename="in.mid", outfilename='out.mid', drawGraph=False, outinstrument='Acoustic Grand Piano',
             debug=False):
    collect = collection(infilename)
    inoffset, inNotes = readinnotes(pretty_midi.PrettyMIDI(infilename), collect.secsperthirty)
    resu, inhil, outhil, inremain, outremain = collect.go(inNotes, drawGraph)
    if debug:
        for p, nl in inhil:
            print(p, '\t', ', '.join([str(nte) for nte in nl]))
        print("IN REMAINS")
        print([str(remain) for remain in inremain])
        print("------------------------------------------------------------")
        for plt, tm, nl in outhil:
            print(plt, '\t', tm, '\t', '[', ', '.join([str(nte) for nte in nl]), ']')
        print("OUT REMAINS")
        print([str(remain) for remain in outremain])
        print("------------------------------------------------------------")
    if len(resu) > 0:
        out_midi = pretty_midi.PrettyMIDI(initial_tempo=collect.raw_tempo)
        outinst = pretty_midi.Instrument(program=pretty_midi.instrument_name_to_program(outinstrument))
        if min([note.notenumber + inoffset for note in resu]) < tonic:
            inoffset += 12
        for note in resu:
            outNote = pretty_midi.Note(velocity=100, pitch=note.notenumber + inoffset, start=note.start, end=note.end)
            outinst.notes.append(outNote)
        out_midi.instruments.append(outinst)
        out_midi.write(outfilename)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='transform riff')
    parser.add_argument('inputfilename', metavar='in', type=str,
                        default='in.mid', action='store',
                        help='midi input file')
    parser.add_argument('outputfilename', metavar='out', type=str,
                        default='out.mid', action='store',
                        help='midi output file')
    parser.add_argument('--graph', const=True, default=False, action='store_const',
                        help='draw a graphviz graph of rules applied')
    args = vars(parser.parse_args())
    fromFile(args['inputfilename'], args['outputfilename'], args['graph'])
    #fromFile('test1.mid', 'out.mid', debug=True)