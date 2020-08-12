import unittest, pretty_midi
from rififi import readinnotes, collection, findtempo, getnoteset


class TestCase01(unittest.TestCase):
    def setUp(self):
        pass

    def check_01(self):
        collect = collection('test3.mid')
        inoffset, inNotes = readinnotes(pretty_midi.PrettyMIDI('test3.mid'), collect.secsperthirty)
        resu, inhil, outhil, inremain, outremain = collect.go(inNotes)
        assert getnoteset(resu) == {('I', 11), ('V', 24), ('I+', 20), ('bV', 18), ('bV', 7), ('V', 3), ('IV', 1), ('IV', 6), ('I+', 25), ('V', 8), ('I+', 4), ('bV', 2), ('IV', 17), ('V', 19), ('bV', 23), ('IV', 22), ('I+', 9)}

    def check_02(self):
        collect = collection('test1.mid')
        inoffset, inNotes = readinnotes(pretty_midi.PrettyMIDI('test1.mid'), collect.secsperthirty)
        resu, inhil, outhil, inremain, outremain = collect.go(inNotes)
        assert getnoteset(resu) == {('bIII', 25), ('IV', 19), ('bV', 15), ('I', 3), ('V', 11), ('I', 29), ('I', 23), ('I', 1), ('bVII-', 5), ('I', 7)}

    def check_03(self):
        collect = collection('test4.mid')
        inoffset, inNotes = readinnotes(pretty_midi.PrettyMIDI('test4.mid'), collect.secsperthirty)
        resu, inhil, outhil, inremain, outremain = collect.go(inNotes)
        assert getnoteset(resu) == {('I', 1), ('II', 27), ('bIII', 23), ('bIII', 15), ('I', 5), ('II', 13), ('I', 11), ('I', 29), ('bIII', 25)}

    def check_04(self):
        collect = collection('test5.mid')
        inoffset, inNotes = readinnotes(pretty_midi.PrettyMIDI('test5.mid'), collect.secsperthirty)
        resu, inhil, outhil, inremain, outremain = collect.go(inNotes)
        assert getnoteset(resu) == {('bVII', 15), ('bVII', 25), ('V', 23), ('III', 5), ('II+', 27), ('I', 3), ('I', 1), ('bVII', 31), ('bVII', 9), ('V', 7), ('III', 21), ('II+', 11), ('I', 19), ('I', 17)}


def suite():
    suite1 = unittest.makeSuite(TestCase01, 'check')
    alltests = unittest.TestSuite((suite1,),)
    return alltests


def main():
    runner = unittest.TextTestRunner(descriptions=0, verbosity=2)
    runner.run(suite())


if __name__ == '__main__':
    main()