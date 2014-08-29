package dojo

class ScrabbleTest extends GroovyTestCase {

    void testLetterPoints() {
        assert Scrabble.letterPoints('z') == 10 : "We're in trouble"
        assert Scrabble.letterPoints('x') == 8
        assert Scrabble.letterPoints('u') == 1
    }

    void testWordScore() {
        assert Scrabble.wordScore("abc") == 7
    }

    void testLetterOccurences() {
        assert Scrabble.letterOccurences("abbccc") == ['a': 1, 'b': 2, 'c': 3]
    }

    void testValidWord() {
        assert Scrabble.isValidWord("because", "hicquwh") == false
        assert Scrabble.isValidWord("which", "hicquwh") == true
    }

    void testHighestScoreWord() {
        assert Scrabble.highestScoreWord(["repots", "powers"]) == "powers"
    }

    void testBestWord() {
        assert Scrabble.bestWord(["because", "first", "these", "could", "which"], "hicquwh") == "which"
    }

}


