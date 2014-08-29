package dojo

class Scrabble {

    static letterPoints(letter) {
        ['z': 10, 'q': 10,
         'x': 8, 'j': 8,
         'k': 5,
         'f': 4, 'h': 4, 'v': 4, 'w' :4, 'y': 4,
         'b': 3, 'c': 3, 'm': 3, 'p': 3,
         'd': 2, 'g': 2].get(letter, 1)
    }

    static letterOccurences(word) {
        word.split('')
            .grep { it != ''}
            .groupBy()
            .collectEntries { k, v -> [(k): v.size()] }
    }

    static isValidWord(word, letters) {
        letterOccurences(word).dropWhile { k, v -> letterOccurences(letters).get(k, 0) >= v  }
                              .isEmpty()
    }

    static validWords(words, letters) {
        words.grep { isValidWord(it, letters) }
    }

    static wordScore(word) {
        letterOccurences(word).collect { k, v -> letterPoints(k) * v }
                              .sum()
    }

    static highestScoreWord(words) {
        words.max { w1, w2 -> wordScore(w1) <=> wordScore(w2) }
    }

    static bestWord(words, letters) {
        highestScoreWord(validWords(words, letters))
    }

}
