// Read inputs from System.in, Write outputs to System.out.

def occurences(words) {
    words.split('').groupBy()
}

def isValid(word, letters) {
    wordOccurences = occurences(word)
    letterOccurences = occurences(letters)
    wordOccurences.dropWhile {
        letterOccurences.(it.key) != null &&
        it.value.size <= letterOccurences.(it.key).size
    }.isEmpty()
}

def weight(word) {
    wordOccurences = occurences(word)
    wordOccurences.inject(0) { acc, occurence ->
            acc + points[occurence.key] * occurence.value.size
        }
}

points = ['': 0,
          'e': 1, 'a': 1, 'i': 1, 'o': 1, 'n': 1, 'r': 1, 't': 1, 'l': 1, 's': 1, 'u': 1,
          'd': 2, 'g': 2,
          'b': 3, 'c': 3, 'm': 3, 'p': 3,
          'f': 4, 'h': 4, 'v': 4, 'w': 4, 'y': 4,
          'k': 5,
          'j': 6, 'x': 8,
          'q': 10, 'z': 10]

input = new Scanner(System.in)
n = input.nextInt()

words = (1..n).collect {
            input.next()
        }

letters = input.next()

println words.grep { isValid(it, letters) }
             .sort { o1, o2 -> weight(o2) <=> weight(o1) }
             .first()

