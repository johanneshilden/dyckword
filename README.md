# dyckword [![Build Status](https://img.shields.io/travis/johanneshilden/dyckword/master.svg?style=flat)](https://travis-ci.org/johanneshilden/dyckword)

In formal language theory, the *Dyck language* consists of all strings of evenly balanced left and right parentheses, brackets, or some other symbols, together with the *empty* word. Words in this language (named after German mathematician Walther von Dyck) are known as *Dyck words*, some examples of which are `()()()`, `(())((()))`, and `((()()))()`.

The type of Dyck language considered here is defined over a binary alphabet. We will take this alphabet to be the set Σ = {(, )} in the following examples. The binary Dyck language is the subset of Σ* (the Kleene closure of Σ) of all words that satisfy two conditions:

1. The number of left brackets must be the same as the number of right brackets.
2. Going from left to right, for each character read, the total number of right brackets visited must be less than or equal to the number of left brackets up to the current position.

E.g., `(()(()` and `())(())()` are **not** Dyck words.

When regarded as a combinatorial class &ndash; with the size of a word defined as the number of bracket pairs it contains &ndash; the counting sequence associated with the Dyck language is the *Catalan numbers*.
