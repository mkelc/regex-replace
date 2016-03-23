regex-replace - Replace functions in Haskell regular expressions
================================================================

Looking around for a function usable to do substituions on strings
using regular expressions I finally found the following question on 
Stackoverflow:

http://stackoverflow.com/questions/3847475/haskell-regex-substitution

Here Chris Kuklewicz states that there will not be a generic substituion
function - at least because it may not be possible to do it in a performance
sensible way.

Nevertheless I need a substitution function for myself and therefore 
started to implement one. During this I found that a performance sensitive
way of implementing a generic substitution function may be possible when using
the Monoid-Typeclass to have an append function.

Therefore: here it is a very small Substitution function for regular expressions
based on Haskells great Text.Regexp.Base API.

I've tested it with the POSIX and the TDFA implementation of the API.

