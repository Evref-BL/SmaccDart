Class {
	#name : #MyRxMatcher,
	#superclass : #RxMatcher,
	#category : #PreprocessingDart
}

{ #category : #copying }
MyRxMatcher >> copyStream: aStream to: writeStream replacingMatchesWith: aString [
    "Copy the contents of <aStream> on the <writeStream>,
     except for the matches. Replace each match with <aString>."

    | searchStart matchStart matchEnd |
    stream := aStream.
    markerPositions := nil.
    [searchStart := aStream position.
    self proceedSearchingStream: aStream] whileTrue: [ | ws rep |
        matchStart := (self subBeginning: 1) first.
        matchEnd := (self subEnd: 1) first.
        aStream position: searchStart.
        searchStart to: matchStart - 1 do:
            [:ignoredPos | writeStream nextPut: aStream next].

        "------- The following lines replaced: writeStream nextPutAll: aString ------"
        "Do the regex replacement including lookback substitutions"
        writeStream nextPutAll: (aString format: self subexpressionStrings).
        "-------"

        aStream position: matchEnd.
        "Be extra careful about successful matches which consume no input.
        After those, make sure to advance or finish if already at end."
        matchEnd = searchStart ifTrue: 
            [aStream atEnd
                ifTrue: [^self "rest after end of whileTrue: block is a no-op if atEnd"]
                ifFalse:    [writeStream nextPut: aStream next]]].
    aStream position: searchStart.
    [aStream atEnd] whileFalse: [writeStream nextPut: aStream next]
]

{ #category : #'as yet unclassified' }
MyRxMatcher >> subexpressionStrings [
   "Create an array of lookback strings"
   | ws |
   ws := Array new writeStream.
   2 to: (self subexpressionCount) do: [ :n | | se |
      ws nextPut: ((se := self subexpression: n) ifNil: [ '' ] ifNotNil: [ se ]) ].
   ^ws contents.
]
