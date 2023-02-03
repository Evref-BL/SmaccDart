Extension { #name : #String }

{ #category : #'*PreprocessingDart' }
String >> asMyRegex [

	"Compile the receiver as a regex matcher. May raise RxParser>>syntaxErrorSignal
	or RxParser>>compilationErrorSignal.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^ MyRxParser preferredMatcherClass for: (MyRxParser new parse: self)

]
