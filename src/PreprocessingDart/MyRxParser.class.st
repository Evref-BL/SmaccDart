Class {
	#name : #MyRxParser,
	#superclass : #RxParser,
	#category : #PreprocessingDart
}

{ #category : #preferences }
MyRxParser class >> preferredMatcherClass [

	^ MyRxMatcher
]
