Class {
	#name : #DartConfigurableUriNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'uris',
		'ifTokens',
		'leftParenTokens',
		'uriTests',
		'rightParenTokens'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConfigurableUriNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConfigurableUri: self
]

{ #category : #generated }
DartConfigurableUriNode >> compositeNodeVariables [

	^ #( #uris #uriTests )
]

{ #category : #generated }
DartConfigurableUriNode >> compositeTokenVariables [

	^ #( #ifTokens #leftParenTokens #rightParenTokens )
]

{ #category : #generated }
DartConfigurableUriNode >> ifTokens [

	^ ifTokens
]

{ #category : #generated }
DartConfigurableUriNode >> ifTokens: anOrderedCollection [

	ifTokens := anOrderedCollection
]

{ #category : #'generated-initialize-release' }
DartConfigurableUriNode >> initialize [
	super initialize.
	uris := OrderedCollection new: 2.
	ifTokens := OrderedCollection new: 2.
	leftParenTokens := OrderedCollection new: 2.
	uriTests := OrderedCollection new: 2.
	rightParenTokens := OrderedCollection new: 2.
]

{ #category : #generated }
DartConfigurableUriNode >> leftParenTokens [

	^ leftParenTokens
]

{ #category : #generated }
DartConfigurableUriNode >> leftParenTokens: anOrderedCollection [

	leftParenTokens := anOrderedCollection
]

{ #category : #generated }
DartConfigurableUriNode >> rightParenTokens [

	^ rightParenTokens
]

{ #category : #generated }
DartConfigurableUriNode >> rightParenTokens: anOrderedCollection [

	rightParenTokens := anOrderedCollection
]

{ #category : #generated }
DartConfigurableUriNode >> uriTests [

	^ uriTests
]

{ #category : #generated }
DartConfigurableUriNode >> uriTests: anOrderedCollection [

	self setParents: self uriTests to: nil.
	uriTests := anOrderedCollection.
	self setParents: self uriTests to: self
]

{ #category : #generated }
DartConfigurableUriNode >> uris [

	^ uris
]

{ #category : #generated }
DartConfigurableUriNode >> uris: anOrderedCollection [

	self setParents: self uris to: nil.
	uris := anOrderedCollection.
	self setParents: self uris to: self
]
