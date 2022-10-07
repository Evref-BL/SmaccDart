Class {
	#name : #DartCatchPartNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartCatchPartNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitCatchPart: self
]

{ #category : #generated }
DartCatchPartNode >> compositeNodeVariables [

	^ #( #identifiers )
]

{ #category : #generated }
DartCatchPartNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartCatchPartNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartCatchPartNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
]
