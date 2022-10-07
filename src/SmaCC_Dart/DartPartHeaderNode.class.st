Class {
	#name : #DartPartHeaderNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifiers',
		'stringLiteralWithoutInterpolation'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartPartHeaderNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitPartHeader: self
]

{ #category : #generated }
DartPartHeaderNode >> compositeNodeVariables [

	^ #( #identifiers )
]

{ #category : #generated }
DartPartHeaderNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartPartHeaderNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartPartHeaderNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartPartHeaderNode >> stringLiteralWithoutInterpolation [

	^ stringLiteralWithoutInterpolation
]

{ #category : #generated }
DartPartHeaderNode >> stringLiteralWithoutInterpolation: aSmaCCToken [

	stringLiteralWithoutInterpolation := aSmaCCToken
]

{ #category : #generated }
DartPartHeaderNode >> tokenVariables [

	^ #( #stringLiteralWithoutInterpolation )
]
