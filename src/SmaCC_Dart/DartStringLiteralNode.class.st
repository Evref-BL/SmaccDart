Class {
	#name : #DartStringLiteralNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'multiLineStrings',
		'singleLineStrings'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartStringLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitStringLiteral: self
]

{ #category : #generated }
DartStringLiteralNode >> compositeNodeVariables [

	^ #( #multiLineStrings #singleLineStrings )
]

{ #category : #'generated-initialize-release' }
DartStringLiteralNode >> initialize [
	super initialize.
	multiLineStrings := OrderedCollection new: 2.
	singleLineStrings := OrderedCollection new: 2.
]

{ #category : #generated }
DartStringLiteralNode >> multiLineStrings [

	^ multiLineStrings
]

{ #category : #generated }
DartStringLiteralNode >> multiLineStrings: anOrderedCollection [

	self setParents: self multiLineStrings to: nil.
	multiLineStrings := anOrderedCollection.
	self setParents: self multiLineStrings to: self
]

{ #category : #generated }
DartStringLiteralNode >> singleLineStrings [

	^ singleLineStrings
]

{ #category : #generated }
DartStringLiteralNode >> singleLineStrings: anOrderedCollection [

	self setParents: self singleLineStrings to: nil.
	singleLineStrings := anOrderedCollection.
	self setParents: self singleLineStrings to: self
]
