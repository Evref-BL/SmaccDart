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

	self setParent: self multiLineStrings to: nil.
	multiLineStrings := anOrderedCollection.
	self setParent: self multiLineStrings to: self
]

{ #category : #generated }
DartStringLiteralNode >> otherVariables [

	^ #( #multiLineStrings #singleLineStrings )
]

{ #category : #generated }
DartStringLiteralNode >> singleLineStrings [

	^ singleLineStrings
]

{ #category : #generated }
DartStringLiteralNode >> singleLineStrings: anOrderedCollection [

	self setParent: self singleLineStrings to: nil.
	singleLineStrings := anOrderedCollection.
	self setParent: self singleLineStrings to: self
]
