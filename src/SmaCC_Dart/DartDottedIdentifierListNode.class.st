Class {
	#name : #DartDottedIdentifierListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartDottedIdentifierListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitDottedIdentifierList: self
]

{ #category : #generated }
DartDottedIdentifierListNode >> compositeNodeVariables [

	^ #( #identifiers )
]

{ #category : #generated }
DartDottedIdentifierListNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartDottedIdentifierListNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartDottedIdentifierListNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
]
