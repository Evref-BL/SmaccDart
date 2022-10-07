Class {
	#name : #DartIdentifierListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'nextIdentifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartIdentifierListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitIdentifierList: self
]

{ #category : #generated }
DartIdentifierListNode >> compositeNodeVariables [

	^ #( #nextIdentifiers )
]

{ #category : #generated }
DartIdentifierListNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartIdentifierListNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #'generated-initialize-release' }
DartIdentifierListNode >> initialize [
	super initialize.
	nextIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartIdentifierListNode >> nextIdentifiers [

	^ nextIdentifiers
]

{ #category : #generated }
DartIdentifierListNode >> nextIdentifiers: anOrderedCollection [

	self setParents: self nextIdentifiers to: nil.
	nextIdentifiers := anOrderedCollection.
	self setParents: self nextIdentifiers to: self
]

{ #category : #generated }
DartIdentifierListNode >> nodeVariables [

	^ #( #identifier )
]
