Class {
	#name : #DartInitializedIdentifierListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'initializedIdentifier',
		'nextInitializedIdentifiers'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartInitializedIdentifierListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitInitializedIdentifierList: self
]

{ #category : #generated }
DartInitializedIdentifierListNode >> compositeNodeVariables [

	^ #( #nextInitializedIdentifiers )
]

{ #category : #'generated-initialize-release' }
DartInitializedIdentifierListNode >> initialize [
	super initialize.
	nextInitializedIdentifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartInitializedIdentifierListNode >> initializedIdentifier [

	^ initializedIdentifier
]

{ #category : #generated }
DartInitializedIdentifierListNode >> initializedIdentifier: aDartInitializedIdentifierNode [

	self initializedIdentifier notNil ifTrue: [ 
		self initializedIdentifier parent: nil ].
	initializedIdentifier := aDartInitializedIdentifierNode.
	self initializedIdentifier notNil ifTrue: [ 
		self initializedIdentifier parent: self ]
]

{ #category : #generated }
DartInitializedIdentifierListNode >> nextInitializedIdentifiers [

	^ nextInitializedIdentifiers
]

{ #category : #generated }
DartInitializedIdentifierListNode >> nextInitializedIdentifiers: anOrderedCollection [

	self setParents: self nextInitializedIdentifiers to: nil.
	nextInitializedIdentifiers := anOrderedCollection.
	self setParents: self nextInitializedIdentifiers to: self
]

{ #category : #generated }
DartInitializedIdentifierListNode >> nodeVariables [

	^ #( #initializedIdentifier )
]
